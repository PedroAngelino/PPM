import GameDomain.*
import GameUtils.*
import GameEngine.*
import GameEngine.given
import GameState.*
import GameLoop.*
import TUI.*

import scala.annotation.tailrec

object AIPlayer {

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    val (index, newRand) = rand.nextInt(lstOpenCoords.length) //de uma lista de posições possiveis vai escolher uma aleatoriamente
    (lstOpenCoords(index), newRand)
    
  def playRandomly(
                    board: Board,
                    r: MyRandom,
                    player: Stone,
                    lstOpenCoords: List[Coord2D],
                    f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    val playerPieces = board.toList.collect { //pegar as coordenadas das pecas do jogador
      case (coord, p) if p == player => coord
    }

    //para as coordenadas livres, ver quais pecas tem moves validos para la
    val validDestinationsMap: Map[Coord2D, List[Coord2D]] = lstOpenCoords.map { open =>
      val validOrigins = playerPieces.filter { from =>
        validDestinations(board, player, from, lstOpenCoords).contains(open)
      }
      open -> validOrigins
    }.filter { case (_, origins) => origins.nonEmpty }.toMap // Manter apenas as que têm jogadas

    if (validDestinationsMap.isEmpty) { //se nao houver jogadas possiveis retorna sem alteracoes
      (None, r, lstOpenCoords, None)
    } else {
      // escolher um dos destinos livres válidos
      val availableDestinations = validDestinationsMap.keys.toList
      val (selectedTo, r2) = f(availableDestinations, r)

      // Escolher uma das peças (origem) que conseguem saltar para o destino selecionado
      val possibleOrigins = validDestinationsMap(selectedTo)
      val (selectedFrom, r3) = f(possibleOrigins, r2)

      // Aplicar a jogada
      val (newBoardOpt, newOpenCoords) = GameEngine.play(board, player, selectedFrom, selectedTo, lstOpenCoords)

      // Devolver o novo tabuleiro
      (newBoardOpt, r3, newOpenCoords, Some(selectedTo))
    }
  }


  def doComputerMove(): Unit =
    Thread.sleep(600) // Pequena pausa para observar cada salto do PC
    val multiplecaptures: Boolean = GameState.cfgDiff == 2

    //no modo dificil o bot continua a capturar pecas ate nao houver moves possiveis
    //(apartir da peca que fez o primeiro move da jogada)
    @tailrec
    def continuePcCaptures(from: Coord2D): Unit =
    //ver se ha mais moves possiveis
      val nextMoves = GameEngine.validDestinations(GameState.board, Stone.Black, from, GameState.open).flatMap { to =>
        GameEngine.play(GameState.board, Stone.Black, from, to, GameState.open) match
          case (Some(nb), newOpen) => Some((to, nb, newOpen))
          case _ => None
      }
      if nextMoves.nonEmpty then
        val (to, nb, newOpen) = nextMoves.head
        //aplica a jogada
        GameState.board = nb;
        GameState.open = newOpen
        if shouldPrint then
          println(s"  PC continuou (${from._1},${from._2}) -> (${to._1},${to._2})")
        GameState.onStateChanged()
        Thread.sleep(600) // Pequena pausa para observar cada salto do PC
        continuePcCaptures(to) //saltar para a prox posicao e voltar a rodar

    //para diff 1, a jogada vai ser totalmente aleatoria
    if GameState.cfgDiff == 1 then
      val (newBoardOpt, newRand, newOpen, dest) =
        AIPlayer.playRandomly(GameState.board, GameState.rand, Stone.Black, GameState.open, AIPlayer.randomMove)
      GameState.rand = newRand
      newBoardOpt.foreach { nb => GameState.board = nb; GameState.open = newOpen }
      if shouldPrint then
        println(s"  PC jogou para ${dest.map(d => s"(${d._1},${d._2})").getOrElse("?")}")
        GameState.onStateChanged()
    else
      //para diff 2, jogada vai ser a que vai capturar mais pecas de uma vez
      val best = GameState.board.toList.collect { case (c, Stone.Black) => c }.flatMap { from =>
        GameEngine.validDestinations(GameState.board, Stone.Black, from, GameState.open).flatMap { to =>
          GameEngine.play(GameState.board, Stone.Black, from, to, GameState.open) match
            case (Some(nb), newOpen) => Some((GameState.board.size - nb.size, from, to, nb, newOpen))
            case _ => None

        }
      }

      if best.nonEmpty then
        val (_, from, to, nb, newOpen) = best.maxBy(_._1) //fazer jogada com mais capturas
        GameState.board = nb;
        GameState.open = newOpen

        if shouldPrint then
          println(s"  PC jogou (${from._1},${from._2}) -> (${to._1},${to._2})")

        GameState.onStateChanged()
        Thread.sleep(600) // Pequena pausa para observar cada salto do PC

        //tentar continuar a capturar
        if multiplecaptures then
          continuePcCaptures(to)

    //terminar turno
    GameState.resetTurnTimer()
    GameState.currentPlayer = Stone.White
    GameState.onStateChanged()
    //ver se o jogador ainda tem jogadas, senao ganha o bot
    GameEngine.checkWinner(GameState.board, Stone.White, GameState.open) match
      case Some(w) =>
        GameState.gameActive = false
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.onStateChanged()
      case None =>
        if shouldPrint then
          println(TUI.renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))

}
