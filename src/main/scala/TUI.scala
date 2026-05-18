import scala.annotation.tailrec
import GameDomain.*
import GameEngine.*
import GameEngine.given
import GameUtils.*
import GameLoop.*

import scala.io.StdIn


object TUI {
  private def cell(board: Board, coord: Coord2D): String = //pegar a stone numa celula de uma certa coordenadas
    GameDomain.getStone(board)(coord) match
      case Some(GameDomain.Stone.Black) => "B"
      case Some(GameDomain.Stone.White) => "W"
      case None                 => "."

  private def renderRow( //vai chamar o metodo cell para ler uma row
                         row: Int,
                         col: Int,
                         cols: Int,
                         board: Board,
                         complete: List[String]
                       ): List[String] =
    if col >= cols then complete.reverse //quando chegar ao fim da linha dá reverse no resultado final, para ficar na ordem certa
    else
      val value = cell(board, (row, col))
      renderRow(row, col + 1, cols, board, value :: complete)

  @tailrec
  private def renderRows( //vai dar render em todas as rows
                          r: Int,
                          rows: Int,
                          cols: Int,
                          board: Board,
                          complete: List[String]
                        ): List[String] =
    if r >= rows then complete.reverse //quando tiver todas as linhas vai colocar na ordem certa
    else
      val rowString = renderRow(r, 0, cols, board, Nil).mkString(" ") //cria a string com " " como separação
      renderRows(r + 1, rows, cols, board, rowString :: complete)

  private def header(cols: Int): String = //linhas numericas

    @tailrec
    def loop(c: Int, complete: List[String]): List[String] =
      if c >= cols then complete.reverse //quando processar todas as colunas retorna o inverso
      else loop(c + 1, c.toString :: complete)

    "  " + loop(0, Nil).mkString(" ") //transformar a lista final numa string

  def renderBoard(board: Board, rows: Int, cols: Int): String =
    val head = header(cols) //criar linha numerica

    val body =
      renderRows(0, rows, cols, board, Nil)
        .zipWithIndex //para cada linha ter o seu proprio indice
        .map { case (row, i) => s"$i $row" } //colocar o indice no inicio de cada linha
        .mkString("\n") //justar tudo numa so string com separação de troca de linha

    s"$head\n$body" //o header ficar em cima e o corpo em baixo


  def parseCoord(s: String): Option[Coord2D] =
    s.trim.split("\\s+") match
      case Array(r, c) =>
        try Some((r.toInt, c.toInt))
        catch case _: NumberFormatException => None
      case _ => None

  def showValidMoves(): Unit =
    val moves = GameState.board.toList
      .collect { case (c, s) if s == GameState.currentPlayer =>
        c -> GameEngine.validDestinations(GameState.board, GameState.currentPlayer, c, GameState.open)
      }
      .filter { case (_, dests) => dests.nonEmpty }
      .sortBy(_._1)
    println(s"\n  Moves validos para ${GameState.currentPlayer}:")
    moves.foreach { case (from, dests) =>
      println(s"    De (${from._1},${from._2})  ->  ${dests.map(d => s"(${d._1},${d._2})").mkString("  ")}")
    }

  @tailrec
  def humanTurnTUI(movedFrom: Option[Coord2D], startTime: Long): Unit =
    showValidMoves()
    movedFrom match
      case Some(locked) =>
        println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
        println(s"  Peca em (${locked._1},${locked._2}). 'row col' para andar ou ENTER para acabar:")
        val line = StdIn.readLine().trim
        if GameUtils.isTimeOver(startTime, GameState.cfgTime) then
          println("[!] Tempo esgotado! Turno perdido.")
          GameLoop.finishWhiteTurn()
        else if line.isEmpty then
          GameLoop.finishWhiteTurn()
        else
          parseCoord(line) match
            case Some(to) =>
              GameEngine.play(GameState.board, Stone.White, locked, to, GameState.open) match
                case (Some(nb), newOpen) =>
                  GameState.board = nb;
                  GameState.open = newOpen
                  GameState.onStateChanged()
                  if GameEngine.validDestinations(nb, Stone.White, to, newOpen).nonEmpty then humanTurnTUI(Some(to), startTime)
                  else  GameLoop.finishWhiteTurn()
                case _ =>
                  println("  Move invalido."); humanTurnTUI(movedFrom, startTime)
            case None =>
              println("  Mau input."); humanTurnTUI(movedFrom, startTime)

      case None =>
        println(s"\n  Turno de ${GameState.currentPlayer}. Introduz: fromRow fromCol toRow toCol ou 'undo'")
        val line = StdIn.readLine().trim.toLowerCase
        if GameUtils.isTimeOver(startTime, GameState.cfgTime) then
          println("[!] Tempo esgotado! Turno perdido.")
          GameLoop.finishWhiteTurn()
        else if line == "undo" then
          GameLoop.doUndo();
          humanTurnTUI(None, startTime)
        else
          line.split("\\s+") match
            case Array(r1, c1, r2, c2) =>
              (parseCoord(s"$r1 $c1"), parseCoord(s"$r2 $c2")) match
                case (Some(from), Some(to)) =>
                  GameState.history = (GameState.board, GameState.open, Stone.White) :: GameState.history
                  GameEngine.play(GameState.board, Stone.White, from, to, GameState.open) match
                    case (Some(nb), newOpen) =>
                      GameState.board = nb;
                      GameState.open = newOpen
                      GameState.onStateChanged()
                      if GameEngine.validDestinations(nb, Stone.White, to, newOpen).nonEmpty then humanTurnTUI(Some(to), startTime)
                      else  GameLoop.finishWhiteTurn()
                    case _ =>
                      println("  Move invalido."); humanTurnTUI(None, startTime)
                case _ =>
                  println("  Mau input."); humanTurnTUI(None, startTime)
            case _ =>
              println("  Mau input."); humanTurnTUI(None, startTime)

  @tailrec
  def gameLoopTUI(): Unit =
    if !GameState.gameActive then return
    println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
    GameEngine.checkWinner(GameState.board, GameState.currentPlayer, GameState.open) match
      case Some(w) =>
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.gameActive = false
        GameState.onStateChanged()
      case None =>
        if GameState.currentPlayer == Stone.White then
          humanTurnTUI(None, GameState.turnStartTime)
        else
          GameLoop.doComputerMove()
        gameLoopTUI()

  @tailrec
  def mainMenu(): Unit =
    println("\n" + "=" * 30)
    println("        KONANE - MENU")
    println("=" * 30)
    println(s"1. Jogar")
    println(s"2. Nivel de dificuldade (Atual: Nivel ${GameState.cfgDiff})")
    println(s"3. Definir tempo por jogada (Atual: ${GameState.cfgTime / 1000}s)")
    println(s"0. Sair")
    println("=" * 30)
    print("Escolha uma opcao: ")
    StdIn.readLine().trim match
      case "1" =>
          print("Modo de jogo (1-TUI, 2-GUI, 3-Ambos): ")
          GameState.cfgMode = StdIn.readLine().toIntOption.getOrElse(3)

          if GameState.cfgMode == 2 || GameState.cfgMode == 3 then
            if GUI.instance == null then GUI.launch()
            while GUI.instance == null do Thread.sleep(100)
            GameLoop.registerGuiCallbacks()

          GameLoop.startGame()
          if GameState.cfgMode == 1 || GameState.cfgMode == 3 then
            gameLoopTUI()
          else
            // Modo so GUI: esperar que o jogo termine sem bloquear com a TUI
            println("A jogar na GUI. Aguarda o fim do jogo...")
            while GameState.gameActive do Thread.sleep(500)
            println("Jogo terminado.")
          mainMenu()
      case "2" =>
        print("Nova dificuldade (1-Facil, 2-Dificil): ")
        GameState.cfgDiff = StdIn.readLine().toIntOption.getOrElse(GameState.cfgDiff)
        mainMenu()
      case "3" =>
        print("Novo tempo limite (segundos): ")
        GameState.cfgTime = StdIn.readLine().toIntOption.getOrElse(GameState.cfgTime / 1000) * 1000
        mainMenu()
      case "0" =>
        println("Adeus!")
      case _ =>
        println("Opcao invalida.")
        mainMenu()
}
