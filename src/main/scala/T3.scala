import T2.*
import T2.given
import T1.Coord2D
import T1.Board

object T3 {

  def playRandomly(
                    board: Board,
                    r: MyRandom,
                    player: Stone,
                    lstOpenCoords: List[Coord2D],
                    f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    val playerPieces = board.toList.collect {
      case (coord, p) if p == player => coord
    }

    val validDestinationsMap: Map[Coord2D, List[Coord2D]] = lstOpenCoords.map { open =>
      val validOrigins = playerPieces.filter { from =>
        validDestinations(board, player, from, lstOpenCoords).contains(open)
      }
      open -> validOrigins
    }.filter { case (_, origins) => origins.nonEmpty }.toMap // Manter apenas as que têm jogadas

    if (validDestinationsMap.isEmpty) {
      // retorna estado inalterado
      (None, r, lstOpenCoords, None)
    } else {
      // escolher um dos destinos livres válidos
      val availableDestinations = validDestinationsMap.keys.toList
      val (selectedTo, r2) = f(availableDestinations, r)

      // Escolher uma das peças (origem) que conseguem saltar para o destino selecionado
      val possibleOrigins = validDestinationsMap(selectedTo)
      val (selectedFrom, r3) = f(possibleOrigins, r2)

      // Aplicar a jogada  com T2.play
      val (newBoardOpt, newOpenCoords) = play(board, player, selectedFrom, selectedTo, lstOpenCoords)

      // Devolvemos o novo tabuleiro
      (newBoardOpt, r3, newOpenCoords, Some(selectedTo))
    }
  }
}
