import T1.{Board, Coord2D}
import T2.Stone

object T5 {

 
  def checkWinner(board: Board, currentPlayer: Stone, open: List[Coord2D]): Option[Stone] = {
    
    val currentPlayerHasMoves = board.exists { case (coord, stone) =>
      // Confirma que a pedra é do jogador e que tem destinos válidos a partir daquela origem
      stone == currentPlayer && T2.validDestinations(board, currentPlayer, coord, open).nonEmpty
    }

    if (currentPlayerHasMoves) {
      None // O jogador atual tem jogadas válidas, logo o jogo continua
    } else {
      // Se o jogador atual não tem jogadas, ele perde. O adversário é vencedor
      Some(currentPlayer.opponent)
    }
  }

}