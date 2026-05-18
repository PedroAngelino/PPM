import scala.collection.parallel.immutable.ParMap

object GameDomain {

  type Coord2D = (Int, Int) // linha, coluna
  type Board = ParMap[Coord2D, Stone]
  enum Stone:
    case Black, White

    def opponent: Stone = this match
      case Black => White
      case White => Black

  def hasStone(board: Board)(coord: Coord2D): Boolean = board.contains(coord)

  def getStone(board: Board)(coord: Coord2D): Option[Stone] = board.get(coord)

  def isOwned(board: Board)(player: Stone)(coord: Coord2D): Boolean =
    board.get(coord).contains(player)
}
