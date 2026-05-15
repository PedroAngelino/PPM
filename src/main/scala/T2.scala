
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec
import T1.Coord2D
import T1.Board

object T2 {

  enum Stone:
    case Black, White
    def opponent: Stone = this match
      case Black => White
      case White => Black

  def hasStone(board: Board)(coord: Coord2D): Boolean = board.contains(coord)
  def getStone(board: Board)(coord: Coord2D): Option[Stone] = board.get(coord)
  def isOwned(board: Board)(player: Stone)(coord: Coord2D): Boolean =
    board.get(coord).contains(player)

  def initBoard(rows: Int, cols: Int): Board =
    @tailrec
    def build(r: Int, c: Int, acc: Map[Coord2D, Stone]): Map[Coord2D, Stone] =
      (r, c) match
        case (row, _) if row >= rows  => acc
        case (row, col) if col >= cols => build(row + 1, 0, acc)
        case (row, col) =>
          val stone = if (row + col) % 2 == 0 then Stone.Black else Stone.White
          build(row, col + 1, acc + ((row, col) -> stone))

    build(0, 0, Map.empty).par

  def setupBoard(board: Board, rows: Int, cols: Int): Board =
    val adjDirs = List((0, 1), (0, -1), (1, 0), (-1, 0))
    val centers = List((rows/2, cols/2), (rows/2-1, cols/2-1), (0,0), (0, cols-1), (rows-1, 0), (rows-1, cols-1))

    @tailrec
    def findAdj(c: Coord2D, dirs: List[Coord2D]): Option[(Coord2D, Coord2D)] =
      dirs match
        case Nil => None
        case (dr, dc) :: t =>
          val a = (c._1 + dr, c._2 + dc)
          (board.get(c), board.get(a)) match
            case (Some(Stone.Black), Some(Stone.White)) => Some((c, a))
            case (Some(Stone.White), Some(Stone.Black)) => Some((a, c))
            case _ => findAdj(c, t)

    @tailrec
    def findPair(candidates: List[Coord2D]): Option[(Coord2D, Coord2D)] =
      candidates match
        case Nil => None
        case h :: t => findAdj(h, adjDirs) match
          case Some(p) => Some(p)
          case None    => findPair(t)

    findPair(centers) match
      case Some((b, w)) => board - b - w
      case None         => board

  def capturedCoord(from: Coord2D, to: Coord2D): Option[Coord2D] =
    (from, to) match
      case ((r1, c1), (r2, c2)) =>
        (r2 - r1, c2 - c1) match
          case (2, 0)  => Some((r1 + 1, c1))
          case (-2, 0) => Some((r1 - 1, c1))
          case (0, 2)  => Some((r1, c1 + 1))
          case (0, -2) => Some((r1, c1 - 1))
          case _       => None

  def validDestinations(board: Board, player: Stone, from: Coord2D, open: List[Coord2D]): List[Coord2D] =
    @tailrec
    def exists(target: Coord2D, l: List[Coord2D]): Boolean = l match
      case Nil => false
      case h :: _ if h == target => true
      case _ :: t => exists(target, t)

    @tailrec
    def check(dirs: List[Coord2D], acc: List[Coord2D]): List[Coord2D] =
      dirs match
        case Nil => acc
        case (dr, dc) :: t =>
          val dest = (from._1 + dr, from._2 + dc)
          val mid  = ((from._1 + dest._1)/2, (from._2 + dest._2)/2)
          val ok   = exists(dest, open) && isOwned(board)(player.opponent)(mid)
          check(t, if ok then dest :: acc else acc)

    check(List((-2, 0), (2, 0), (0, -2), (0, 2)), Nil)

  def play(board: Board, player: Stone, from: Coord2D, to: Coord2D, open: List[Coord2D])
          (using updater: (List[Coord2D], Coord2D, Coord2D) => List[Coord2D]): (Option[Board], List[Coord2D]) =

    @tailrec
    def exists(target: Coord2D, l: List[Coord2D]): Boolean = l match
      case Nil => false
      case h :: _ if h == target => true
      case _ :: t => exists(target, t)

    board.get(from) match
      case Some(s) if s == player && exists(to, open) =>
        capturedCoord(from, to) match
          case Some(mid) if board.get(mid).contains(player.opponent) =>
            val nb = board - from - mid + (to -> player)
            (Some(nb), updater(open, from, mid))
          case _ => (None, open)
      case _ => (None, open)

  given openCoordsUpdater: ((List[Coord2D], Coord2D, Coord2D) => List[Coord2D]) =
  (open, from, cap) =>
    @tailrec
    def clean(l: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = l match
      case Nil => acc
      case h :: t if h == from || h == cap => clean(t, acc)
      case h :: t => clean(t, h :: acc)
    from :: cap :: clean(open, Nil)
}