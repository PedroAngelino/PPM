import GameDomain.*

import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

object GameEngine {

  def initBoard(rows: Int, cols: Int): Board =
    @tailrec
    def build(r: Int, c: Int, acc: Map[Coord2D, Stone]): Map[Coord2D, Stone] =
      (r, c) match
        case (row, _) if row >= rows => acc
        case (row, col) if col >= cols => build(row + 1, 0, acc)
        case (row, col) =>
          val stone = if (row + col) % 2 == 0 then Stone.Black else Stone.White
          build(row, col + 1, acc + ((row, col) -> stone))

    build(0, 0, Map.empty).par

  def setupBoard(board: Board, rows: Int, cols: Int): Board =
    val adjDirs = List((0, 1), (0, -1), (1, 0), (-1, 0))
    val centers = List((rows / 2, cols / 2), (rows / 2 - 1, cols / 2 - 1), (0, 0), (0, cols - 1), (rows - 1, 0), (rows - 1, cols - 1))

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
          case None => findPair(t)

    findPair(centers) match
      case Some((b, w)) => board - b - w
      case None => board

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
          val mid = ((from._1 + dest._1) / 2, (from._2 + dest._2) / 2)
          val ok = exists(dest, open) && !board.contains(dest) && isOwned(board)(player.opponent)(mid)
          check(t, if ok then dest :: acc else acc)

    board.get(from) match
      case Some(s) if s == player => check(List((-2, 0), (2, 0), (0, -2), (0, 2)), Nil)
      case _ => Nil

  def checkWinner(board: Board, currentPlayer: Stone, open: List[Coord2D]): Option[Stone] = {

    val currentPlayerHasMoves = board.exists { case (coord, stone) =>
      // Confirma que a pedra é do jogador e que tem destinos válidos a partir daquela origem
      stone == currentPlayer && validDestinations(board, currentPlayer, coord, open).nonEmpty
    }

    if (currentPlayerHasMoves) {
      None // O jogador atual tem jogadas válidas, logo o jogo continua
    } else {
      // Se o jogador atual não tem jogadas, ele perde. O adversário é vencedor
      Some(currentPlayer.opponent)
    }
  }

  def play(board: Board, player: Stone, from: Coord2D, to: Coord2D, open: List[Coord2D])
          (using updater: (List[Coord2D], Coord2D, Coord2D, Coord2D) => List[Coord2D]): (Option[Board], List[Coord2D]) =

    @tailrec
    def exists(target: Coord2D, l: List[Coord2D]): Boolean = l match
      case Nil => false
      case h :: _ if h == target => true
      case _ :: t => exists(target, t)

    board.get(from) match
      case Some(s) if s == player && exists(to, open) && !board.contains(to) =>
        capturedCoord(from, to) match
          case Some(mid) if board.get(mid).contains(player.opponent) =>
            val nb = board - from - mid + (to -> player)
            (Some(nb), updater(open, from, mid, to))
          case _ => (None, open)
      case _ => (None, open)
  
  def capturedCoord(from: Coord2D, to: Coord2D): Option[Coord2D] =
    (from, to) match
      case ((r1, c1), (r2, c2)) =>
        (r2 - r1, c2 - c1) match
          case (2, 0) => Some((r1 + 1, c1))
          case (-2, 0) => Some((r1 - 1, c1))
          case (0, 2) => Some((r1, c1 + 1))
          case (0, -2) => Some((r1, c1 - 1))
          case _ => None
  
  given openCoordsUpdater: ((List[Coord2D], Coord2D, Coord2D, Coord2D) => List[Coord2D]) =
    (open, from, cap, to) =>
      @tailrec
      def clean(l: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = l match
        case Nil => acc
        case h :: t if h == from || h == cap || h == to => clean(t, acc)
        case h :: t => clean(t, h :: acc)

      from :: cap :: clean(open, Nil)
}  
