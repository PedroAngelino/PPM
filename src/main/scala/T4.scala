import scala.annotation.tailrec
import T1.Coord2D
import T1.Board

object T4 {

  private def cell(board: Board, coord: Coord2D): String = //pegar a stone numa celula de uma certa coordenadas
    T2.getStone(board)(coord) match
      case Some(T2.Stone.Black) => "B"
      case Some(T2.Stone.White) => "W"
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
}