import scala.collection.parallel.immutable.ParMap
import T2.Stone

object T1 {

  type Coord2D = (Int, Int) // linha, coluna
  type Board = ParMap[Coord2D, Stone]

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) =
    val (index, newRand) = rand.nextInt(lstOpenCoords.length) //de uma lista de posições possiveis vai escolher uma aleatoriamente
    (lstOpenCoords(index), newRand)

}