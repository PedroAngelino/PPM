import scala.collection.parallel.CollectionConverters.*
import GameDomain.*
import GameUtils.*

// Estado partilhado entre TUI (Main) e GUI (T8)
// Ambos leem e escrevem neste objeto para se manterem sincronizados
object GameState:
  var board: Board                                    = _
  var open: List[Coord2D]                             = Nil
  var currentPlayer: Stone                            = Stone.White
  var rand: MyRandom                                  = MyRandom(this.hashCode().toLong)
  var history: List[(Board, List[Coord2D], Stone)]    = Nil
  var gameActive: Boolean                             = false
  var cfgRows: Int                                    = 6
  var cfgCols: Int                                    = 6
  var cfgTime: Int                                    = 10000
  var cfgDiff: Int                                    = 1

  // Callback chamado sempre que o estado muda — a GUI usa isto para re-desenhar
  var onStateChanged: () => Unit = () => ()
