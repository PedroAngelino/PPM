import scala.collection.parallel.CollectionConverters.*
import GameDomain.*
import GameUtils.*

// Estado partilhado entre TUI (Main) e GUI (T8)
// Ambos leem e escrevem neste objeto para se manterem sincronizados
object GameState:
  var board: Board                                    = _ //tablueiro do jogo
  var open: List[Coord2D]                             = Nil //posicoes livres
  var currentPlayer: Stone                            = Stone.White //player
  var rand: MyRandom                                  = MyRandom(this.hashCode().toLong) //random com seed
  var history: List[(Board, List[Coord2D], Stone)]    = Nil //historico vazio
  @volatile var gameActive: Boolean                   = false //se o jogo esta a decorrer, volatile para quando apenas jogamos na GUI o TUI nao ter escrever no terminal
  var cfgRows: Int                                    = 8 //dimensoes
  var cfgCols: Int                                    = 8
  var cfgTime: Int                                    = 10000 //tempo em ms das jogadas
  var cfgDiff: Int                                    = 1 //dificuldade
  var cfgMode: Int                                    = 3 // 1 = só TUI, 2 = só GUI, 3 = ambos
  var turnStartTime: Long                             = System.currentTimeMillis() //momento em q comeca o turno

  def resetTurnTimer(): Unit =
    turnStartTime = System.currentTimeMillis() //reinicia o tempo do turno

  // Callback chamado sempre que o estado muda — a GUI usa isto para re-desenhar
  var onStateChanged: () => Unit = () => ()
