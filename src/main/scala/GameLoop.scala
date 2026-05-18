import GUI.*
import TUI.*
import GameEngine.*
import GameEngine.given
import GameDomain.*
import GameUtils.*
import AIPlayer.*
import scala.annotation.tailrec


object GameLoop {

  val shouldPrint = GameState.cfgMode == 1 || GameState.cfgMode == 3

  def startGame(): Unit =
    val full = GameEngine.initBoard(GameState.cfgRows, GameState.cfgCols)
    GameState.board = GameEngine.setupBoard(full, GameState.cfgRows, GameState.cfgCols)
    GameState.open = full.keys.filterNot(GameState.board.contains).toList
    GameState.currentPlayer = Stone.White
    GameState.rand = MyRandom(this.hashCode().toLong)
    GameState.history = Nil
    GameState.gameActive = true
    GameState.resetTurnTimer()
    GameState.onStateChanged()


  def finishWhiteTurn(): Unit =
    GameState.resetTurnTimer()
    GameState.currentPlayer = Stone.Black
    GameState.onStateChanged()
    GameEngine.checkWinner(GameState.board, Stone.Black, GameState.open) match
      case Some(w) =>
        GameState.gameActive = false
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.onStateChanged()
      case None =>
        // Correr o PC numa thread separada para nao bloquear a GUI
        val t = new Thread(() => AIPlayer.doComputerMove())
        t.setDaemon(true)
        t.start()


  def doUndo(): Unit =
    GameState.history match
      case Nil =>
        println("  Sem jogadas para desfazer!")
      case (oldBoard, oldOpen, oldPlayer) :: rest =>
        GameState.board = oldBoard
        GameState.open = oldOpen
        GameState.currentPlayer = oldPlayer
        GameState.history = rest
        GameState.resetTurnTimer()
        println("  Undo feito.")
        GameState.onStateChanged()

  def registerGuiCallbacks(): Unit =
    val app = GUI.instance

    GUI.onHumanMove = (from, to) =>
      if !GameState.gameActive || GameState.currentPlayer != Stone.White then return
      // Guardar historico apenas no primeiro salto
      if !GameState.history.headOption.exists { case (b,_,_ ) => b eq GameState.board} then
        GameState.history = (GameState.board, GameState.open, Stone.White) :: GameState.history
      GameEngine.play(GameState.board, Stone.White, from, to, GameState.open) match
        case (Some(nb), newOpen) =>
          GameState.board = nb;
          GameState.open = newOpen
          val moreDests = GameEngine.validDestinations(nb, Stone.White, to, newOpen)
          GameState.onStateChanged()
          app.notifyJumpDone(to, moreDests)
          if moreDests.isEmpty then finishWhiteTurn()
        case _ =>
          GameState.onStateChanged()

    GUI.onEndTurn = () => finishWhiteTurn()
    GUI.onSkipTurn = () => finishWhiteTurn()
    GUI.onUndo = () => doUndo()
    GUI.onRestart = () => {
      startGame();
      if shouldPrint then
        println(TUI.renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
    }
}
