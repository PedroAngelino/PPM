import GUI.*
import TUI.*
import GameEngine.*
import GameEngine.given
import GameDomain.*
import GameUtils.*
import AIPlayer.*


object GameLoop {

  def startGame(): Unit =
    val full = GameEngine.initBoard(GameState.cfgRows, GameState.cfgCols)
    GameState.board = GameEngine.setupBoard(full, GameState.cfgRows, GameState.cfgCols)
    GameState.open = full.keys.filterNot(GameState.board.contains).toList
    GameState.currentPlayer = Stone.White
    GameState.rand = MyRandom(this.hashCode().toLong)
    GameState.history = Nil
    GameState.gameActive = true
    GameState.onStateChanged()

  def finishWhiteTurn(): Unit =
    GameState.currentPlayer = Stone.Black
    GameState.onStateChanged()
    GameEngine.checkWinner(GameState.board, Stone.Black, GameState.open) match
      case Some(w) =>
        GameState.gameActive = false
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.onStateChanged()
      case None =>
        // Correr o PC numa thread separada para nao bloquear a GUI
        val t = new Thread(() => doComputerMove())
        t.setDaemon(true)
        t.start()

  def doComputerMove(): Unit =
    Thread.sleep(600) // pequena pausa para a GUI mostrar o estado antes do PC jogar
    if GameState.cfgDiff == 1 then
      val (newBoardOpt, newRand, newOpen, dest) =
        AIPlayer.playRandomly(GameState.board, GameState.rand, Stone.Black, GameState.open, AIPlayer.randomMove)
      GameState.rand = newRand
      newBoardOpt.foreach { nb => GameState.board = nb; GameState.open = newOpen }
      println(s"  PC jogou para ${dest.map(d => s"(${d._1},${d._2})").getOrElse("?")}")
    else
      val best = GameState.board.toList.collect { case (c, Stone.Black) => c }.flatMap { from =>
        GameEngine.validDestinations(GameState.board, Stone.Black, from, GameState.open).flatMap { to =>
          GameEngine.play(GameState.board, Stone.Black, from, to, GameState.open) match
            case (Some(nb), newOpen) => Some((GameState.board.size - nb.size, from, to, nb, newOpen))
            case _ => None
        }
      }
      if best.nonEmpty then
        val (_, from, to, nb, newOpen) = best.maxBy(_._1)
        GameState.board = nb;
        GameState.open = newOpen
        println(s"  PC jogou (${from._1},${from._2}) -> (${to._1},${to._2})")

    GameState.currentPlayer = Stone.White
    GameState.onStateChanged()
    GameEngine.checkWinner(GameState.board, Stone.White, GameState.open) match
      case Some(w) =>
        GameState.gameActive = false
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.onStateChanged()
      case None =>
        println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))

  def doUndo(): Unit =
    GameState.history match
      case Nil =>
        println("  Sem jogadas para desfazer!")
      case (oldBoard, oldOpen, oldPlayer) :: rest =>
        GameState.board = oldBoard
        GameState.open = oldOpen
        GameState.currentPlayer = oldPlayer
        GameState.history = rest
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
      startGame(); println(TUI.renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
    }
}
