import GUI.*
import TUI.*
import GameEngine.*
import GameEngine.given
import GameDomain.*
import GameUtils.*
import AIPlayer.*
import scala.annotation.tailrec


object GameLoop {

  //so deve imprimir no terminal se tiver o TUI ativo
  val shouldPrint = GameState.cfgMode == 1 || GameState.cfgMode == 3

  def startGame(): Unit =
    val full = GameEngine.initBoard(GameState.cfgRows, GameState.cfgCols) //cria o tablueiro
    GameState.board = GameEngine.setupBoard(full, GameState.cfgRows, GameState.cfgCols) //iniciar board de jogo
    GameState.open = full.keys.filterNot(GameState.board.contains).toList //coordenadas livres
    GameState.currentPlayer = Stone.White //escolher o nosso jogador
    GameState.rand = MyRandom(this.hashCode().toLong) //criar random com seed
    GameState.history = Nil //historia de plays vazia
    GameState.gameActive = true //ativar jogo
    GameState.resetTurnTimer() //iniciar timer
    GameState.onStateChanged()

  //quando acabar o move do jogador, muda de player e confirma se o jogador ganhou
  def finishWhiteTurn(): Unit =
    GameState.resetTurnTimer()
    GameState.currentPlayer = Stone.Black
    GameState.onStateChanged()
    GameEngine.checkWinner(GameState.board, Stone.Black, GameState.open) match
      case Some(w) =>
        GameState.gameActive = false //desativar o jogo se ganhou
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.onStateChanged()
      case None =>
        // Correr o PC numa thread separada para nao bloquear a GUI
        val t = new Thread(() => AIPlayer.doComputerMove())
        t.setDaemon(true)
        t.start()


  def doUndo(): Unit =//desfazer a ultima jogada do player
    GameState.history match
      case Nil => //se nao tiver jogadas
        println("  Sem jogadas para desfazer!")
      case (oldBoard, oldOpen, oldPlayer) :: rest => //pegar os estados anteriores e separar o ultimo
        GameState.board = oldBoard //restaurar tudo ao anterior
        GameState.open = oldOpen
        GameState.currentPlayer = oldPlayer
        GameState.history = rest //remover o ultimo move do histotico
        GameState.resetTurnTimer()
        println("  Undo feito.")
        GameState.onStateChanged() //redesenhar

  //liga os botoes do GUI as funcoes do GameLoop
  def registerGuiCallbacks(): Unit =
    val app = GUI.instance //associar o GUI

    GUI.onHumanMove = (from, to) =>
      if !GameState.gameActive || GameState.currentPlayer != Stone.White then return
      // Guardar historico apenas no primeiro salto do turno
      if !GameState.history.headOption.exists { case (b,_,_ ) => b eq GameState.board} then
        GameState.history = (GameState.board, GameState.open, Stone.White) :: GameState.history
        //tenta executar a jogada
      GameEngine.play(GameState.board, Stone.White, from, to, GameState.open) match
        case (Some(nb), newOpen) =>
          GameState.board = nb;
          GameState.open = newOpen
          val moreDests = GameEngine.validDestinations(nb, Stone.White, to, newOpen)
          GameState.onStateChanged()
          //notificar a GUI se o turno acabou ou nao
          app.notifyJumpDone(to, moreDests)
          if moreDests.isEmpty then finishWhiteTurn() //se nao houverem mais moves para o white
        case _ =>
          GameState.onStateChanged()

    GUI.onEndTurn = () => finishWhiteTurn()  //associar os botoes do gui a suas funcoes
    GUI.onSkipTurn = () => finishWhiteTurn()
    GUI.onUndo = () => doUndo()
    GUI.onRestart = () => {
      startGame();
      if shouldPrint then
        println(TUI.renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
    }
}
