import javafx.application.{Application, Platform}
import javafx.geometry.{Insets, Pos}
import javafx.scene.Scene
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{BorderPane, GridPane, HBox, StackPane}
import javafx.scene.paint.Color
import javafx.scene.shape.{Circle, Rectangle}
import javafx.scene.text.{Font, FontWeight}
import javafx.stage.Stage
import TUI.*
import GameEngine.*
import GameEngine.given
import GameDomain.*
import GameUtils.*
import AIPlayer.*

object GUI {

  var onHumanMove: (Coord2D, Coord2D) => Unit = (_, _) => ()
  var onEndTurn: () => Unit = () => ()
  var onSkipTurn: () => Unit = () => ()
  var onUndo: () => Unit = () => ()
  var onRestart: () => Unit = () => ()

  // aqui fica guardada a instancia da janela do JavaFX
  var instance: KonaneApp = _

  def launch(): Unit =
    val t = new Thread(() => Application.launch(classOf[KonaneApp]))
    t.setDaemon(true)
    t.start()

  class KonaneApp extends Application:

    private var primaryStage: Stage = _
    private var gridPane: GridPane = _
    private var statusLabel: Label = _
    private var timerLabel: Label = _
    private var endTurnBtn: Button = _

    // variaveis que a GUI usa para saber o que esta selecionado
    private var selectedCoord: Option[Coord2D] = None
    private var validDests: List[Coord2D] = Nil
    private var canEndTurn: Boolean = false
    private var timeExpired: Boolean = false

    private val CELL = 70

    override def start(stage: Stage): Unit =
      primaryStage = stage
      stage.setTitle("Konane - GUI")
      stage.setResizable(false)
      buildGameScreen()
      startTimerThread()
      // quando o estado muda no Main, a GUI atualiza outra vez
      GameState.onStateChanged = () =>
        if GameState.cfgMode != 1 then Platform.runLater(() => refresh())
      GUI.instance = this

    private def buildGameScreen(): Unit =
      if GameState.cfgMode == 1 then return // se for so TUI nao abre a janela
      val root = new BorderPane()
      root.setStyle("-fx-background-color: white;")

      // parte de cima da janela
      val topBar = new HBox(20)
      topBar.setPadding(new Insets(10, 15, 10, 15))
      topBar.setStyle("-fx-background-color: black;")
      topBar.setAlignment(Pos.CENTER_LEFT)
      val titleLbl = new Label("KONANE")
      titleLbl.setFont(Font.font("Georgia", FontWeight.BOLD, 18))
      titleLbl.setTextFill(Color.WHITE)
      statusLabel = new Label("A aguardar inicio...")
      statusLabel.setTextFill(Color.LIGHTGRAY)
      timerLabel = new Label(s"Tempo: ${GameState.cfgTime / 1000}s")
      timerLabel.setTextFill(Color.WHITE)
      timerLabel.setFont(Font.font("Arial", FontWeight.BOLD, 13))
      topBar.getChildren.addAll(titleLbl, statusLabel, timerLabel)
      root.setTop(topBar)

      // zona onde fica o tabuleiro
      gridPane = new GridPane()
      gridPane.setPadding(new Insets(20))
      gridPane.setHgap(2);
      gridPane.setVgap(2)
      gridPane.setAlignment(Pos.CENTER)
      root.setCenter(gridPane)

      // botoes de baixo
      endTurnBtn = btn("Terminar Turno")
      endTurnBtn.setVisible(false)
      endTurnBtn.setOnAction(_ => {
        canEndTurn = false; selectedCoord = None; validDests = Nil; endTurnBtn.setVisible(false); GUI.onEndTurn()
      })

      val skipBtn = btn("Pular Jogada")
      skipBtn.setOnAction(_ => {
        selectedCoord = None; validDests = Nil; canEndTurn = false; endTurnBtn.setVisible(false); GUI.onSkipTurn()
      })

      val undoBtn = btn("Undo")
      undoBtn.setOnAction(_ => {
        selectedCoord = None; validDests = Nil; canEndTurn = false; endTurnBtn.setVisible(false); GUI.onUndo()
      })

      val restartBtn = btn("Reiniciar")
      restartBtn.setOnAction(_ => {
        selectedCoord = None; validDests = Nil; canEndTurn = false; endTurnBtn.setVisible(false); GUI.onRestart()
      })

      val bottomBar = new HBox(12, endTurnBtn, skipBtn, undoBtn, restartBtn)
      bottomBar.setPadding(new Insets(12))
      bottomBar.setAlignment(Pos.CENTER)
      bottomBar.setStyle("-fx-background-color: #f0f0f0; -fx-border-color: #cccccc; -fx-border-width: 1 0 0 0;")
      root.setBottom(bottomBar)

      val cols = GameState.cfgCols;
      val rows = GameState.cfgRows
      primaryStage.setScene(new Scene(root, (cols * (CELL + 2) + 80).toDouble, (rows * (CELL + 2) + 130).toDouble))
      primaryStage.show()

    // atualiza o ecra quando alguma coisa muda no jogo
    private def refresh(): Unit =
      renderBoard()
      timeExpired = false
      updateTimerLabel()
      statusLabel.setText(
        if !GameState.gameActive then "Jogo terminado."
        else if GameState.currentPlayer == Stone.White then "O seu turno (GUI)"
        else "Turno das Pretas (PC)..."
      )

    private def startTimerThread(): Unit =
      val t = new Thread(() =>
        while true do
          Thread.sleep(500)
          Platform.runLater(() => updateTimerLabel())
      )
      t.setDaemon(true)
      t.start()

    private def updateTimerLabel(): Unit =
      if timerLabel == null then return
      if !GameState.gameActive then
        timerLabel.setText(s"Tempo: ${GameState.cfgTime / 1000}s")
      else
        val elapsed = (System.currentTimeMillis() - GameState.turnStartTime).toInt
        val remaining = Math.max(0, (GameState.cfgTime - elapsed + 999) / 1000)
        timerLabel.setText(s"Tempo: ${remaining}s")
        if remaining == 0 && GameState.currentPlayer == Stone.White && !timeExpired then
          timeExpired = true
          selectedCoord = None
          validDests = Nil
          canEndTurn = false
          endTurnBtn.setVisible(false)
          statusLabel.setText("Tempo esgotado. Turno perdido.")
          GUI.onSkipTurn()

    // desenha o tabuleiro usando o estado atual do jogo
    private def renderBoard(): Unit =
      gridPane.getChildren.clear()
      val board = GameState.board
      val open = GameState.open
      val rows = GameState.cfgRows
      val cols = GameState.cfgCols

      if board == null then return

      for r <- 0 until rows; c <- 0 until cols do
        val coord = (r, c)
        val pane = new StackPane()
        pane.setPrefSize(CELL, CELL)

        val baseColor = if (r + c) % 2 == 0 then Color.web("#e8e8e8") else Color.web("#c0c0c0")
        val bgColor =
          if selectedCoord.contains(coord) then Color.web("#4444ff", 0.5)
          else if validDests.contains(coord) then Color.web("#ffff00", 0.7)
          else baseColor
        val bg = new Rectangle(CELL, CELL); bg.setFill(bgColor)
        pane.getChildren.add(bg)

        board.get(coord).foreach: stone =>
          val circle = new Circle(CELL / 2.0 - 8)
          circle.setFill(if stone == Stone.Black then Color.BLACK else Color.WHITE)
          circle.setStroke(Color.BLACK);
          circle.setStrokeWidth(2)
          pane.getChildren.add(circle)

        if !board.contains(coord) && open.contains(coord) then
          pane.getChildren.add(new Circle(4, Color.web("#00aa00", 0.6)))

        // so da para clicar quando e o turno das brancas
        if GameState.gameActive && GameState.currentPlayer == Stone.White then
          val isWhite = board.get(coord).contains(Stone.White)
          val isDest = validDests.contains(coord)
          val clickable = (isWhite && !canEndTurn) || isDest
          if clickable then
            pane.setStyle("-fx-cursor: hand;")
            pane.setOnMouseClicked(_ => handleClick(coord))

        gridPane.add(pane, c, r)

    // trata dos cliques no tabuleiro
    private def handleClick(coord: Coord2D): Unit =
      if !GameState.gameActive || GameState.currentPlayer != Stone.White then return
      selectedCoord match
        case None =>
          // escolhe a peca e mostra para onde ela pode ir
          val dests = GameEngine.validDestinations(GameState.board, Stone.White, coord, GameState.open)
          if GameState.board.get(coord).contains(Stone.White) && dests.nonEmpty then
            selectedCoord = Some(coord);
            validDests = dests;
            renderBoard()
          else
            statusLabel.setText("Peca sem movimentos validos.")

        case Some(from) if validDests.contains(coord) =>
          // faz a jogada e depois o Main trata de atualizar o jogo
          val prevSelected = from
          selectedCoord = None;
          validDests = Nil
          GUI.onHumanMove(prevSelected, coord)

        case _ =>
          if !canEndTurn then
            val dests = GameEngine.validDestinations(GameState.board, Stone.White, coord, GameState.open)
            if GameState.board.get(coord).contains(Stone.White) && dests.nonEmpty then
              selectedCoord = Some(coord);
              validDests = dests;
              renderBoard()

    // o Main chama isto depois de um salto para ver se ainda da para continuar
    def notifyJumpDone(to: Coord2D, moreDests: List[Coord2D]): Unit =
      Platform.runLater: () =>
        if moreDests.nonEmpty then
          selectedCoord = Some(to);
          validDests = moreDests;
          canEndTurn = true
          endTurnBtn.setVisible(true)
          statusLabel.setText("Pode continuar ou terminar o turno.")
          renderBoard()
        else
          selectedCoord = None;
          validDests = Nil;
          canEndTurn = false
          endTurnBtn.setVisible(false)

    private def btn(text: String): Button =
      val b = new Button(text)
      b.setFont(Font.font("Arial", 12));
      b.setPrefHeight(36)
      b.setStyle("-fx-background-color: white; -fx-text-fill: black; -fx-border-color: black; -fx-border-width: 1; -fx-background-radius: 3; -fx-cursor: hand;")
      b

}
