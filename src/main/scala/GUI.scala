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

  // Referencia à instancia da app JavaFX, guardada quando start() é chamado
  var instance: KonaneApp = _

  def launch(): Unit =
    val t = new Thread(() => Application.launch(classOf[KonaneApp]))
    t.setDaemon(true)
    t.start()

  class KonaneApp extends Application:

    private var primaryStage: Stage = _
    private var gridPane: GridPane = _
    private var statusLabel: Label = _
    private var endTurnBtn: Button = _

    // Estado local de selecao — unico estado proprio da GUI
    private var selectedCoord: Option[Coord2D] = None
    private var validDests: List[Coord2D] = Nil
    private var canEndTurn: Boolean = false

    private val CELL = 70

    override def start(stage: Stage): Unit =
      GUI.instance = this
      primaryStage = stage
      stage.setTitle("Konane - GUI")
      stage.setResizable(false)
      buildGameScreen()
      // Registar o callback: sempre que o Main muda o estado, a GUI re-desenha
      GameState.onStateChanged = () =>
        if GameState.cfgMode != 1 then Platform.runLater(() => refresh())

    // ===================================================
    // CONSTRUCAO DO ECRA (feito uma vez)
    // ===================================================
    private def buildGameScreen(): Unit =
      if GameState.cfgMode == 1 then return // modo só TUI, nao mostrar janela
      val root = new BorderPane()
      root.setStyle("-fx-background-color: white;")

      // Barra de topo
      val topBar = new HBox(20)
      topBar.setPadding(new Insets(10, 15, 10, 15))
      topBar.setStyle("-fx-background-color: black;")
      topBar.setAlignment(Pos.CENTER_LEFT)
      val titleLbl = new Label("KONANE")
      titleLbl.setFont(Font.font("Georgia", FontWeight.BOLD, 18))
      titleLbl.setTextFill(Color.WHITE)
      statusLabel = new Label("A aguardar inicio...")
      statusLabel.setTextFill(Color.LIGHTGRAY)
      topBar.getChildren.addAll(titleLbl, statusLabel)
      root.setTop(topBar)

      // Tabuleiro
      gridPane = new GridPane()
      gridPane.setPadding(new Insets(20))
      gridPane.setHgap(2);
      gridPane.setVgap(2)
      gridPane.setAlignment(Pos.CENTER)
      root.setCenter(gridPane)

      // Barra de botoes
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

    // ===================================================
    // REFRESH — chamado pelo Main via GameState.onStateChanged
    // ===================================================
    private def refresh(): Unit =
      renderBoard()
      statusLabel.setText(
        if !GameState.gameActive then "Jogo terminado."
        else if GameState.currentPlayer == Stone.White then "O seu turno (GUI)"
        else "Turno das Pretas (PC)..."
      )

    // ===================================================
    // DESENHAR O TABULEIRO (so lê GameState, nao altera nada)
    // ===================================================
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

        // Clicavel apenas se for turno das Brancas
        if GameState.gameActive && GameState.currentPlayer == Stone.White then
          val isWhite = board.get(coord).contains(Stone.White)
          val isDest = validDests.contains(coord)
          val clickable = (isWhite && !canEndTurn) || isDest
          if clickable then
            pane.setStyle("-fx-cursor: hand;")
            pane.setOnMouseClicked(_ => handleClick(coord))

        gridPane.add(pane, c, r)

    // ===================================================
    // CLIQUE — selecao local, depois delega ao Main
    // ===================================================
    private def handleClick(coord: Coord2D): Unit =
      if !GameState.gameActive || GameState.currentPlayer != Stone.White then return
      selectedCoord match
        case None =>
          // Selecionar peca: calcula destinos validos localmente para highlight
          val dests = GameEngine.validDestinations(GameState.board, Stone.White, coord, GameState.open)
          if GameState.board.get(coord).contains(Stone.White) && dests.nonEmpty then
            selectedCoord = Some(coord);
            validDests = dests;
            renderBoard()
          else
            statusLabel.setText("Peca sem movimentos validos.")

        case Some(from) if validDests.contains(coord) =>
          // Executar salto: delega ao Main, que altera GameState e notifica de volta
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

    // Chamado pelo Main apos um salto valido, para permitir multi-salto na GUI
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
