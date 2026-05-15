import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.layout.{BorderPane, GridPane, StackPane, HBox}
import javafx.scene.shape.{Circle, Rectangle}
import javafx.scene.paint.Color
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage
import javafx.geometry.{Insets, Pos}

import T1.*
import T2.*
import T2.given

class KonaneGUI extends Application {

  // ==========================================
  // 1. ESTADO DA GUI (Variáveis Mutáveis)
  // ==========================================
  val Rows = 6
  val Cols = 6

  var board: Board = T2.setupBoard(T2.initBoard(Rows, Cols), Rows, Cols)
  var open: List[Coord2D] = board.keys.toList.take(2) // Ajusta para a tua initialOpenCoords
  var player: Stone = Stone.Black
  var selectedPiece: Option[Coord2D] = None

  // Elementos visuais
  val grid = new GridPane()
  val statusLabel = new Label("Turno das Pretas (Início do Jogo)")

  // ==========================================
  // 2. INÍCIO DA JANELA
  // ==========================================
  override def start(primaryStage: Stage): Unit = {
    val root = new BorderPane()
    grid.setAlignment(Pos.CENTER)

    // Barra de Ferramentas (Opções da TUI)
    val toolBar = new HBox(10)
    toolBar.setPadding(new Insets(10))
    toolBar.setAlignment(Pos.CENTER)

    val btnRestart = new Button("Reiniciar")
    btnRestart.setOnAction(_ => restartGame())

    val btnStopCapture = new Button("Parar de Capturar (Passar Turno)")
    btnStopCapture.setOnAction(_ => passTurn())

    toolBar.getChildren.addAll(btnRestart, btnStopCapture, statusLabel)

    root.setTop(toolBar)
    root.setCenter(grid)

    drawBoard() // Desenha o tabuleiro pela primeira vez

    primaryStage.setTitle("Kōnane - T8")
    primaryStage.setScene(new Scene(root, 600, 650))
    primaryStage.show()
  }

  // ==========================================
  // 3. DESENHAR O TABULEIRO (View)
  // ==========================================
  def drawBoard(): Unit = {
    grid.getChildren.clear() // Limpa o desenho anterior

    // Descobre quais são os destinos válidos se houver uma peça selecionada
    val validMoves = selectedPiece match {
      case Some(orig) => validDestinations(board, player, orig, open)
      case None => List.empty
    }

    for (r <- 0 until Rows; c <- 0 until Cols) {
      val coord = (r, c)
      val cellPane = new StackPane()

      // 3.1. O Quadrado (Fundo da casa)
      val square = new Rectangle(80, 80)
      square.setStroke(Color.BLACK)

      // Pintar a casa: Se for um destino válido, pinta de Verde (Dica visual!)
      if (validMoves.contains(coord)) {
        square.setFill(Color.LIGHTGREEN)
      } else {
        // Padrão de xadrez normal
        square.setFill(if ((r + c) % 2 == 0) Color.BURLYWOOD else Color.SADDLEBROWN)
      }

      cellPane.getChildren.add(square)

      // 3.2. A Pedra (Círculo)
      getStone(board)(coord) match {
        case Some(Stone.Black) =>
          val circle = new Circle(30, Color.BLACK)
          // Se for a peça selecionada, dar um destaque visual (ex: borda amarela)
          if (selectedPiece.contains(coord)) circle.setStroke(Color.YELLOW)
          cellPane.getChildren.add(circle)

        case Some(Stone.White) =>
          val circle = new Circle(30, Color.WHITE)
          if (selectedPiece.contains(coord)) circle.setStroke(Color.YELLOW)
          cellPane.getChildren.add(circle)

        case None => // Casa vazia, não desenha círculo
      }

      // 3.3. O Evento de Clique
      cellPane.setOnMouseClicked(_ => handleCellClick(r, c))

      // Adiciona a casa à grelha do JavaFX
      grid.add(cellPane, c, r)
    }
  }

  // ==========================================
  // 4. LÓGICA DE INTERAÇÃO (Controller)
  // ==========================================
  def handleCellClick(r: Int, c: Int): Unit = {
    val clickedCoord = (r, c)

    selectedPiece match {
      case None =>
        // Cenário A: Não temos peça selecionada. O jogador clicou numa peça dele?
        getStone(board)(clickedCoord) match {
          case Some(s) if s == player =>
            // Verifica se esta peça tem jogadas possíveis
            if (validDestinations(board, player, clickedCoord, open).nonEmpty) {
              selectedPiece = Some(clickedCoord) // Seleciona a peça!
              drawBoard() // Redesenha para mostrar a borda amarela e as casas verdes
            }
          case _ => // Clicou num espaço vazio ou na peça do adversário, ignorar.
        }

      case Some(orig) =>
        // Cenário B: Já temos uma peça selecionada.
        if (clickedCoord == orig) {
          // O jogador clicou na mesma peça para a DESMARCAR
          selectedPiece = None
          drawBoard()
        } else {
          // O jogador clicou noutra casa. É um destino válido?
          val destinos = validDestinations(board, player, orig, open)
          if (destinos.contains(clickedCoord)) {

            // É válido! Vamos usar as funções puras do T2 para calcular o novo estado
            val (newBoardOpt, newOpen) = T2.play(board, player, orig, clickedCoord, open)

            newBoardOpt match {
              case Some(nb) =>
                // ATUALIZA O ESTADO IMPERATIVO!
                board = nb
                open = newOpen

                // LÓGICA DE SALTO MÚLTIPLO:
                // Se a peça no novo destino ainda puder saltar, mantemo-la selecionada!
                if (validDestinations(board, player, clickedCoord, open).nonEmpty) {
                  selectedPiece = Some(clickedCoord)
                  statusLabel.setText(s"Podes continuar a saltar ou clicar 'Parar'.")
                } else {
                  // Se não puder saltar mais, passa o turno!
                  passTurn()
                }

                drawBoard()
              case None => // Erro de jogada (não devia acontecer porque filtramos os destinos antes)
            }
          }
        }
    }
  }

  // Função auxiliar para passar o turno
  def passTurn(): Unit = {
    selectedPiece = None
    player = player.opponent
    statusLabel.setText(s"Turno do jogador: $player")
    drawBoard()

    // Se for o turno das pretas, mandar o computador jogar
    if (player == Stone.Black) {
      // computerPlay() // Terias de implementar esta função na GUI
    }
  }

  def restartGame(): Unit = {
    board = T2.setupBoard(T2.initBoard(Rows, Cols), Rows, Cols)
    open = board.keys.toList.take(2) // Ajusta isto
    player = Stone.Black
    selectedPiece = None
    statusLabel.setText("Jogo Reiniciado. Turno das Pretas.")
    drawBoard()
  }
}

// O pontapé de saída da aplicação JavaFX
@main def runGUI(): Unit = {
  Application.launch(classOf[KonaneGUI])
}