import scala.annotation.tailrec
import scala.io.StdIn
import T1.*
import T2.*
import T2.given
import T3.*
import T4.*
import T5.*
import T6.*

object Main {

  def initialOpenCoords(full: Board, setup: Board): List[Coord2D] =
    full.keys.filterNot(setup.contains).toList

  def hasAnyMove(board: Board, player: Stone, open: List[Coord2D]): Boolean =
    board.exists { case (coord, stone) =>
      stone == player && validDestinations(board, player, coord, open).nonEmpty
    }

  def parseCoord(s: String): Option[Coord2D] = {
    s.trim.split("\\s+") match {
      case Array(r, c) =>
        try { Some((r.toInt, c.toInt)) }
        catch { case _: NumberFormatException => None }
      case _ => None
    }
  }

  def showValidMoves(board: Board, player: Stone, open: List[Coord2D]): Unit = {
    val moves = board.toList
      .collect { case (c, s) if s == player => c -> validDestinations(board, player, c, open) }
      .filter { case (_, dests) => dests.nonEmpty }
      .sortBy(_._1)
    println(s"\n  Moves validos para $player:")
    moves.foreach { case (from, dests) =>
      println(s"    De (${from._1},${from._2})  ->  ${dests.map(d => s"(${d._1},${d._2})").mkString("  ")}")
    }
  }

  @tailrec
  def mainMenu(config: GameConfig): Unit = {
    println("\n" + "=" * 30)
    println("        KŌNANE - MENU")
    println("=" * 30)
    println(s"1. Jogar")
    println(s"2. Alterar dimensões do tabuleiro (Atual: ${config.rows}x${config.cols})")
    println(s"3. Definir tempo por jogada (Atual: ${config.timeLimitMillis / 1000}s)")
    println(s"4. Nível de dificuldade (Atual: Nível ${config.difficulty})")
    println(s"0. Sair")
    println("=" * 30)
    print("Escolha uma opção: ")

    StdIn.readLine().trim match {
      case "1" =>
        // Iniciar o jogo com as configurações atuais
        println("\nA iniciar o jogo...")
        val rand = MyRandom(System.currentTimeMillis())
        val fullBoard = T2.initBoard(config.rows, config.cols)
        val board = T2.setupBoard(fullBoard, config.rows, config.cols)
        val open = initialOpenCoords(fullBoard, board)

        println("Tabuleiro Inicial:")
        println(T4.renderBoard(board, config.rows, config.cols))

        // Chamamos o gameLoop (TEREMOS DE AJUSTAR PARA RECEBER AS CONFIGS tempo/dificuldade se necessário)
        gameLoop(board, open, T2.Stone.Black, rand, Nil, config)

        // Quando o jogo terminar, volta ao menu!
        mainMenu(config)

      case "2" =>
        print("Novas linhas: ")
        val r = StdIn.readLine().toIntOption.getOrElse(config.rows)
        print("Novas colunas: ")
        val c = StdIn.readLine().toIntOption.getOrElse(config.cols)
        println(s"Dimensões atualizadas para ${r}x${c}.")
        // Chama o menu novamente com a configuração atualizada!
        mainMenu(config.copy(rows = r, cols = c))

      case "3" =>
        print("Novo tempo limite (segundos): ")
        val t = StdIn.readLine().toIntOption.getOrElse(config.timeLimitMillis / 1000)
        // Chama o menu com o novo tempo (convertido para milissegundos)
        mainMenu(config.copy(timeLimitMillis = t * 1000))

      case "4" =>
        //POR ENQUANTO NÃO FAZ NADA, AINDA NÃO HÁ MODOS
        print("Nova dificuldade (1-Fácil, 2-Difícil): ")
        val d = StdIn.readLine().toIntOption.getOrElse(config.difficulty)
        mainMenu(config.copy(difficulty = d))

      case "0" =>
        println("Obrigado por jogar Kōnane! Adeus.")

      case _ =>
        println("Opção inválida. Tenta novamente.")
        mainMenu(config)
    }
  }

  @tailrec
  def humanTurn(board: Board, open: List[Coord2D], player: Stone, movedFrom: Option[Coord2D],config: GameConfig): (Board, List[Coord2D]) = {

    showValidMoves(board, player, open)

    movedFrom match {
      case Some(locked) => {
        println(T4.renderBoard(board, config.rows, config.cols))
        println(s"\n  Peça esta em (${locked._1},${locked._2}).")
        println("  Coloca 'row col' para andar, ou clica ENTER para acabar o turno:")
        val line = StdIn.readLine().trim
        if (line.isEmpty) {
          (board, open)
        } else {
          parseCoord(line) match {
            case Some(to) => {
              val (newBoardOpt, newOpen) = play(board, player, locked, to, open)
              newBoardOpt match {
                case Some(nb) => {
                  println(s"  Saltou para (${to._1},${to._2}).")
                  if (validDestinations(nb, player, to, newOpen).nonEmpty)
                    humanTurn(nb, newOpen, player, Some(to),config)
                  else {
                    println("  Turno acabou.")
                    (nb, newOpen)
                  }
                }
                case None => {
                  println("  Move invalido.")
                  humanTurn(board, open, player, movedFrom,config)
                }
              }
            }
            case None => {
              println("  Mau input.")
              humanTurn(board, open, player, movedFrom,config)
            }
          }
        }
      }

      case None => {
        showValidMoves(board, player, open)
        println(s"\n  Your turn ($player). Enter: fromRow fromCol toRow toCol")
        val line = StdIn.readLine().trim
        line.split("\\s+") match {
          case Array(r1, c1, r2, c2) => {
            (parseCoord(s"$r1 $c1"), parseCoord(s"$r2 $c2")) match {
              case (Some(from), Some(to)) => {
                val (newBoardOpt, newOpen) = play(board, player, from, to, open)
                newBoardOpt match {
                  case Some(nb) => {
                    println(s"  Moved (${from._1},${from._2}) -> (${to._1},${to._2}).")
                    if (validDestinations(nb, player, to, newOpen).nonEmpty) {
                      println("  ENTER para parar")
                      humanTurn(nb, newOpen, player, Some(to),config)
                    } else {
                      (nb, newOpen)
                    }
                  }
                  case None => {
                    println("  Move invalido.")
                    humanTurn(board, open, player, None, config)
                  }
                }
              }
              case _ => {
                println("  Mau input - expectado 'r1 c1 r2 c2'.")
                humanTurn(board, open, player, None,config)
              }
            }
          }
          case _ => {
            println("  Mau input - expectado 'r1 c1 r2 c2'.")
            humanTurn(board, open, player, None,config)
          }
        }
      }
    }
  }

  def computerTurn(board: Board, open: List[Coord2D], rand: MyRandom, config: GameConfig): (Board, List[Coord2D], MyRandom) = {
    if (config.difficulty == 1) {
      val (newBoardOpt, newRand, newOpen, movedTo) = playRandomly(board, rand, Stone.Black, open, randomMove)

      newBoardOpt match {
        case None => (board, open, newRand)
        case Some(nb) => {
          println(s"  Black moveu para $movedTo.")
          (nb, newOpen, newRand)
        }
      }
    }else{
      println("Níveis de dificuldade ainda não implementados. A jogar modo aleatório")
      val (newBoardOpt, newRand, newOpen, movedTo) = playRandomly(board, rand, Stone.Black, open, randomMove)

      newBoardOpt match {
        case None => (board, open, newRand)
        case Some(nb) => {
          println(s"  Black moveu para $movedTo.")
          (nb, newOpen, newRand)
        }
      }
    }
  }


    type GameState = (Board, List[Coord2D], Stone)

    @tailrec
    def gameLoop(board: Board, open: List[Coord2D], player: Stone, rand: MyRandom, history: List[GameState], config: GameConfig): Unit = {

      // O TIMER RECOMEÇA AQUI (Sempre que a função é chamada, seja turno novo ou Undo)
      val startTime = System.currentTimeMillis()

      println(s"\n==============================")
      println(s"  Turno do $player")
      println(s"  Tempo Limite: ${config.timeLimitMillis / 1000}s")
      println(s"==============================")
      println(renderBoard(board, config.rows, config.cols))

      // 1. Verificar se já existe um vencedor (T5)
      T5.checkWinner(board, player, open) match {
        case Some(vencedor) =>
          println(s"\n*** JOGO TERMINADO! Vencedor: $vencedor ***\n")

        case None =>
          println(s"Introduza coordenadas (origem destino, ex: '2 0 2 2') ou 'undo':")
          val input = StdIn.readLine().trim.toLowerCase

          // --- LÓGICA DE UNDO ---
          if (input == "undo") {
            history match {
              case Nil =>
                println("-> Não há jogadas para desfazer!")
                gameLoop(board, open, player, rand, Nil, config) // Recomeça o turno e o timer

              case (oldBoard, oldOpen, oldPlayer) :: rest =>
                println("-> Undo realizado! Voltando ao estado anterior...")
                gameLoop(oldBoard, oldOpen, oldPlayer, rand, rest, config) // Recomeça no estado antigo
            }
          }
          // --- LÓGICA DE JOGADA ---
          else {
            // Registar o tempo assim que o input é recebido
            val timeExpired = T6.isTimeOver(startTime, config.timeLimitMillis)

            if (timeExpired) {
              println("\n[!] TEMPO LIMITE ULTRAPASSADO!")



              // OPÇÃO A: O JOGADOR PERDE O TURNO (Passa para o adversário)
              println("-> Perdeste o teu turno.")
              gameLoop(board, open, player.opponent, rand, history, config)

              /* // OPÇÃO B: O JOGADOR PERDE O JOGO IMEDIATAMENTE
               println(s"-> O jogador $player foi desqualificado por tempo!")
               println(s"*** VENCEDOR: ${player.opponent} ***")
               // Fim da recursão
            */

            }
            else {
              // Processar a jogada
              // 2. GUARDAR O HISTÓRICO: Guardamos o estado de agora antes de ele ser modificado
              val currentHistory = (board, open, player) :: history

              // 3. USAR AS TUAS FUNÇÕES ORIGINAIS
              player match {
                case Stone.White =>
                  // A tua função humanTurn faz o trabalho
                  val (nb, newOpen) = humanTurn(board, open, Stone.White, None, config)
                  // Passamos o currentHistory para a próxima iteração
                  gameLoop(nb, newOpen, Stone.Black, rand, currentHistory, config)

                case Stone.Black =>
                  // A tua função computerTurn faz o trabalho
                  val (nb, newOpen, newRand) = computerTurn(board, open, rand, config)
                  // Passamos o currentHistory para a próxima iteração
                  gameLoop(nb, newOpen, Stone.White, newRand, currentHistory, config)
              }
            }
          }
      }
    }

    case class GameConfig(
                           rows: Int = 6,
                           cols: Int = 6,
                           timeLimitMillis: Int = 10000,
                           difficulty: Int = 1 //  1 = Fácil (Random), 2 = Difícil
                         )

    @main def run(): Unit = {
      println("Bem-vindo ao Kōnane!")
      val defaultConfig = GameConfig() // Usa os valores por defeito (6x6, 10s, nível 1)
      mainMenu(defaultConfig)
    }
  }