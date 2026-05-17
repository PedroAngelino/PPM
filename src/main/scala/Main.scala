import scala.annotation.tailrec
import scala.io.StdIn
import T1.*
import T2.*
import T2.given
import T3.*
import T4.*
import T5.*
import T6.*

object Main:

  // ===================================================
  // INICIAR JOGO
  // ===================================================
  def startGame(): Unit =
    val full = T2.initBoard(GameState.cfgRows, GameState.cfgCols)
    GameState.board         = T2.setupBoard(full, GameState.cfgRows, GameState.cfgCols)
    GameState.open          = full.keys.filterNot(GameState.board.contains).toList
    GameState.currentPlayer = Stone.White
    GameState.rand          = MyRandom(42L)
    GameState.history       = Nil
    GameState.gameActive    = true
    GameState.onStateChanged()

  // ===================================================
  // LOGICA DE TURNO (partilhada TUI + GUI)
  // ===================================================
  def finishWhiteTurn(): Unit =
    GameState.currentPlayer = Stone.Black
    GameState.onStateChanged()
    T5.checkWinner(GameState.board, Stone.Black, GameState.open) match
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
        T3.playRandomly(GameState.board, GameState.rand, Stone.Black, GameState.open, T1.randomMove)
      GameState.rand = newRand
      newBoardOpt.foreach { nb => GameState.board = nb; GameState.open = newOpen }
      println(s"  PC jogou para ${dest.map(d => s"(${d._1},${d._2})").getOrElse("?")}")
    else
      val best = GameState.board.toList.collect { case (c, Stone.Black) => c }.flatMap { from =>
        T2.validDestinations(GameState.board, Stone.Black, from, GameState.open).flatMap { to =>
          T2.play(GameState.board, Stone.Black, from, to, GameState.open) match
            case (Some(nb), newOpen) => Some((GameState.board.size - nb.size, from, to, nb, newOpen))
            case _ => None
        }
      }
      if best.nonEmpty then
        val (_, from, to, nb, newOpen) = best.maxBy(_._1)
        GameState.board = nb; GameState.open = newOpen
        println(s"  PC jogou (${from._1},${from._2}) -> (${to._1},${to._2})")

    GameState.currentPlayer = Stone.White
    GameState.onStateChanged()
    T5.checkWinner(GameState.board, Stone.White, GameState.open) match
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
        GameState.board         = oldBoard
        GameState.open          = oldOpen
        GameState.currentPlayer = oldPlayer
        GameState.history       = rest
        println("  Undo feito.")
        GameState.onStateChanged()

  // ===================================================
  // REGISTAR CALLBACKS DA GUI → Main
  // Chamado assim que T8.instance estiver disponivel
  // ===================================================
  def registerGuiCallbacks(): Unit =
    val app = T8.instance

    T8.onHumanMove = (from, to) =>
      if !GameState.gameActive || GameState.currentPlayer != Stone.White then return
      // Guardar historico apenas no primeiro salto
      if !GameState.history.headOption.exists(_._3 == Stone.White) then
        GameState.history = (GameState.board, GameState.open, Stone.White) :: GameState.history
      T2.play(GameState.board, Stone.White, from, to, GameState.open) match
        case (Some(nb), newOpen) =>
          GameState.board = nb; GameState.open = newOpen
          val moreDests = T2.validDestinations(nb, Stone.White, to, newOpen)
          GameState.onStateChanged()
          app.notifyJumpDone(to, moreDests)
          if moreDests.isEmpty then finishWhiteTurn()
        case _ =>
          GameState.onStateChanged()

    T8.onEndTurn  = () => finishWhiteTurn()
    T8.onSkipTurn = () => finishWhiteTurn()
    T8.onUndo     = () => doUndo()
    T8.onRestart  = () => { startGame(); println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols)) }

  // ===================================================
  // TUI
  // ===================================================
  def parseCoord(s: String): Option[Coord2D] =
    s.trim.split("\\s+") match
      case Array(r, c) =>
        try Some((r.toInt, c.toInt))
        catch case _: NumberFormatException => None
      case _ => None

  def showValidMoves(): Unit =
    val moves = GameState.board.toList
      .collect { case (c, s) if s == GameState.currentPlayer =>
        c -> T2.validDestinations(GameState.board, GameState.currentPlayer, c, GameState.open) }
      .filter { case (_, dests) => dests.nonEmpty }
      .sortBy(_._1)
    println(s"\n  Moves validos para ${GameState.currentPlayer}:")
    moves.foreach { case (from, dests) =>
      println(s"    De (${from._1},${from._2})  ->  ${dests.map(d => s"(${d._1},${d._2})").mkString("  ")}")
    }

  @tailrec
  def humanTurnTUI(movedFrom: Option[Coord2D]): Unit =
    val startTime = System.currentTimeMillis()
    showValidMoves()
    movedFrom match
      case Some(locked) =>
        println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
        println(s"  Peca em (${locked._1},${locked._2}). 'row col' para andar ou ENTER para acabar:")
        val line = StdIn.readLine().trim
        if T6.isTimeOver(startTime, GameState.cfgTime) then
          println("[!] Tempo esgotado! Turno perdido.")
          finishWhiteTurn()
        else if line.isEmpty then
          finishWhiteTurn()
        else
          parseCoord(line) match
            case Some(to) =>
              T2.play(GameState.board, Stone.White, locked, to, GameState.open) match
                case (Some(nb), newOpen) =>
                  GameState.board = nb; GameState.open = newOpen
                  GameState.onStateChanged()
                  if T2.validDestinations(nb, Stone.White, to, newOpen).nonEmpty then humanTurnTUI(Some(to))
                  else finishWhiteTurn()
                case _ =>
                  println("  Move invalido."); humanTurnTUI(movedFrom)
            case None =>
              println("  Mau input."); humanTurnTUI(movedFrom)

      case None =>
        println(s"\n  Turno de ${GameState.currentPlayer}. Introduz: fromRow fromCol toRow toCol ou 'undo'")
        val line = StdIn.readLine().trim.toLowerCase
        if T6.isTimeOver(startTime, GameState.cfgTime) then
          println("[!] Tempo esgotado! Turno perdido.")
          finishWhiteTurn()
        else if line == "undo" then
          doUndo(); humanTurnTUI(None)
        else
          line.split("\\s+") match
            case Array(r1, c1, r2, c2) =>
              (parseCoord(s"$r1 $c1"), parseCoord(s"$r2 $c2")) match
                case (Some(from), Some(to)) =>
                  GameState.history = (GameState.board, GameState.open, Stone.White) :: GameState.history
                  T2.play(GameState.board, Stone.White, from, to, GameState.open) match
                    case (Some(nb), newOpen) =>
                      GameState.board = nb; GameState.open = newOpen
                      GameState.onStateChanged()
                      if T2.validDestinations(nb, Stone.White, to, newOpen).nonEmpty then humanTurnTUI(Some(to))
                      else finishWhiteTurn()
                    case _ =>
                      println("  Move invalido."); humanTurnTUI(None)
                case _ =>
                  println("  Mau input."); humanTurnTUI(None)
            case _ =>
              println("  Mau input."); humanTurnTUI(None)

  @tailrec
  def gameLoopTUI(): Unit =
    if !GameState.gameActive then return
    println(renderBoard(GameState.board, GameState.cfgRows, GameState.cfgCols))
    T5.checkWinner(GameState.board, GameState.currentPlayer, GameState.open) match
      case Some(w) =>
        println(s"\n*** JOGO TERMINADO! Vencedor: $w ***\n")
        GameState.gameActive = false
        GameState.onStateChanged()
      case None =>
        if GameState.currentPlayer == Stone.White then
          humanTurnTUI(None)
        else
          doComputerMove()
        gameLoopTUI()

  @tailrec
  def mainMenu(): Unit =
    println("\n" + "=" * 30)
    println("        KONANE - MENU")
    println("=" * 30)
    println(s"1. Jogar")
    println(s"2. Nivel de dificuldade (Atual: Nivel ${GameState.cfgDiff})")
    println(s"3. Definir tempo por jogada (Atual: ${GameState.cfgTime / 1000}s)")
    println(s"0. Sair")
    println("=" * 30)
    print("Escolha uma opcao: ")
    StdIn.readLine().trim match
      case "1" =>
        startGame()
        gameLoopTUI()
        mainMenu()
      case "2" =>
        print("Nova dificuldade (1-Facil, 2-Dificil): ")
        GameState.cfgDiff = StdIn.readLine().toIntOption.getOrElse(GameState.cfgDiff)
        mainMenu()
      case "3" =>
        print("Novo tempo limite (segundos): ")
        GameState.cfgTime = StdIn.readLine().toIntOption.getOrElse(GameState.cfgTime / 1000) * 1000
        mainMenu()
      case "0" =>
        println("Adeus!")
      case _ =>
        println("Opcao invalida.")
        mainMenu()

  @main def run(): Unit =
    println("Bem-vindo ao Konane!")
    // Lançar a GUI numa thread separada
    T8.launch()
    // Aguardar a GUI inicializar e T8.instance ficar disponivel
    while T8.instance == null do Thread.sleep(100)
    // Registar os callbacks agora que a instancia existe
    registerGuiCallbacks()
    // TUI corre na thread principal
    mainMenu()