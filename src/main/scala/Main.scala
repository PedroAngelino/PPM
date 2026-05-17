import GUI.*
import TUI.*
import GameLoop.*

object Main:

  @main def run(): Unit =
    println("Bem-vindo ao Konane!")
    // Lançar a GUI numa thread separada
    GUI.launch()
    // Aguardar a GUI inicializar e T8.instance ficar disponivel
    while GUI.instance == null do Thread.sleep(100)
    // Registar os callbacks agora que a instancia existe
    GameLoop.registerGuiCallbacks()
    // TUI corre na thread principal
    TUI.mainMenu()