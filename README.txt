Para Jogar:

 1. Executar Run do main()

 2. Alterar eventuais defaults do jogo, como o tempo de jogada, o tamanho do board, a dificuldade do bot e a interface de jogo (GUI ou TUI).

 3. Jogar!


Sobre o jogo:

-A dificuldade facil faz com que o bot so faca 1 move por jogada, a dificuldade dificil deixa o bot fazer varias capturas por jogada.

-Apos escolher a opcao jogar, e possivel escolher jogar no TUI, no GUI ou em ambos, o gui so ira aparecer apos escolher uma opcao que ele faca parte.

-E possivel escolher o tempo de jogada.

-E possivel mudar o tamanho do board.

Sobre as classes:

O AIPlayer.scala tem os metodos para escolher moves de forma aleatoria.
O GameState.scala centraliza e partilha todo o estado do jogo entre o GUI e a TUI.
O GameLoop.scala coordena o fluxo do jogo.
O GUI implementa a interface gráfica.
O GameUtils  cria o valor random e o método para ver se o tempo acabou.
O main da run no jogo.
O GameEngine implementa toda a lógica do jogo.
O TUI implementa a interface de texto.
O GameDomain inicializa informacao default do jogo.