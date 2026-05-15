case class MyRandom(seed: Long): //cria um valor de forma aleatoria
  def nextInt(v: Int): (Int, MyRandom) = 
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = ((newSeed >>> 16) % v).toInt.abs
    (n, nextRandom)