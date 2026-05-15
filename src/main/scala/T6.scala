object T6 {
  def isTimeOver(startTime: Long,limitMillis: Int): Boolean = {
    (System.currentTimeMillis() - startTime) > limitMillis
  }
}