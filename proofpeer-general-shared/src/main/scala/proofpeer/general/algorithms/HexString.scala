package proofpeer.general.algorithms 

object HexString {

  private val hexArray : Array[Char] = 
    Array('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f')

  def fromBytes(bytes : Byte*) : String = {
    val len = bytes.size
    val hexChars : Array[Char] = new Array(len * 2)
    var i = 0
    for (b <- bytes) {
      val v = b & 0xFF
      hexChars(i) = hexArray(v >>> 4)
      hexChars(i + 1) = hexArray(v & 0xF)
      i += 2
    }
    new String(hexChars)
  }

  private def valueOfHexChar(c : Char) : Option[Byte] = {
    if (c >= '0' && c <= '9') 
      Some((c - '0').toByte)
    else if (c >= 'a' && c <= 'f')
      Some(((c - 'a') + 10).toByte)
    else if (c >= 'A' && c <= 'F')
      Some(((c - 'A') + 10).toByte)
    else
      None
  }

  def toBytes(hexStr : String) : Option[Array[Byte]] = {
    val len = hexStr.length
    if (len % 2 != 0) return None
    val bytes = new Array[Byte](len / 2)
    for (i <- 0 until len / 2) {
      (valueOfHexChar(hexStr(2*i)), valueOfHexChar(hexStr(2*i+1))) match {
        case (Some(hi), Some(lo)) =>
          bytes(i) = ((hi << 4) | lo).toByte
        case _ =>
          return None
      }
    }
    Some(bytes)
  }

}