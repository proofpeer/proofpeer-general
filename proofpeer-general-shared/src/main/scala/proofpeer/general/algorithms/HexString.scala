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

}