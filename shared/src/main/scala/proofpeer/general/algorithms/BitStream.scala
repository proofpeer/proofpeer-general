package proofpeer.general.algorithms 

trait BitStream {

  /** @return true iff the next read would return -1 */
  def done : Boolean

  /** Reads a bit from the stream.
    * @return -1 if the end of the stream has been reached, or the bit (either 0 or 1); once
    *   -1 has been returned, all subsequent calls must return -1 as well.
    *   
    */
  def read() : Int

}

object BitStream {

  val empty = 
    new BitStream { 
      def done = true
      def read() = -1 
    }

  def repeat(bit : Boolean, n : Long) : BitStream = {
    new BitStream {
      var countdown = n
      val result = if (bit) 1 else 0
      def done = countdown <= 0
      def read() = 
        if (countdown <= 0)
          -1
        else {
          countdown -= 1
          result
        }
    }
  }

  def ones(n : Long) = repeat(true, n)

  def zeros(n : Long) = repeat(false, n)

  def one = ones(1)

  def zero = zeros(1)

  def join(streams : BitStream*) : BitStream = {
    if (streams.isEmpty) 
      empty
    else {
      new BitStream {
        var head = streams.head
        var tail = streams.tail
        def skipDone() {
          while (head.done && !tail.isEmpty) {
            head = tail.head
            tail = tail.tail
          }
        }
        def done : Boolean = {
          skipDone()
          head.done
        }
        def read() : Int = {
          skipDone()
          head.read()
        }
      }
    }
  }

  trait Endian {

    def fromLong(x : Long, length : Int) : BitStream 

    def toLong(bits : BitStream, maxLength : Int) : Long

    def fromLong(x : Long) : BitStream = fromLong(x, 64)

    def toLong(bits : BitStream) : Long  = toLong(bits, 64) 

    def fromInt(x : Int) = fromLong(x, 32)

    def toInt(bits : BitStream) : Int = toLong(bits, 32).toInt

    def fromShort(x : Short) = fromLong(x, 16)

    def toShort(bits : BitStream) : Short = toLong(bits, 16).toShort

    def fromByte(x : Byte) = fromLong(x, 8)

    def toByte(bits : BitStream) : Byte = toLong(bits, 8).toByte

    def fromBytes(bytes : Byte*) : BitStream = {
      join(bytes.map(fromByte _) : _*)
    }

    def toBytes(bits : BitStream, maxNumBytes : Int) : Array[Byte] = {
      val bytes = new Array[Byte](maxNumBytes)
      for (i <- 0 until maxNumBytes) {
        if (bits.done) {
          val result = new Array[Byte](i)
          for (j <- 0 until i) result(j) = bytes(j)
          return result 
        }
        bytes(i) = toByte(bits)
      }
      bytes
    }

  }

  object LittleEndian extends Endian {

    def fromLong(x : Long, length : Int) : BitStream = {  
      new BitStream() {
        var len : Int = length
        def done = len <= 0
        def read() : Int = {
          if (len <= 0) 
            -1
          else {
            len = len - 1
            if ((x & (1L << len)) != 0) 1 else 0
          }
        }
      }
    }

    def toLong(bits : BitStream, maxLength : Int) : Long = {
      var value : Long = 0
      var i : Int = 0
      var max_i = if (maxLength < 64) maxLength - 1 else 63
      for (i <- 0 to max_i) {
        if (bits.read() == 1) value |= (1L << i)
      }
      value
    }

  }

  object BigEndian extends Endian {

    def fromLong(x : Long, length : Int) : BitStream = { 
      if (length == 0) return empty 
      new BitStream() {
        var mask : Long = if (length <= 0) 0 else 1L << (length - 1)
        def done = mask == 0
        def read() : Int = {
          if (mask == 0)
            -1
          else {
            val bit = x & mask
            mask = mask >>> 1
            if (bit != 0) 1 else 0
          }
        }
      }
    }

    def toLong(bits : BitStream, maxLength : Int) : Long = {
      var value : Long = 0
      var i : Int = 0
      var max_i = if (maxLength < 64) maxLength - 1 else 63
      for (i <- max_i to 0 by -1) {
        if (bits.read() == 1) value |= (1L << i)
      }
      value
    }

  }

}