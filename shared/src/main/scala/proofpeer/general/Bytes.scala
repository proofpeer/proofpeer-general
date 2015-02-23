package proofpeer.general

class Bytes private (bytes : Vector[Byte]) {
  
  /** The length of this byte sequence. */
  def length : Int = bytes.size

  /** Same as this.length == 0. */
  def isEmpty = bytes.isEmpty

  /** Concatenates this and B. */
  def ++ (B : Bytes) = new Bytes(bytes ++ B.toVector)
  
  /** Returns the byte sequence that starts at position from and has length len. */
  def sub (from : Int, len : Int) : Bytes = new Bytes(bytes.drop(from).take(len))

  /** Returns the byte sequence that starts at position from. */
  def sub (from : Int) : Bytes = new Bytes(bytes.drop(from))

  /** Returns the i-th element of the byte sequence. */
  def apply (i : Int) : Byte = bytes(i)

  def toArray : Array[Byte] = bytes.toArray

  def toVector : Vector[Byte] = bytes

  override def hashCode : Int = bytes.hashCode
  
  override def equals(other : Any) : Boolean = {
    other match {
      case other : Bytes => bytes == other.toVector
      case _ => false
    }
  }
  
  /** Returns the 256 bit SHA-2 hash of this byte sequence. */
  def sha256 : Bytes = Bytes(algorithms.SHA256.compute(bytes : _*))

  /** Represents this byte sequence as hexadecimal string. */
  def asHex : String = algorithms.HexString.fromBytes(bytes : _*)

  /** Assumes that this byte sequence is the UTF-8 encoding of a string and returns this string. 
    * This method throws an exception if the byte sequence is not a valid UTF-8 encoding. 
    */
  def asString : String = StringUtils.fromUtf8Bytes(bytes)

  /** Decodes this byte sequence via Bytes.decode . */
  def decode : Any = {
    val (value, len) = Bytes.decode(this, 0)
    if (len != length) throw new RuntimeException("cannot decode, trailing " + (length - len) + " bytes")
    value
  }

  override def toString : String = bytes.toString
}

object Bytes {

  /** The empty byte sequence. */
  val empty : Bytes = new Bytes(Vector())

  /** Joins (concatenates) a sequence of byte sequences. */
  def join(seq : Bytes*) : Bytes = {
    if (seq.isEmpty) empty
    else {
      var t = seq.head
      for (s <- seq.tail) t = t ++ s
      t
    }
  }

  /** Constructs a byte sequence from a vector of bytes. */
  def apply(bytes : Vector[Byte]) : Bytes = {
    new Bytes(bytes)
  } 

  /** Constructs a byte sequence from an array of bytes. */
  def apply(bytes : Array[Byte]) : Bytes = {
    new Bytes(bytes.toVector)
  }

  /** Constructs a byte sequence from the specified individual bytes. */
  def apply(b : Byte*) : Bytes = {
    if (b.isEmpty) empty else Bytes(b.toVector)
  }

  private def error[T](method : String) : T = {
    throw new RuntimeException("Bytes." + method + " failed")
  }

  /** Extracts the byte from a 1-byte sequence. */
  def asByte(bytes : Bytes) : Byte = {
    if (bytes.length != 1) error("asByte")
    bytes(0)
  }

  /** Encodes a boolean using 1 byte. */
  def fromBoolean(x : Boolean) : Bytes = Bytes(if (x) 1.toByte else 0.toByte)

  /** Inverse to [[fromBoolean]]. */
  def asBoolean(b : Bytes) : Boolean = {
    if (b.length != 1) error("asBoolean")
    b(0) match {
      case 0 => false
      case 1 => true
      case _ => error("asBoolean")
    }
  }

  /** Encodes an integer using a 4-byte big endian encoding. */
  def fromInt(x : Int) : Bytes = {
    val b3 : Byte = ((x >> 24) & 0xFF).toByte
    val b2 : Byte = ((x >> 16) & 0xFF).toByte
    val b1 : Byte = ((x >> 8) & 0xFF).toByte
    val b0 : Byte = (x & 0xFF).toByte
    Bytes(Vector(b3, b2, b1, b0))
  }

  /** Inverse to [[fromInt]]. */
  def asInt(b : Bytes) : Int = {
    if (b.length != 4) error("asInt")
    val b3 = b(0) & 0xFF
    val b2 = b(1) & 0xFF
    val b1 = b(2) & 0xFF
    val b0 = b(3) & 0xFF
    (b3 << 24) + (b2 << 16) + (b1 << 8) + b0
  }

  /** Encodes a long using an 8-byte big endian encoding. */
  def fromLong(x : Long) : Bytes = {
    val b7 : Byte = ((x >> 56) & 0xFF).toByte
    val b6 : Byte = ((x >> 48) & 0xFF).toByte
    val b5 : Byte = ((x >> 40) & 0xFF).toByte
    val b4 : Byte = ((x >> 32) & 0xFF).toByte
    val b3 : Byte = ((x >> 24) & 0xFF).toByte
    val b2 : Byte = ((x >> 16) & 0xFF).toByte
    val b1 : Byte = ((x >> 8) & 0xFF).toByte
    val b0 : Byte = (x & 0xFF).toByte
    Bytes(Vector(b7, b6, b5, b4, b3, b2, b1, b0))
  }

  /** Inverse to [[fromLong]]. */
  def asLong(b : Bytes) : Long = {
    if (b.length != 8) error("asLong")
    val b7 : Long = b(0) & 0xFF
    val b6 : Long = b(1) & 0xFF
    val b5 : Long = b(2) & 0xFF
    val b4 : Long = b(3) & 0xFF
    val b3 : Long = b(4) & 0xFF
    val b2 : Long = b(5) & 0xFF
    val b1 : Long = b(6) & 0xFF
    val b0 : Long = b(7) & 0xFF
    (b7 << 56) + (b6 << 48) + (b5 << 40) + (b4 << 32) + (b3 << 24) + (b2 << 16) + (b1 << 8) + b0    
  }

  /** Encodes a char using a 2-byte big endian encoding. */
  def fromChar(x : Char) = fromShort(x.toShort)

  /** Inverse to [[fromChar]]. */
  def asChar(bytes : Bytes) = asShort(bytes).toChar

  /** Encodes a short using a 2-byte big endian encoding. */
  def fromShort(x : Short) : Bytes = {
    val b1 : Byte = ((x >> 8) & 0xFF).toByte
    val b0 : Byte = (x & 0xFF).toByte
    Bytes(Vector(b1, b0))    
  }

  /** Inverse to [[asShort]]. */
  def asShort(b : Bytes) : Short = {
    if (b.length != 2) error("asShort")
    val b1 = b(0) & 0xFF
    val b0 = b(1) & 0xFF
    ((b1 << 8) + b0).toShort    
  }

  /** Encodes a string as UTF-8 byte sequence. */
  def fromString(s : String) : Bytes = {
    Bytes(StringUtils.utf8Bytes(s))
  }

  /** Decodes a hexadecimal string into its byte sequence representation. */
  def fromHex(hex : String) : Bytes = {
    algorithms.HexString.toBytes(hex) match {
      case Some(bytes) => Bytes(bytes)
      case None => error("fromHex")
    }
  }

  private val CODE_LONG = 0
  private val CODE_STRING = 1
  private val CODE_BYTES = 2
  private val CODE_VECTOR = 3

  /** Encodes a value of generic type as Bytes. The values of the following types are encodable:
    *  - Byte (will be decoded as Long)
    *  - Short (will be decoded as Long)
    *  - Char (will be decoded as Long)
    *  - Int (will be decoded as Long)
    *  - Long 
    *  - String
    *  - Bytes
    *  - Seq[T], where T is encodable (will be decoded as Vector[Any])
    *  - Product, where the elements of the product are encodable (will be decoded as Vector[Any])
    * The encoding tries to be space-efficient without actually using any compression algorithms.
    * For example, the value Vector(1L, -5L, 11L) will be represented with a byte sequence of length 4, but the value
    * Vector(Long.MaxValue, Long.MinValue, Long.MaxValue) uses 28 bytes. 
    */
  def encode(x : Any) : Bytes = {
    x match {
      case x : Long => encodeLong(x)
      case x : Int => encodeLong(x)
      case x : Short => encodeLong(x)
      case x : Char => encodeLong(x)
      case x : Byte => encodeLong(x)
      case x : String => 
        val utf8 = fromString(x)
        if (utf8.length <= 62)
          join(Bytes(mkcode(CODE_STRING, utf8.length)), utf8)
        else
          join(Bytes(mkcode(CODE_STRING)), encodeNat(utf8.length - 63), utf8)
      case x : Bytes if x.length <= 62 => join(Bytes(mkcode(CODE_BYTES, x.length)), x)
      case x : Bytes => join(Bytes(mkcode(CODE_BYTES)), encodeNat(x.length - 63), x)
      case x : Seq[_] => encodeIterator(x.iterator)
      case x : Product => encodeIterator(x.productIterator)
      case _ => error("encode: " + x)
    }
  }

  private def encodeIterator(iterator : Iterator[Any]) : Bytes = {
    var b : List[Bytes] = List()
    var size : Long = 0
    for (elem <- iterator) {
      b = encode(elem) :: b
      size += 1
    }
    b = 
      if (size <= 62) 
        Bytes(mkcode(CODE_VECTOR, size.toInt)) :: b.reverse
      else 
        Bytes(mkcode(CODE_VECTOR)) :: encodeNat(size - 63) :: b.reverse
    join(b : _*)
  }

  private def mkcode(type_code : Int, value : Int = 63) : Byte = {
    ((type_code | (value << 2)) & 0xFF).toByte
  }

  private def byteInLong(x : Long, fromBit : Int) : Byte = {
    ((x >> fromBit) & 0xFF).toByte
  }

  private def encodeLong(x : Long) : Bytes = {
    if (x >= -28L && x <= 27L) {
      Bytes(mkcode(CODE_LONG, x.toInt + 28))
    } else {
      // encode using n bytes, where n is between 1 and 8
      if (x >= -128L && x <= 127L) {
        Bytes(mkcode(CODE_LONG, 56), 
          byteInLong(x, 0))
      } else if (x >= -32768L && x <= 32767L) {
        Bytes(mkcode(CODE_LONG, 57), 
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x >= -8388608L && x <= 8388607L) {
        Bytes(mkcode(CODE_LONG, 58), 
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x >= -2147483648L && x <= 2147483647L) {
        Bytes(mkcode(CODE_LONG, 59), 
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x >= -549755813888L && x <= 549755813887L) {
        Bytes(mkcode(CODE_LONG, 60), 
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x >= -140737488355328L && x <= 140737488355327L) {
        Bytes(mkcode(CODE_LONG, 61), 
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x >= -36028797018963968L && x <= 36028797018963967L) {
        Bytes(mkcode(CODE_LONG, 62), 
          byteInLong(x, 48),
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else {
        Bytes(mkcode(CODE_LONG, 63), 
          byteInLong(x, 56),
          byteInLong(x, 48),
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      }
    }
  }

  private def decodeLong(b : Bytes, from : Int) : (Long, Int) = {
    val value = (b(from) >> 2) & 63
    if (value <= 55)
      (value - 28, 1)
    else {
      val numBytes = value - 55
      var result : Long = 0
      for (i <- 1 to numBytes) {
        result = result << 8
        if (i == 1)
          result |= b(from + i).toLong
        else
          result |= (b(from + i) & 0xFF)
      }
      (result, numBytes + 1)
    }
  }

  /** Encodes a non-negative number as a sequence of bytes. Values less than 248 are encoded
    * as a single byte, otherwise values less than 2^(8*n) are encoded using (n + 1) bytes.
    */
  def encodeNat(x : Long) : Bytes = {
    if (x < 0) throw new RuntimeException("cannot encode negative number: " + x)
    else if (x <= 247L) {
      Bytes(byteInLong(x, 0))
    } else {
      // encode using n bytes, where n is between 1 and 8
      if (x < 256L) {
        Bytes(248.toByte, 
          byteInLong(x, 0))
      } else if (x < 65536L) {
        Bytes(249.toByte, 
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x < 16777216L) {
        Bytes(250.toByte, 
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x < 4294967296L) {
        Bytes(251.toByte, 
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x < 1099511627776L) {
        Bytes(252.toByte, 
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x < 281474976710656L) {
        Bytes(253.toByte, 
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else if (x < 72057594037927936L) {
        Bytes(254.toByte, 
          byteInLong(x, 48),
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      } else {
        Bytes(255.toByte, 
          byteInLong(x, 56),
          byteInLong(x, 48),
          byteInLong(x, 40),
          byteInLong(x, 32),
          byteInLong(x, 24),
          byteInLong(x, 16),
          byteInLong(x, 8),
          byteInLong(x, 0))
      }
    }
  }

  /** Inverse to [[encodeNat]]. Starts reading at position from.
    * @return (nat, len) where nat is the decoded non-negative number and len the number of read bytes.
    */
  def decodeNat(b : Bytes, from : Int) : (Long, Int) = {
    val value = b(from) & 0xFF
    if (value <= 247)
      (value, 1)
    else {
      val numBytes = value - 247
      var result : Long = 0
      for (i <- 1 to numBytes) {
        result = result << 8
        result |= (b(from + i) & 0xFF)
      }
      (result, numBytes + 1)
    }
  }

  /** Inverse to [[encode]]. Starts reading at position from.
    * @return (result, len) where result is the decoded result value, and len is the number of read bytes 
    */
  def decode(b : Bytes, from : Int) : (Any, Int) = {
    (b(from) & 0x3) match {
      case CODE_LONG => decodeLong(b, from)
      case CODE_STRING =>
        val size = (b(from) >> 2) & 63
        if (size <= 62) 
          (b.sub(from + 1, size).asString, size + 1)
        else {
          val (nat, natlen) = decodeNat(b, from + 1)
          val size = (nat + 63).toInt
          (b.sub(from + 1 + natlen, size).asString, 1 + natlen + size)
        }
      case CODE_BYTES => 
        val size = (b(from) >> 2) & 63
        if (size <= 62) 
          (b.sub(from + 1, size), size + 1)
        else {
          val (nat, natlen) = decodeNat(b, from + 1)
          val size = (nat + 63).toInt
          (b.sub(from + 1 + natlen, size), 1 + natlen + size)
        }
      case CODE_VECTOR =>
        var size = (b(from) >> 2) & 63
        var pos = from + 1
        if (size > 62) {
          val (nat, natlen) = decodeNat(b, from + 1)
          size = (nat + 63).toInt
          pos = from + 1 + natlen
        }
        var elems : Array[Any] = new Array(size)
        for (i <- 0 until size) {
          val (elem, len) = decode(b, pos)
          elems(i) = elem
          pos += len
        }
        (elems.toVector, pos - from)
      case t => error("decode, type = " + t)
    }
  }

  def decode(b : Bytes) : Any = {
    val (d, len) = decode(b, 0)
    if (len != b.length) throw new RuntimeException("Decoding Bytes resulted in left-over data.")
    d
  }

  /** Encodes the same kind of values as [[encode]] into strings. These strings are just treated
    * as sequences of 16-bit code units and do not necessarily contain valid Unicode text. 
    */
  def encodeToUCS2(value : Any) : String = {
    val bytes = encode(value)
    val len = bytes.length
    val builder = new StringBuilder(if (len % 2 == 0) len / 2 else (len + 1) / 2)
    var i = 0
    while (i + 1 < len) {
      val hi : Int = (bytes(i) & 0xFF)
      val lo : Int = (bytes(i + 1) & 0xFF)
      builder.append((hi * 256 + lo).toChar)
      i += 2
    }
    if (i < len) {
      val hi : Int = (bytes(i) & 0xFF)
      val lo : Int = 0
      builder.append((hi * 256 + lo).toChar)      
    }
    builder.toString
  }

  /** Inverse to [[encodeToUCS2]]. */
  def decodeFromUCS2(ucs2 : String) : Any = {
    val len = ucs2.length * 2
    val bytes = new Array[Byte](len)
    var i = 0
    while (i < len) {
      val c : Int = ucs2(i / 2)
      val hi : Byte = ((c >> 8) & 0xFF).toByte
      val lo : Byte = (c & 0xFF).toByte
      bytes(i) = hi
      bytes(i + 1) = lo
      i += 2
    }
    val (value, decodedLen) = decode(Bytes(bytes), 0)
    if (decodedLen == len || (decodedLen + 1 == len && bytes(decodedLen) == 0))
      value
    else 
      error("decodeFromUCS2("+ucs2+")")
  }

}