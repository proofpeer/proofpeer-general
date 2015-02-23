package proofpeer.general

object StringUtils {
  
  def isASCIILetter(c : Char) : Boolean = {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  }

  def isASCIILowerLetter(c : Char) : Boolean = {
    (c >= 'a' && c <= 'z') 
  }

  def isASCIIUpperLetter(c : Char) : Boolean = {
    (c >= 'A' && c <= 'Z')
  }

  def isASCIIDigit(c : Char) : Boolean = {
    (c >= '0' && c <= '9')
  }  

  def toLowerCase(codepoint : Int) : Int = {
    if (codepoint >= 'A' && codepoint <= 'Z') 
      codepoint + 32
    else
      codepoint
  }

  def toUpperCase(codepoint : Int) : Int = {
    if (codepoint >= 'a' && codepoint <= 'z')
      codepoint - 32
    else
      codepoint
  }
  
  def charCount(codepoint : Int) : Int = if (codepoint >= 0x10000) 2 else 1
  
  def codePointAt(s : String, index : Int) : Int = {
    val code : Int = s.charAt(index)
    if (code >= 0xD800 && code <= 0xDFFF) {
      val hi : Int = code
      val lo : Int = s.charAt(index+1)
      if (hi < 0xDC00 && lo >= 0xDC00 && lo <= 0xDFFF) {
        ((hi - 0xD800) << 10) + (lo - 0xDC00) + 0x10000 
      } else throw new RuntimeException("Invalid hi/lo surrogate pair: "+hi+"/"+lo)
    } else code
  }

  def codePoints(s : String) : Vector[Int] = {
    var index = 0
    var v : Vector[Int] = Vector()
    val len = s.length
    while (index < len) {
      val c = codePointAt(s, index)
      index = index + charCount(c)
      v = v :+ c
    }
    v
  }

  def fromCodePoints(cps : Vector[Int]) : String = {
    val builder = new StringBuilder(cps.size)
    for (codepoint <- cps) {
      if ((codepoint >= 0 && codepoint <= 0xD7FF) || (codepoint >= 0xE000 && codepoint <= 0xFFFF)) {
        builder.append(codepoint.toChar)
      } else if (codepoint >= 0x10000 && codepoint <= 0x10FFFF) {
        val cp = codepoint - 0x10000
        val hi = ((cp >> 10) & 0x3FF) + 0xD800
        val lo = (cp & 0x3FF) + 0xDC00
        builder.append(hi.toChar)
        builder.append(lo.toChar)
      } else throw new RuntimeException("invalid codepoint: " + codepoint)
    }
    builder.toString
  }

  def utf8Bytes(s : String) : Vector[Byte] = {
    var v : Vector[Byte] = Vector()
    val codes = codePoints(s)
    for (code <- codes) {
      if (code <= 0x7F) {
        v = v :+ code.toByte       
      } else if (code <= 0x7FF) {
        val b1 = 0xC0 | ((code >> 6) & 0x1F)
        val b2 = 0x80 | (code & 0x3F)
        v = v :+ b1.toByte
        v = v :+ b2.toByte        
      } else if (code <= 0xFFFF) {
        val b1 = 0xE0 | ((code >> 12) & 0xF)
        val b2 = 0x80 | ((code >> 6) & 0x3F)
        val b3 = 0x80 | (code & 0x3F)
        v = v :+ b1.toByte
        v = v :+ b2.toByte        
        v = v :+ b3.toByte                
      } else {
        val b1 = 0xF0 | ((code >> 18) & 0x7)
        val b2 = 0x80 | ((code >> 12) & 0x3F)
        val b3 = 0x80 | ((code >> 6) & 0x3F)
        val b4 = 0x80 | (code & 0x3F)
        v = v :+ b1.toByte
        v = v :+ b2.toByte        
        v = v :+ b3.toByte
        v = v :+ b4.toByte                        
      }
    }
    v
  }

  def utf8Bytes2Codepoints(bytes : Vector[Byte]) : Vector[Int] = {
    var codepoints : Array[Int] = new Array(bytes.size)
    var i = 0
    var j = 0
    val len = bytes.size
    while (i < len) {
      var c : Int = bytes(i) & 0xFF
      if ((c >> 7 & 1) == 0) { // 1 Byte Sequence
        i += 1
      } else if (((c >> 5) & 7) == 6) { // 2 Byte Sequence
        c = ((c & 0x1F) << 6) + (bytes(i + 1) & 0x3F)
        i += 2
      } else if (((c >> 4) & 15) == 14) { // 3 Byte Sequence
        c = ((c & 15) << 12) + ((bytes(i + 1) & 0x3F) << 6) + (bytes(i + 2) & 0x3F)
        i += 3
      } else if (((c >> 3) & 31) == 30) { // 4 Byte Sequence
        c = ((c & 7) << 18) + ((bytes(i + 1) & 0x3F) << 12) + ((bytes(i + 2) & 0x3F) << 6) + (bytes(i + 3) & 0x3F)
        i += 4
      } else throw new RuntimeException("bytes encoding is not UTF8")
      codepoints(j) = c
      j += 1
    }
    codepoints.take(j).toVector
  }

  def fromUtf8Bytes(bytes : Vector[Byte]) : String = {
    fromCodePoints(utf8Bytes2Codepoints(bytes))
  }
  
  def replaceAll(oldS : String, newS : String, content : String) : String = {
    val oldLen = oldS.length()
    if (oldLen == 0) return content
    val newLen = newS.length()
    var s = content
    var j = 0
    do {
      val i = s.indexOf(oldS, j)
      if (i >= j) {
        s = s.substring(0, i) + newS + s.substring(i + oldLen)
        j = i + newLen
      } else return s
    } while (true)
    s
  }
  
  def startsWith(s : String, prefix : String) : Boolean = {
    s.length() >= prefix.length() && s.substring(0, prefix.length()) == prefix
  }
  
  def split(s : String, sep : String) : Seq[String] = {
    var i = 0
    val len = s.length()
    val seplen = sep.length()
    var seq : Seq[String] = Seq()
    while (i < len) {
      val j = s.indexOf(sep, i)
      if (j < 0) {
        seq :+= s.substring(i, len)
        return seq
      } else {
        seq :+= s.substring(i, j)
        i = j + seplen
      }
    }
    return seq
  }
  
  def split_nonempty(s : String, sep : String) : Seq[String] = {
    split(s, sep).filter(!_.isEmpty())
  }

  def escapeHtml(text : String) : String = {
    val e0 = replaceAll("&", "&ampersand;", text)
    val e1 = replaceAll("\"", "&quot;", e0)
    val e2 = replaceAll("<", "&lt;", e1)
    val e3 = replaceAll(">", "&gt;", e2)
    e3
  }  

  def urlDecode(encoded : String) : String = {
    var bytes : Vector[Byte] = Vector()
    val len = encoded.length
    var i = 0
    while (i < len) {
      val c = encoded(i)
      if (c == '%') {
        val s = encoded.substring(i + 1, i + 3)
        algorithms.HexString.toBytes(s) match {
          case Some(Array(b)) =>
            bytes = bytes :+ b
          case _ =>
            throw new RuntimeException("invalid hex string: '" + s + "'")
        }
        i = i + 3
      } else {
        val b = c.toByte
        bytes = bytes :+ b
        i = i + 1
      }
    }
    fromUtf8Bytes(bytes)
  }

}

class CaseInsensitiveString private (content : String) {
  private val lowered = content.toLowerCase
  override def toString : String = content
  override def hashCode : Int = lowered.hashCode
  override def equals(that : Any) : Boolean = {
    that match {
      case s : CaseInsensitiveString =>
        lowered == s.lowered
      case s : String =>
        lowered == s.toLowerCase
      case _ => false
    }
  }
}

object CaseInsensitiveString {
  def apply(content : String) = new CaseInsensitiveString(content)
}

class CaseInsensitiveMap[T] private (private val mapping : Map[CaseInsensitiveString, T]) {
  def get(key : String) : Option[T] = {
    mapping.get(CaseInsensitiveString(key))
  }
  def put(key : String, value : T) : CaseInsensitiveMap[T] = {
    new CaseInsensitiveMap(mapping + (CaseInsensitiveString(key) -> value))
  }
  def toSeq : Seq[(String, T)] = {
    mapping.toSeq.map({case (k, v) => (k.toString, v)})
  }
  def keys : Set[String] = {
    mapping.keys.map(_.toString).toSet
  }
  def ++ (that : CaseInsensitiveMap[T]) : CaseInsensitiveMap[T] = {
    new CaseInsensitiveMap(mapping ++ that.mapping)
  }
  override def hashCode : Int = mapping.hashCode
  override def equals(that : Any) : Boolean = {
    that match {
      case s : CaseInsensitiveMap[_] => mapping.equals(s.mapping)
      case _ => false
    }
  }
}

object CaseInsensitiveMap {
  def apply[T]() : CaseInsensitiveMap[T] = new CaseInsensitiveMap(Map())
  def fromSeq[T](l : Seq[(String, T)]) : CaseInsensitiveMap[T] = {
    var mapping : Map[CaseInsensitiveString, T] = Map()
    for ((k, v) <- l) mapping = mapping + (CaseInsensitiveString(k) -> v)
    new CaseInsensitiveMap(mapping)
  }
}

