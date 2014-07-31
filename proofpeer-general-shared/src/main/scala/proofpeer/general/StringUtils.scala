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

class CaseInsensitiveMap[T] private (mapping : Map[CaseInsensitiveString, T]) {
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
}

object CaseInsensitiveMap {
  def apply[T]() : CaseInsensitiveMap[T] = new CaseInsensitiveMap(Map())
  def fromSeq[T](l : Seq[(String, T)]) : CaseInsensitiveMap[T] = {
    var mapping : Map[CaseInsensitiveString, T] = Map()
    for ((k, v) <- l) mapping = mapping + (CaseInsensitiveString(k) -> v)
    new CaseInsensitiveMap(mapping)
  }
}

