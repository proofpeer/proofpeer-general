package proofpeer.general

trait Serializer[T, A] {

  def serialize(t : T) : A

  def deserialize(b : Any) : T

}

object VectorSerializer {

  def apply[T, A](serializer : Serializer[T, A]) : Serializer[Vector[T], Vector[A]] = 
    new Serializer[Vector[T], Vector[A]] {
      def serialize(v : Vector[T]) = v.map(x => serializer.serialize(x))
      def deserialize(b : Any) : Vector[T] = {
        b match {
          case v : Vector[Any] => v.map(x => serializer.deserialize(x))
          case _ => throw new RuntimeException("VectorSerializer: cannot deserialize " + b)
        }
      }
    }

}

object ListSerializer {

  def apply[T, A](serializer : Serializer[T, A]) : Serializer[List[T], Vector[A]] = 
    new Serializer[List[T], Vector[A]] {
      def serialize(v : List[T]) = v.map(x => serializer.serialize(x)).toVector
      def deserialize(b : Any) : List[T] = {
        b match {
          case v : Vector[Any] => v.map(x => serializer.deserialize(x)).toList
          case _ => throw new RuntimeException("ListSerializer: cannot deserialize " + b)
        }
      }
    }

}

object MapSerializer {

  def apply[K, V](keySerializer : Serializer[K, _], valueSerializer : Serializer[V, _]) : Serializer[Map[K, V], Vector[Any]] =
    new Serializer[Map[K, V], Vector[Any]] {
      def serialize(m : Map[K, V]) : Vector[Any] = {
        var result : List[Any] = List()
        for ((k, v) <- m) result = keySerializer.serialize(k) :: valueSerializer.serialize(v) :: result
        result.toVector
      }
      def deserialize(b : Any) : Map[K, V] = {
        b match {
          case v : Vector[Any] if v.size % 2 == 0 =>
            var m : Map[K, V] = Map()
            val size = v.size
            var i = 0
            while (i < size) {
              val key = keySerializer.deserialize(v(i))
              val value = valueSerializer.deserialize(v(i+1))
              m = m + (key -> value)
              i += 2
            }
            m
          case _ => throw new RuntimeException("MapSerializer: cannot deserialize " + b)
        }
      }
    }

}

object PairSerializer {

  def apply[U, V](sU : Serializer[U, _], sV : Serializer[V, _]) : Serializer[(U, V), Vector[Any]] = 
    new Serializer[(U, V), Vector[Any]] {
      def serialize(uv : (U, V)) : Vector[Any] = Vector(sU.serialize(uv._1), sV.serialize(uv._2))
      def deserialize(b : Any) : (U, V) = {
        b match {
          case Vector(u, v) => (sU.deserialize(u), sV.deserialize(v))
          case _ => throw new RuntimeException("PairSerializer: cannot deserialize " + b)
        }
      }
    }

}

object TripleSerializer {

  def apply[U, V, W](sU : Serializer[U, _], sV : Serializer[V, _], sW : Serializer[W, _]) : Serializer[(U, V, W), Vector[Any]] = 
    new Serializer[(U, V, W), Vector[Any]] {
      def serialize(uvw : (U, V, W)) : Vector[Any] = Vector(sU.serialize(uvw._1), sV.serialize(uvw._2), sW.serialize(uvw._3))
      def deserialize(b : Any) : (U, V, W) = {
        b match {
          case Vector(u, v, w) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w))
          case _ => throw new RuntimeException("TripleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object OptionSerializer {

  def apply[U, A](serializer : Serializer[U, A]) : Serializer[Option[U], Vector[A]] = {
    new Serializer[Option[U], Vector[A]] {
      def serialize(opt : Option[U]) : Vector[A] = {
        if (opt.isDefined) Vector(serializer.serialize(opt.get)) else Vector()
      }
      def deserialize(b : Any) : Option[U] = {
        b match {
          case Vector() => None
          case Vector(u) => Some(serializer.deserialize(u))
          case _ => throw new RuntimeException("OptionSerializer: cannot deserialize " + b)
        }
      }
    }
  }
}

object StringSerializer extends Serializer[String, String] {

  def serialize(s : String) : String = s

  def deserialize(b : Any) : String = {
    b match {
      case s : String => s
      case _ => throw new RuntimeException("StringSerializer: cannot deserialize " + b)
    }
  }

}

object BooleanSerializer extends Serializer[Boolean, Int] {

  def serialize(x : Boolean) : Int = if (x) 1 else 0

  def deserialize(b : Any) : Boolean = {
    b match {
      case x : Long if x == 0 || x == 1 => x == 1
      case _ => throw new RuntimeException("BooleanSerializer: cannot deserialize " + b)
    }
  }

}

object IntSerializer extends Serializer[Int, Int] {

  def serialize(x : Int) : Int = x

  def deserialize(b : Any) : Int = {
    b match {
      case x : Long => x.toInt
      case _ => throw new RuntimeException("IntSerializer: cannot deserialize " + b)
    }
  }

}

object LongSerializer extends Serializer[Long, Long] {

  def serialize(x : Long) : Long = x

  def deserialize(b : Any) : Long = {
    b match {
      case x : Long => x
      case _ => throw new RuntimeException("LongSerializer: cannot deserialize " + b)
    }
  }

}

object BigIntSerializer extends Serializer[BigInt, Any] {

  def serialize(x : BigInt) : Any = {
    if (x >= Long.MinValue && x <= Long.MaxValue) x.toLong else x.toString
  }

  def deserialize(b : Any) : BigInt = {
    b match {
      case l : Long => BigInt(l)
      case s : String => BigInt(s)
      case _ => throw new RuntimeException("BigIntSerializer: cannot deserialize " + b)
    }
  }

}

trait CaseClassSerializerBase[T] extends Serializer[T, Vector[Any]] {
  
  def decomposeAndSerialize(obj : T) : (Int, Vector[Any])
  
  def deserializeAndCompose(kind : Int, args : Vector[Any]) : T

  def serialize(obj : T) : Vector[Any] = {
    val (kind, args) = decomposeAndSerialize(obj)
    kind +: args
  }

  def deserialize(b : Any) : T = {
    b match {
      case v : Vector[Any] if !v.isEmpty && v(0).isInstanceOf[Long] =>
        val kind = v(0).asInstanceOf[Long].toInt
        deserializeAndCompose(kind, v.tail)
      case _ => throw new RuntimeException("Cannot deserialize: " + b)
    }
  }

}

/** Use this tool to generate code which implements a CaseClassSerializerBase[typename] */
class CaseClassSerializerTool(basename : String, cases : Vector[Any], typename : String = "Any") {

  /** You can make the encoding more efficient by overriding this method and detecting additional 
    * serializers which are guaranteed to have vectors as result. */
  protected def isVectorSerializer(serializerSpec : String) : Boolean = {
    val prefixes = Vector("VectorSerializer(", "ListSerializer(", "MapSerializer(", "PairSerializer(", 
      "TripleSerializer(", "OptionSerializer(")
    prefixes.exists(prefix => serializerSpec.startsWith(prefix))
  }

  private def normalizeCase(c : Any) : (String, Vector[String]) = {
    c match {
      case s : String => (s, Vector())
      case (s : String, t1 : String) => (s, Vector(t1))
      case (s : String, t1 : String, t2 : String) => (s, Vector(t1, t2))
      case (s : String, t1 : String, t2 : String, t3 : String) => (s, Vector(t1, t2, t3))
      case (s : String, args : Vector[Any]) => (s, args.map(_.toString))
      case _ => throw new RuntimeException("cannot normalize case: " + c)
    }
  }

  private val normalizedCases = cases.map(normalizeCase _)

  private def outputKinds() {
    println("  object Kind {")
    var kind = 0
    for ((name, _) <- normalizedCases) {
      println("    val " + name.toUpperCase + " = " + kind)
      if (kind <= 0) kind = - kind + 1 else kind = - kind
    }
    println("  }")
  }

  private def outputSerializers() {
    println("  object Serializers {")
    for ((name, args) <- normalizedCases) {
      args.size match {
        case 0 =>
        case 1 => 
          println("    val " + name.toUpperCase + " = " + args(0))
        case n =>
          var serializer = 
            n match {
              case 2 => "PairSerializer"
              case 3 => "TripleSerializer"
              case _ => throw new RuntimeException("cannot serialize tuples of arity " + n)
            }
          serializer = serializer + "(" + args(0)
          for (arg <- args.tail) serializer = serializer + "," + arg
          serializer = serializer + ")"
          println("    val " + name.toUpperCase + " = " + serializer)
      }
    }
    println("  }")
  }

  private def outputSerializer() {
    println("  def decomposeAndSerialize(obj : " + typename + ") : (Int, Vector[Any]) = {")
    println("    obj match {")
    val SPACES = "      "
    for ((name, args) <- normalizedCases) {
      val NAME = name.toUpperCase
      args.size match {
        case 0 =>
          println(SPACES + "case " + name + " =>")
          println(SPACES + "  (Kind." + NAME + ", Vector())")
        case 1 if isVectorSerializer(args(0)) =>
          println(SPACES + "case " + name + "(x) =>")
          println(SPACES + "  (Kind." + NAME + ", Serializers." + NAME + ".serialize(x))")
        case 1 =>
          println(SPACES + "case " + name + "(x) =>")
          println(SPACES + "  (Kind." + NAME + ", Vector(Serializers." + NAME + ".serialize(x)))")          
        case _ =>
          val arg = args(0)
          println(SPACES + "case t : " + name + " =>")
          println(SPACES + "  (Kind." + NAME + ", Serializers." + NAME + ".serialize(" + name + ".unapply(t).get))")
      }
    }
    println(SPACES + "case _ => throw new RuntimeException(\"" + basename + ": cannot serialize \" + obj)")
    println("    }")
    println("  }")
  }

  private def outputDeserializer() {
    println("  def deserializeAndCompose(kind : Int, args : Vector[Any]) : " + typename + " = {")
    println("    kind match {")
    val SPACES = "      "
    for ((name, args) <- normalizedCases) {
      val NAME = name.toUpperCase
      args.size match {
        case 0 =>
          println(SPACES + "case Kind." + NAME + " if args.size == 0 => ")
          println(SPACES + "  " + name)
        case 1 if isVectorSerializer(args(0)) =>
          println(SPACES + "case Kind." + NAME + " => ")
          println(SPACES + "  " + name + "(Serializers." + NAME + ".deserialize(args))")
        case 1 =>
          println(SPACES + "case Kind." + NAME + " if args.size == 1 => ")
          println(SPACES + "  " + name + "(Serializers." + NAME + ".deserialize(args(0)))")
        case _ =>
          println(SPACES + "case Kind." + NAME + " => ")
          println(SPACES + "  " + name + ".tupled(Serializers." + NAME + ".deserialize(args))")
      }
    }
    println(SPACES + "case _ => throw new RuntimeException(\"" + basename + ": cannot deserialize \" + (kind, args))")    
    println("    }")
    println("  }")
  }

  def output() {
    println("object " + basename + " extends CaseClassSerializerBase[" + typename + "] {")
    println("")
    outputKinds()
    println("")
    outputSerializers()
    println("")
    outputSerializer()
    println("")
    outputDeserializer()
    println("")
    println("}")
  }

  def main(args : Array[String]) {
    output()
  }

}



