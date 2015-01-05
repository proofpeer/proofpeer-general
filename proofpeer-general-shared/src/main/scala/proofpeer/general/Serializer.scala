package proofpeer.general

trait Serializer[T, A] {

  def serialize(t : T) : A

  def deserialize(b : Any) : T

}

trait AtomicSerializer[T] extends Serializer[T, Any]

trait CompoundSerializer[T] extends Serializer[T, Vector[Any]]

object VectorSerializer {

  def apply[T](serializer : Serializer[T, _]) : CompoundSerializer[Vector[T]] = 
    new CompoundSerializer[Vector[T]] {
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

  def apply[T](serializer : Serializer[T, _]) : CompoundSerializer[List[T]] = 
    new CompoundSerializer[List[T]] {
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

  def apply[K, V](keySerializer : Serializer[K, _], valueSerializer : Serializer[V, _]) : CompoundSerializer[Map[K, V]] =
    new CompoundSerializer[Map[K, V]] {
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

  def apply[U, V](sU : Serializer[U, _], sV : Serializer[V, _]) : CompoundSerializer[(U, V)] = 
    new CompoundSerializer[(U, V)] {
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

  def apply[U, V, W](sU : Serializer[U, _], sV : Serializer[V, _], sW : Serializer[W, _]) : CompoundSerializer[(U, V, W)] = 
    new CompoundSerializer[(U, V, W)] {
      def serialize(uvw : (U, V, W)) : Vector[Any] = Vector(sU.serialize(uvw._1), sV.serialize(uvw._2), sW.serialize(uvw._3))
      def deserialize(b : Any) : (U, V, W) = {
        b match {
          case Vector(u, v, w) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w))
          case _ => throw new RuntimeException("TripleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object QuadrupleSerializer {

  def apply[U, V, W, X](sU : Serializer[U, _], sV : Serializer[V, _], sW : Serializer[W, _], sX : Serializer[X, _]) : CompoundSerializer[(U, V, W, X)] = 
    new CompoundSerializer[(U, V, W, X)] {
      def serialize(uvwx : (U, V, W, X)) : Vector[Any] = Vector(sU.serialize(uvwx._1), sV.serialize(uvwx._2), sW.serialize(uvwx._3), sX.serialize(uvwx._4))
      def deserialize(b : Any) : (U, V, W, X) = {
        b match {
          case Vector(u, v, w, x) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w), sX.deserialize(x))
          case _ => throw new RuntimeException("QuadrupleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object QuintupleSerializer {

  def apply[U, V, W, X, Y](sU : Serializer[U, _], sV : Serializer[V, _], sW : Serializer[W, _], sX : Serializer[X, _], sY : Serializer[Y, _]) : CompoundSerializer[(U, V, W, X, Y)] = 
    new CompoundSerializer[(U, V, W, X, Y)] {
      def serialize(uvwxy : (U, V, W, X, Y)) : Vector[Any] = Vector(sU.serialize(uvwxy._1), sV.serialize(uvwxy._2), sW.serialize(uvwxy._3), sX.serialize(uvwxy._4), sY.serialize(uvwxy._5))
      def deserialize(b : Any) : (U, V, W, X, Y) = {
        b match {
          case Vector(u, v, w, x, y) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w), sX.deserialize(x), sY.deserialize(y))
          case _ => throw new RuntimeException("QuintupleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object OptionSerializer {

  def apply[U, A](serializer : Serializer[U, A]) : CompoundSerializer[Option[U]] = {
    new CompoundSerializer[Option[U]] {
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

object StringSerializer extends AtomicSerializer[String] {

  def serialize(s : String) : Any = s

  def deserialize(b : Any) : String = {
    b match {
      case s : String => s
      case _ => throw new RuntimeException("StringSerializer: cannot deserialize " + b)
    }
  }

}

object BooleanSerializer extends AtomicSerializer[Boolean] {

  def serialize(x : Boolean) : Any = if (x) 1 else 0

  def deserialize(b : Any) : Boolean = {
    b match {
      case x : Long if x == 0 || x == 1 => x == 1
      case _ => throw new RuntimeException("BooleanSerializer: cannot deserialize " + b)
    }
  }

}

object IntSerializer extends AtomicSerializer[Int] {

  def serialize(x : Int) : Any = x

  def deserialize(b : Any) : Int = {
    b match {
      case x : Long => x.toInt
      case _ => throw new RuntimeException("IntSerializer: cannot deserialize " + b)
    }
  }

}

object LongSerializer extends AtomicSerializer[Long] {

  def serialize(x : Long) : Any = x

  def deserialize(b : Any) : Long = {
    b match {
      case x : Long => x
      case _ => throw new RuntimeException("LongSerializer: cannot deserialize " + b)
    }
  }

}

object BigIntSerializer extends AtomicSerializer[BigInt] {

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

trait CaseClassSerializerBase[T] extends CompoundSerializer[T] {
  
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

  /** Override and correct this method if you get type checking errors from the generated code. */
  protected def isCompoundSerializer(serializerSpec : String) : Boolean = !isAtomicSerializer(serializerSpec)

  protected def isAtomicSerializer(serializerSpec : String) : Boolean = {
    val atomicSerializers = Vector("StringSerializer", "BooleanSerializer", "IntSerializer", "LongSerializer", "BigIntSerializer")
    atomicSerializers.contains(serializerSpec)
  }

  private def normalizeCase(c : Any) : (String, Vector[String]) = {
    c match {
      case s : String => (s, Vector())
      case (s : String, t1 : String) => (s, Vector(t1))
      case (s : String, t1 : String, t2 : String) => (s, Vector(t1, t2))
      case (s : String, t1 : String, t2 : String, t3 : String) => (s, Vector(t1, t2, t3))
      case (s : String, t1 : String, t2 : String, t3 : String, t4 : String) => (s, Vector(t1, t2, t3, t4))
      case (s : String, t1 : String, t2 : String, t3 : String, t4 : String, t5 : String) => (s, Vector(t1, t2, t3, t4, t5))
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
              case 4 => "QuadrupleSerializer"
              case 5 => "QuintupleSerializer"
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
        case 1 if isCompoundSerializer(args(0)) =>
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
        case 1 if isCompoundSerializer(args(0)) =>
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

class TypecastSerializer[T, S, A](serializer : Serializer[T, A]) extends Serializer[S, A] {

  def serialize(s : S) : A = serializer.serialize(s.asInstanceOf[T])

  def deserialize(serialized : Any) : S = {
    serializer.deserialize(serialized).asInstanceOf[S]
  }

}

class DummySerializer[T, A] extends Serializer[T, A] {

  def serialize(t : T) : A = {
    throw new RuntimeException("DummySerializer: cannot serialize anything")
  }

  def deserialize(b : Any) : T = {
    throw new RuntimeException("DummySerializer: cannot deserialize anything")
  }

}



