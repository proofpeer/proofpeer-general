package proofpeer.general

trait Serializer[T] {

  def serialize(t : T) : Any

  def deserialize(b : Any) : T

}

object VectorSerializer {

  def apply[T](serializer : Serializer[T]) : Serializer[Vector[T]] = 
    new Serializer[Vector[T]] {
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

  def apply[T](serializer : Serializer[T]) : Serializer[List[T]] = 
    new Serializer[List[T]] {
      def serialize(v : List[T]) = v.map(x => serializer.serialize(x))
      def deserialize(b : Any) : List[T] = {
        b match {
          case v : Vector[Any] => v.map(x => serializer.deserialize(x)).toList
          case _ => throw new RuntimeException("ListSerializer: cannot deserialize " + b)
        }
      }
    }

}

object SetSerializer {

  def apply[T](serializer : Serializer[T]) : Serializer[Set[T]] = 
    new Serializer[Set[T]] {
      def serialize(v : Set[T]) = v.map(x => serializer.serialize(x))
      def deserialize(b : Any) : Set[T] = {
        b match {
          case v : Vector[Any] => v.map(x => serializer.deserialize(x)).toSet
          case _ => throw new RuntimeException("SetSerializer: cannot deserialize " + b)
        }
      }
    }

}

object MapSerializer {

  def apply[K, V](keySerializer : Serializer[K], valueSerializer : Serializer[V]) : Serializer[Map[K, V]] =
    new Serializer[Map[K, V]] {
      def serialize(m : Map[K, V]) : Any = {
        var result : List[Any] = List()
        for ((k, v) <- m) result = keySerializer.serialize(k) :: valueSerializer.serialize(v) :: result
        result
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

  def apply[U, V](sU : Serializer[U], sV : Serializer[V]) : Serializer[(U, V)] = 
    new Serializer[(U, V)] {
      def serialize(uv : (U, V)) = Vector(sU.serialize(uv._1), sV.serialize(uv._2))
      def deserialize(b : Any) : (U, V) = {
        b match {
          case Vector(u, v) => (sU.deserialize(u), sV.deserialize(v))
          case _ => throw new RuntimeException("PairSerializer: cannot deserialize " + b)
        }
      }
    }

}

object TripleSerializer {

  def apply[U, V, W](sU : Serializer[U], sV : Serializer[V], sW : Serializer[W]) : Serializer[(U, V, W)] = 
    new Serializer[(U, V, W)] {
      def serialize(uvw : (U, V, W)) = Vector(sU.serialize(uvw._1), sV.serialize(uvw._2), sW.serialize(uvw._3))
      def deserialize(b : Any) : (U, V, W) = {
        b match {
          case Vector(u, v, w) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w))
          case _ => throw new RuntimeException("TripleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object QuadrupleSerializer {

  def apply[U, V, W, X](sU : Serializer[U], sV : Serializer[V], sW : Serializer[W], sX : Serializer[X]) : Serializer[(U, V, W, X)] = 
    new Serializer[(U, V, W, X)] {
      def serialize(uvwx : (U, V, W, X)) = Vector(sU.serialize(uvwx._1), sV.serialize(uvwx._2), sW.serialize(uvwx._3), sX.serialize(uvwx._4))
      def deserialize(b : Any) : (U, V, W, X) = {
        b match {
          case Vector(u, v, w, x) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w), sX.deserialize(x))
          case _ => throw new RuntimeException("QuadrupleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object QuintupleSerializer {

  def apply[U, V, W, X, Y](sU : Serializer[U], sV : Serializer[V], sW : Serializer[W], sX : Serializer[X], sY : Serializer[Y]) : Serializer[(U, V, W, X, Y)] = 
    new Serializer[(U, V, W, X, Y)] {
      def serialize(uvwxy : (U, V, W, X, Y)) = Vector(sU.serialize(uvwxy._1), sV.serialize(uvwxy._2), sW.serialize(uvwxy._3), sX.serialize(uvwxy._4), sY.serialize(uvwxy._5))
      def deserialize(b : Any) : (U, V, W, X, Y) = {
        b match {
          case Vector(u, v, w, x, y) => (sU.deserialize(u), sV.deserialize(v), sW.deserialize(w), sX.deserialize(x), sY.deserialize(y))
          case _ => throw new RuntimeException("QuintupleSerializer: cannot deserialize " + b)
        }
      }
    }

}

object OptionSerializer {

  def apply[U](serializer : Serializer[U]) : Serializer[Option[U]] = {
    new Serializer[Option[U]] {
      def serialize(opt : Option[U]) = {
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

object StringSerializer extends Serializer[String] {

  def serialize(s : String) = s

  def deserialize(b : Any) : String = {
    b match {
      case s : String => s
      case _ => throw new RuntimeException("StringSerializer: cannot deserialize " + b)
    }
  }

}

object BooleanSerializer extends Serializer[Boolean] {

  def serialize(x : Boolean) = if (x) 1 else 0

  def deserialize(b : Any) : Boolean = {
    b match {
      case x : Long if x == 0 || x == 1 => x == 1
      case _ => throw new RuntimeException("BooleanSerializer: cannot deserialize " + b)
    }
  }

}

object IntSerializer extends Serializer[Int] {

  def serialize(x : Int) = x

  def deserialize(b : Any) : Int = {
    b match {
      case x : Long => x.toInt
      case _ => throw new RuntimeException("IntSerializer: cannot deserialize " + b)
    }
  }

}

object LongSerializer extends Serializer[Long] {

  def serialize(x : Long) = x

  def deserialize(b : Any) : Long = {
    b match {
      case x : Long => x
      case _ => throw new RuntimeException("LongSerializer: cannot deserialize " + b)
    }
  }

}

object BigIntSerializer extends Serializer[BigInt] {

  def serialize(x : BigInt) = {
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

trait CaseClassSerializerBase[T] extends Serializer[T] {
  
  def decomposeAndSerialize(obj : T) : (Int, Option[Any])
  
  def deserializeAndCompose(kind : Int, args : Option[Any]) : T

  def serialize(obj : T) : Any = {
    val (kind, args) = decomposeAndSerialize(obj)
    args match {
      case None => kind
      case Some(args) => Vector(kind, args)
    }
  }

  def deserialize(b : Any) : T = {
    b match {
      case kind : Long =>
        deserializeAndCompose(kind.toInt, None)
      case Vector(kind : Long, args) =>
        deserializeAndCompose(kind.toInt, Some(args))
      case _ => throw new RuntimeException("Cannot deserialize: " + b)
    }
  }

}

/** Use this tool to generate code which implements a CaseClassSerializerBase[typename] */
class CaseClassSerializerTool(basename : String, cases : Vector[Any], typename : String = "Any") {

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
    println("  def decomposeAndSerialize(obj : " + typename + ") : (Int, Option[Any]) = {")
    println("    obj match {")
    val SPACES = "      "
    for ((name, args) <- normalizedCases) {
      val NAME = name.toUpperCase
      args.size match {
        case 0 =>
          println(SPACES + "case " + name + " =>")
          println(SPACES + "  (Kind." + NAME + ", None)")
        case 1 =>
          println(SPACES + "case " + name + "(x) =>")
          println(SPACES + "  (Kind." + NAME + ", Some(Serializers." + NAME + ".serialize(x)))")          
        case _ =>
          println(SPACES + "case t : " + name + " =>")
          println(SPACES + "  (Kind." + NAME + ", Some(Serializers." + NAME + ".serialize(" + name + ".unapply(t).get)))")
      }
    }
    println(SPACES + "case _ => throw new RuntimeException(\"" + basename + ": cannot serialize \" + obj)")
    println("    }")
    println("  }")
  }

  private def outputDeserializer() {
    println("  def deserializeAndCompose(kind : Int, args : Option[Any]) : " + typename + " = {")
    println("    kind match {")
    val SPACES = "      "
    for ((name, args) <- normalizedCases) {
      val NAME = name.toUpperCase
      args.size match {
        case 0 =>
          println(SPACES + "case Kind." + NAME + " if args.isEmpty => ")
          println(SPACES + "  " + name)
        case 1 =>
          println(SPACES + "case Kind." + NAME + " if args.isDefined => ")
          println(SPACES + "  " + name + "(Serializers." + NAME + ".deserialize(args.get))")
        case _ =>
          println(SPACES + "case Kind." + NAME + " if args.isDefined => ")
          println(SPACES + "  " + name + ".tupled(Serializers." + NAME + ".deserialize(args.get))")
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

class TransformSerializer[S, T](serializer : Serializer[T], transformST : S => T, transformTS : T => S) extends Serializer[S] {

  def serialize(s : S) : Any = serializer.serialize(transformST(s))

  def deserialize(b : Any) : S = transformTS(serializer.deserialize(b))

}

class TypecastSerializer[S, T](serializer : Serializer[T]) extends TransformSerializer[S, T](serializer,
  _.asInstanceOf[T], _.asInstanceOf[S])

class DummySerializer[T] extends Serializer[T] {

  def serialize(t : T) : Any = {
    throw new RuntimeException("DummySerializer: cannot serialize anything")
  }

  def deserialize(b : Any) : T = {
    throw new RuntimeException("DummySerializer: cannot deserialize anything")
  }

}





