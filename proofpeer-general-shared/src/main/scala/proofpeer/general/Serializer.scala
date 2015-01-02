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
          case _ => throw new RuntimeException("cannot deserialize, vector expected")
        }
      }
    }

}
