package proofpeer.general

package object continuation {

  sealed trait Thunk[T] {
    protected def compute() : Either[Thunk[T], T]
    def apply() : T = {
      var current : Either[Thunk[T], T] = Left(this)
      while (current.isLeft) current = current.left.get.compute()
      current.right.get
    }
  }

  private final class ValueThunk[T](value : T) extends Thunk[T] {
    protected def compute() = Right(value)
  }

  private final class ComputeThunk[T](computation : () => Thunk[T]) extends Thunk[T] {
    protected def compute() = {
      val t = computation()
      Left(t)
    }
  }
  
  object Thunk {

    def value[T](v : T) : Thunk[T] = new ValueThunk(v)

    def computation[T](f : () => Thunk[T]) : Thunk[T] = new ComputeThunk(f)

  }

  type Continuation[S, T] = (S => Thunk[T])

}

object TestContinuation {

  import continuation._

  def even(n : Long, depth : Int, cont : Continuation[Boolean, Boolean]) : Thunk[Boolean] = {
    if (depth > 1000) Thunk.computation(() => even(n, 0, cont))
    else if (n == 0) cont(true)
    else odd(n - 1, depth + 1, cont)
  }

  def odd(n : Long, depth : Int, cont : Continuation[Boolean, Boolean]) : Thunk[Boolean] = {
    if (depth > 1000) Thunk.computation(() => odd(n, 0, cont))
    else if (n == 0) cont(false)
    else even(n - 1, depth + 1, cont)
  }

  def main(args : Array[String]) {
    val t = even(100000000L, 0, (r => Thunk.value(r)))
    println("t = " + t())
  }

}

