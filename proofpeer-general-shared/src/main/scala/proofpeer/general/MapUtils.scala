package proofpeer.general

object MapUtils {

  def merge[K, V](m1 : Map[K, V], m2 : Map[K, V], join : (V, V) => V) : Map[K, V] = {
    var m = m1
    for ((k, v) <- m2) {
      m1.get(k) match {
        case None => 
          m = m + (k -> v)
        case Some(u) =>
          m = m + (k -> join(u, v))
      }
    }
    m
  }

}