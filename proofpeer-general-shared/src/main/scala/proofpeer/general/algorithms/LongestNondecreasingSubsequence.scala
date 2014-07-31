package proofpeer.general.algorithms

object LongestNondecreasingSubsequence {

  private case class Entry[T](value : T, previousEntry : Entry[T])

  def compute[T](seq : Seq[T])(implicit ordering : Ordering[T]) : Seq[T] = {
    if (seq.isEmpty) return seq
    val b : Array[Entry[T]] = new Array(seq.size)
    var r : Int = 0
    b(r) = Entry(seq.head, null)
    for (t <- seq.tail) {
      if (ordering.compare(t, b(r).value) >= 0) {
        r = r + 1
        b(r) = Entry(t, b(r - 1))
      } else {
        // find smallest j such that t < b[j].value
        var lo = 0
        var hi = r
        while (lo < hi) {
          val mid = (lo + hi) / 2
          if (ordering.compare(t, b(mid).value) < 0)
            hi = mid
          else 
            lo = mid + 1
        }
        if (lo == 0)
          b(lo) = Entry(t, null)
        else 
          b(lo) = Entry(t, b(lo-1))
      }
    } 
    var result : List[T] = List()
    var entry : Entry[T] = b(r)
    do {
      result = entry.value :: result
      entry = entry.previousEntry
    } while (entry != null)
    return result
  }

  def compute[T](seq : Seq[T], mustContain : Int)(implicit ordering : Ordering[T]) : Seq[T] = {
    val (front, back) = seq splitAt mustContain
    val t = back.head
    val frontLongestSeq = compute(front.filter(s => ordering.compare(s, t) <= 0))(ordering)
    val backLongestSeq = compute(back.tail.filter(s => ordering.compare(t, s) <= 0))(ordering)
    frontLongestSeq ++ (t +: backLongestSeq)
  }

}