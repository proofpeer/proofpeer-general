package proofpeer.general.algorithms

object TopologicalSort {

  def compute[T](graph : Map[T, Set[T]]) : (Seq[T], Map[T, Set[T]]) = {
    var result : List[T] = List()
    var sorted : Set[T] = Set()
    var unsorted : Map[T, Set[T]] = graph

    def sort : Boolean = {
      var did_something = false
      for ((n, parents) <- unsorted) {
        if (parents subsetOf sorted) {
          result = n :: result
          sorted = sorted + n
          unsorted = unsorted - n
          did_something = true
        }
      }
      did_something
    }

    while (sort) {}

    (result.reverse, unsorted)
  }

}