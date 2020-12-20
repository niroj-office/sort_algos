object HeapSort{
  def apply(A: Array[Int]): Array[Int] = {sort(A);A}
 
  private def sort(a: Array[Int]): Unit = {
    var n = a.length - 1 
    buildMinHeap(a, n)
    while (n >= 1) {
      swap(a, 0, n)
      n-=1
      heapify(a, 0, n)
    }
  }

  private def buildMinHeap(a: Array[Int], n: Int): Unit = {
    for (i <- n/2 to 0 by -1) {
      heapify(a, i, n)
    }
  }

  private def swap(a: Array[Int], pos1: Int, pos2:Int): Unit = {
    val tmp = a(pos1)
    a(pos1) = a(pos2)
    a(pos2) = tmp
  }
  
  @annotation.tailrec
  private def heapify(a: Array[Int], i: Int, lastChild: Int): Unit = {
    val left = 2*i 
    val right = 2*i+1

    var max = i

    if(left <=  lastChild && a(left) > a(max)) max = left
    if(right <= lastChild && a(right) > a(max)) max = right

    if(max != i) {
      swap(a, max, i)
      heapify(a, max, lastChild)
    }
  }


}
