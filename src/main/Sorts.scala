object Timer{
	def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
}
}


object Sorts extends App{

 def swap[T](array: Array[T], pos1:Int, pos2:Int ): Array[T] ={
   val tmp = array(pos1)
   array(pos1) = array(pos2)
   array(pos2) = tmp
   return array
}
 
def selectionSort(A: Array[Int]): Array[Int] = {
 for(i <- 0 until A.size){
    var min = i
    for(j <- i+1 until A.size){
      if(A(j) < A(min))
         min = j
     }
    if( min != i)
       swap(A, i, min)
 }
A
}
 
 def reverseSelectionSort(A: Array[Int]): Array[Int] = {

  for(i <- 0 until A.size){
     var max = i;
	 //println(s"i=$i");
	 
	 for(j <- i+1 until A.size){
       if(A(j) > A(max))
          max = j
      }
     if( max != i)
      {
		swap(A,max,i)
     } 
	 //println(A.mkString(","))
  }
 A
 }

def bubbleSort(A: Array[Int]): Array[Int] = {
	val n = A.size
	for (i <- 0 until n)
		for(j <- n-1 until i by -1){
		  if( A(j-1) > A(j))
                   swap(A,j-1,j)
	         }
	A
  }
 
 }
