Object Sorts extends App{

 def selectionSort(A: Array[Int]): Array[Int] = {

  for(i <- 0 until A.size){
     var max = i;println(s"i=$i");for(j <- i+1 until A.size){
       if(A(j) > A(max))
          max = j
      }
     if( max != i)
      {
        val tmp = A(max)
        A(max) = A(i)
        A(i) = tmp
     }; println(A.mkString(","))
  }
 A
 }
 
 }
