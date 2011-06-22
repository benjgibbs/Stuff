package pe

object Problem51 {
  def main(args : Array[String]) : Unit = {
    val searchTo = 1000000
    val numToCheck = 8
    val ps = new PrimeSieve(searchTo)
    for(i <- 2 until searchTo){
      if(ps.isPrime(i)){
    	  val pStr = i.toString
    	  for(j <- 0 to 9){
    	    var others = List[Int]()
    	    val jStr = j.toString
    	    for(k<-j+1 to 9){
    	      val kStr = k.toString
    	      val toTest = pStr.replace(jStr,kStr).toInt
    	      if(toTest != i && ps.isPrime(toTest)){
    	        //println(pStr + ","+i+","+j+","+k)
    	        others = toTest :: others
    	      }
    	    }
    	    if(others.size+1 >= numToCheck ){
    	      println(pStr + " generates " + others)
    	    }
    	  }
      }
    }
  }
}
