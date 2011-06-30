package pe

object Problem99 {
  
  def split(s:String) = {
    val ss = s.split(',')
    Tuple(ss(0).toInt,ss(1).toInt)
  }
  
  def isGreater(a:(Int,Int),b:(Int,Int)) = {
    a._2*Math.log(a._1) < b._2*Math.log(b._1)
  }
  
  def main(args:Array[String]) : Unit = {
	  var lineCount = 0
	  var greatest = (1,1)
	  var greatestLine = 0
	  for(l <- io.Source.fromFile("resources/base_exp.txt").getLines()){
	    lineCount += 1
	    val test = split(l)
	    if(isGreater(greatest,test)){
	      greatest = test
	      greatestLine = lineCount
	      println("New greatest: " + greatest + " on line " +lineCount)
	    }
	  }
	  println("Greatest: " + greatest + ", on line: " + greatestLine)
	}
}