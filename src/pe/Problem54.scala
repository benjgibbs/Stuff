package pe
import scala.util.Sorting

object Problem54 {
  val order = List.range(1,10).map(_.toString) ++ List ("T","J","Q","K","A")
  def value(x:String) = order.findIndexOf(_==x)
  def card(x:String) = x(0).toString
  def suit(x:String) = x(1).toString
    
  def isFlush(a:List[String]) = a.map(suit).distinct.length == 1
  def isRoyal(a:List[String]) = (a.map(card)).equals(List("A","J","K","Q","T"))
  def isRoyalFlush(a:List[String]) = isFlush(a) && isRoyal(a)
  
  def numValue(a:List[String], s:String) = a.map(card).filter(_==s).length
  def hasNOfAKind(a:List[String], n:Int) = order.foldLeft((false,-1))((x,y) => {
     def nv = numValue(a,y) == n
     def max :Int = if(nv) value(y) else x._2
     (x._1||nv, max)
  })
  
  def hasFourOfAKind(a:List[String]) = hasNOfAKind(a,4) 
  def hasThreeOfAKind(a:List[String]) = hasNOfAKind(a,3) 
  def hasPair(a:List[String]) = hasNOfAKind(a,2)
  
  def has2Pairs(a:List[String]) = {
    var max = -1
    val search = order.map(x => {
        if(numValue(a,x) == 2) {
          max = value(x)
          1 
        }
        else 0
      })
      if(search.foldLeft(0)(_+_) == 2) (true,max)
      else (false,0)         
    }
  
  def isFullHouse(a:List[String]) = hasThreeOfAKind(a)._1 && hasPair(a)._1
  def isStraight(a:List[String]) =  a.map(card).foldLeft((true,-1))((x,y)=>(x._1 && (x._2 + 1 == value(y) || x._2 == -1),value(y)))._1
  def isStraightFlush(a:List[String]) = isFlush(a) && isStraight(a)
  
  def score(a:List[String]) : (Int,Int) = {
    def highestCard(a:List[String]) = value(card(a(4)))
    val hc = highestCard(a)
    
    val fourOfAKind = hasFourOfAKind(a)
    val threeOfAKind = hasThreeOfAKind(a)
    val twoPairs = has2Pairs(a)
    val pair = hasPair(a)
    
    if(isRoyalFlush(a)) (0,hc)
    else if(isStraightFlush(a)) (1,hc)
    else if(fourOfAKind._1) (2,fourOfAKind._2)
    else if(isFullHouse(a)) (3,hc)
    else if(isFlush(a)) (4,hc)
    else if(isStraight(a)) (5,hc)
    else if(threeOfAKind._1) (6,threeOfAKind._2)
    else if(twoPairs._1) (7, twoPairs._2)
    else if(pair._1) (8,pair._2)
    else (9,hc)
  }
  
  val check1 = check("5H 5C 6S 7S KD 2C 3S 8S 8D TD")
  val check2 = check("5D 8C 9S JS AC 2C 5C 7D 8S QH")
  val check3 = check("2D 9C AS AH AC 3D 6D 7D TD QD")
  val check4 = check("4D 6S 9H QH QC 3D 6D 7H QD QS")
  val check5 = check("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")
    
      
  //p1 wins -> 1, p2 wins -> 2, draw = 0
  def check(line: String) : Int = {
    val hands = List.fromString(line,' ').splitAt(5)
      val p1 = hands._1.sort(_<_)
      val p2 = hands._2.sort(_<_)
      val p1Score = score(p1)
      val p2Score = score(p2)
      print(p1+": "+p1Score+", "+p2+": "+p2Score)
      if(p1Score._1 > p2Score._1) 1
      else if(p1Score._1 < p2Score._2) 2
      else if(p1Score._2 > p2Score._2) 1
      else if(p1Score._2 < p2Score._2) 2
      else 0
  }
    
  
  def main(args: Array[String]): Unit = {  
    var p1Wins = 0
    var p2Wins = 0
    var draws = 0
    
    for(l<-scala.io.Source.fromFile("resources/poker.txt").getLines){
    	val res = check(l)
    	if(res == 1) p1Wins += 1
    	else if(res == 2) p2Wins += 1
    	else draws += 1
    }
    println("End - p1Wins: " + p1Wins + ", p2Wins: " + p2Wins + ", Draws: " + draws)
  }
}