package pe
import org.junit._

class PrimeSieveTest {
	@Test def sieveTheFirst1000Pries() = {
	  var currentTest = 0;
	  val ps = new PrimeSieve(8000)
	  for(line <- scala.io.Source.fromFile("resources/1000.txt").getLines){
	    for(prime <- List.fromString(line, ' ').map( x => Integer.parseInt(x))) {
	        while(currentTest < prime) {
	          Assert.assertFalse(currentTest + " should not be prime.", ps.isPrime(currentTest))
	          currentTest += 1
	        }
	        Assert.assertTrue(currentTest + " should be prime.", ps.isPrime(currentTest))
	        currentTest += 1
	    }
	  }
	}
	
	@Test def twoHasOnlyOneDivisor(){
	  val ps = new PrimeSieve(100)
	  val divisors = ps.primeDivisors(2)
	  Assert.assertEquals(1,divisors.length)
	  Assert.assertEquals(List(2),divisors)
	}
	
	@Test def primesHaveOnlyOneDivisor(){
	  val ps = new PrimeSieve(100)
	  val divisors = ps.primeDivisors(13)
	  Assert.assertEquals(1,divisors.length)
	  Assert.assertEquals(List(13),divisors)
	}
	
	@Test def compundsHaveTheRightDivisor(){
	  val ps = new PrimeSieve(100)
	  val divisors = ps.primeDivisors(30)
	  Assert.assertEquals(3,divisors.length)
	  Assert.assertEquals(List(2,3,5),divisors)
	}
	
	@Test def primePowerOf8() = Assert.assertEquals(3, PrimeSieve.primePower(8,2))
	@Test def primePowerOf34() = Assert.assertEquals(1, PrimeSieve.primePower(34,17))
}