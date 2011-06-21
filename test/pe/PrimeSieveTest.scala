package pe
import org.junit._

class PrimeSieveTest {
	@Test def sieveTheFirst1000Pries() = {
	  var currentTest = 0;
	  val ps = new PrimeSieve(8000)
	  for(line <- scala.io.Source.fromFile("1000.txt").getLines){
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
}