package pe

class PrimeSieve(upto: Int) {
	val sieve = new Array[Boolean](upto)
	for(i <- 2 until upto) sieve(i) = true
	var p = 2
	var limit = Math.sqrt(upto)
	while(p <= limit){
		while(!sieve(p)) p+=1
		var i = p*2
		while(i < upto){
		  sieve(i) = false 
		  i += p
		}
		p+=1
	}
	def isPrime(test: Int) : Boolean = sieve(test)
}