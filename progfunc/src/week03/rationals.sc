package week03

object rationals {
  val x = new Rational(1,3)                       //> x  : week03.Rational = 1/3
  val y = new Rational(5,7)                       //> y  : week03.Rational = 5/7
  val z = new Rational(3,2)                       //> z  : week03.Rational = 3/2
  x + y                                           //> res0: week03.Rational = 22/21
  x.neg                                           //> res1: week03.Rational = 1/-3
  x - y - z                                       //> res2: week03.Rational = -79/42
  x < y                                           //> res3: Boolean = true
  x max y                                         //> res4: week03.Rational = 5/7
	
}

class Rational(x: Int, y: Int){
	require(y != 0, "denom must be non-zero")
	
	//secondary constructor
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
	private val g = gcd(x, y)
	
	val numer = x/g
	val denom = y/g
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if(this < that) that else this
	
	def + (that: Rational) =
		new Rational(numer * that.denom + that.numer * denom,denom * that.denom)
	
	def - (that: Rational) = this + that.neg
	
	def neg : Rational = new Rational(-numer, denom)
		
	override def toString ={
		numer + "/" + denom}
		
}