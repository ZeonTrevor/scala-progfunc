package week02

object exercise_currying {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
  	if(a > b) 1
  	else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  def fact(n: Int) = {
  	product(x => x)(1,n)
  }                                               //> fact: (n: Int)Int
  
  def mapReduce(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
  }                                               //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
	def productMR(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x*y, 1)(a,b)
                                                  //> productMR: (f: Int => Int)(a: Int, b: Int)Int
	productMR(x=>x)(3,4)                      //> res0: Int = 12
}