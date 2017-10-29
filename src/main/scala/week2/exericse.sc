object exericse{

  // function that computes product from a+1 to b
  def sum(f:Int=>Int)(a:Int, b:Int):Int =
    if(a>b) 0
    else f(a)+sum(f)(a+1,b)


  sum(x => x)(0,5)
  sum(x => x*x)(1,4)


  // function that computes product from a+1 to b
  def product(f:Int=>Int)(a:Int, b:Int):Int =
    if(a>b) 1
    else f(a)*product(f)(a+1,b)


  product(x => x)(2,5)
  product(x => x*x)(1,4)

  // define factorial in terms of product
  def factorial(a: Int) : Int = {
    product(x => x)(1, a)
  }

  factorial(5)

  // function that combines sum and product functions
  def mapReduce(f:Int=>Int, combine : (Int, Int)=>Int, zero: Int)(a:Int, b:Int): Int = {

    if(a>b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1,b))

  }

  // compute sum
  mapReduce(x => x, (a,b)=>a+b, 0)(0,5)

  // compute product
  mapReduce(x => x, (a,b)=>a*b, 1)(1,5)

  def product_new(f:Int=>Int)(a:Int, b:Int):Int = mapReduce(x => x, (a,b)=>a*b, 1)(a,b)

  product_new(x => x)(1,5)
  
}