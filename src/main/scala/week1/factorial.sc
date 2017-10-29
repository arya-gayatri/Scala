object fact{

//  def factorial(x:Int) : Int = {
//    if(x==1) 1
//    else x*factorial(x-1)
//
//  }

  def factorial(n:Int) : Int = {
    def loop(acc:Int, n:Int): Int =
      if(n==0) acc
      else loop(acc*n,n-1)

    loop(1,n)
  }

  factorial(5)
}