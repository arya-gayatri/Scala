object pairs{

  // flatten used to get combine collection of collections into a single collection

  // get tuples (i,j) such that j<i

  val n=7

  ((1 until n) map (i => (1 until i) map (j => (i,j)))).flatten

  // OR

  (1 until n) flatMap (i => (1 until i) map (j => (i,j)))

  // filter such that sum of tuple is prime

  def isPrime(n: Int) = (2 until n) forall(n%_!=0)

    (1 until n) flatMap (i => (1 until i) map (j => (i,j))) filter(pair => isPrime(pair._1+pair._2))

  // using for expression

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
} yield (i,j)


  def scalarProduct(xs:List[Double], ys:List[Double]): List[Double] = {
    for {
      // make tuples from the 2 lists
      (x, y) <- xs zip ys

    } yield (x * y)

  }

  val first = List(10.2, 11.1)
  val second = List(2.1,3.1)
  scalarProduct(first, second)
}