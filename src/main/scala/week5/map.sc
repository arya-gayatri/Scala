object map{

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => y*y :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x*x)

  val l = List(1,2,3,4,5)
  squareList1(l)
  squareList2(l)



}