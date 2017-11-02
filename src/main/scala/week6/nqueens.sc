object nqueens{

  def queens(n: Int): Set[List[Int]] = {

    def placeQueens(k:Int): Set[List[Int]] =
      if(k==0) Set(List())
      else
        for {

          queen <- placeQueens(k - 1)
          col <- 0 until n
          if (isSafe(col, queen))
        } yield col :: queen

    placeQueens(n)
  }

  def isSafe(col: Int,queens: List[Int]): Boolean = {

    val row = queens.length
    val queenWithRow = (row-1 to 0 by -1) zip queens
    queenWithRow forall{

      case (r,c) => col!=c && math.abs(col-c)!= row-r

    }

  }

  def show(queen: List[Int])= {

    val lines =
      for (col <- queen.reverse)
        yield Vector.fill(queen.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }


  queens(4) map show

  (queens(4) map show) mkString "\n"

}