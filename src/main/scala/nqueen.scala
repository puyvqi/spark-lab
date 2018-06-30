class nqueen {

}

object nqueen {
  def queens(n: Int): Set[List[Int]] = {
    def placekqueen(k: Int): Set[List[Int]] = {
      if (k == 0) Set(Nil)
      else {
        for {queen <- placekqueen(k - 1)
             col <- 0 until n
             if safe(col, queen)} yield col :: queen
      }
    }

    placekqueen(n)
  }

  def safe(col: Int, queen: List[Int]): Boolean = {
    val row = queen.length
    val indexedcol = (queen.length - 1 to 0 by -1) zip queen
    indexedcol.forall { case (row2, col2) =>
      col2 != col && math.abs(row2 - row) != math.abs(col - col2)
    }
  }

  def show(a: List[Int]) = {
    val lines = for {
      line <- a.reverse
    } yield Vector.fill(a.length)("* ").updated(line, "o ").mkString
    "\n" + lines.mkString("\n")
  }

  def main(agrs: Array[String]) = {
    val res = queens(6)
    for(i<-res){
      println(show(i))
    }
    print("dfs")
  }

  def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

  def sqrtStream(org: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + org / guess) / 2

    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }


}