import java.util.NoSuchElementException

object cons {
  def main(agrs:Array[String]): Unit ={
    val v = Vector
    def insert(x:Int, xs: List[Int]):List[Int]={
      xs match {
        case Nil =>x :: Nil
        case y::ys => if(x<y){
          x::y::ys
        } else {
          y :: insert(x,ys)
        }
      }
    }
    def isort(list: List[Int]):List[Int]={
      list match {
        case Nil=> Nil
        case x::xs=>
          insert(x,isort(xs))
      }
    }
    val list = List(6,3,4,5,7,8)
    val ls= List(1,23,3)
    println(ls ::: list)
    println(ls ++ list)
  }

}
