class waterpouring(val capacity:Vector[Int], val target: Int) {

  trait Move{
    def change(originState: Vector[Int]): Vector[Int]
  }
  case class Full(pos: Int) extends Move{
    override def change(originState: Vector[Int]): Vector[Int] = {
      originState updated (pos,capacity(pos))
    }

    override def toString: String = s"Full $pos"
  }
  case class Empty(pos: Int) extends Move{
    override def change(originState: Vector[Int]): Vector[Int]={
      originState updated (pos,0)
    }
    override def toString = s"Empty $pos"
  }
  case class Pour(from: Int, to: Int)extends Move{
    override def change(originState: Vector[Int]):Vector[Int]={
      val amount = capacity(to)-originState(to)
      if(originState(from)<amount){
        originState updated (to,originState(from)+originState(to)) updated (from,0)
      } else {
        originState updated (to,capacity(to)) updated (from,originState(from)-amount)
      }
    }
    override def toString:String=s"Pour from $from to $to"
  }
  val initialState:Vector[Int] = capacity map (a=>0)
  val glasses = 0 until capacity.length
  val moves = (for {g<-glasses}yield Full(g)) ++
    (for {g<-glasses}yield Empty(g)) ++
    (for {to <-glasses;from <-glasses; if to!=from}yield Pour(from,to))
  class Path(val history: List[Move]){
    val endState: Vector[Int]= (history foldRight initialState)(_.change(_))
    def extend(move: Move):Path=new Path(move::history)
    override def toString=(history mkString("->"))+ "-->"+endState
  }
  val initialPath =new Path(List())
  def from(oldset: Set[Path], exploredStates:Set[Vector[Int]]):Stream[Set[Path]]={
    if(oldset.isEmpty)Stream.empty
    else{
      val more=for{
        path<-{oldset}
        next<-moves map path.extend
        if !(exploredStates contains next.endState )
      }yield next
      oldset#::from(more,exploredStates ++ more.map(_.endState))
    }
  }
  val pathSets = from(Set(initialPath),Set(initialState))
  def solve():Stream[Path]={
    for {
      paths<-pathSets
      path<-paths
      if path.endState contains target
    }yield path
  }

}
object waterpouring {
  def main(agrs:Array[String]): Unit ={
    val ss = new waterpouring(Vector(4,9),7)
    val res= ss.solve().take(4).toList
    for {path<-res}println(path)

  }
}
