import scala.util.Random


object CompteBon extends App {
  
  private var i:Int=1
  
  override def main(arg:Array[String]):Unit={
    combinatoire(List(100,5,9,50,4,10)).foreach(println)
  }
  
  /**
   * 
   */
  def randTotal:Int={
    var r:Int = -1
    while (r<=100 && r>=999){
      r=Random.nextInt()
    }
    r
  }
  
  /**
   * 
   */
  private def compte(l:List[Int],acu:Int):Unit= l match {
    case Nil    => ()
    case (h::t) => {
      i+=1
      println(s"$i ... $h + $acu = ${h+acu}") 
      compte(t,acu+h)
      i+=1
      println(s"$i ... $acu - $h = ${Math.abs(acu-h)}")
      compte(t,Math.abs(acu-h))
      i+=1
      println(s"$i ... $acu * $h = ${acu*h}")
      compte(t,acu*h)
      if (h>0 && acu>0) {
        i+=1
        println(s"$i ... $acu / $h = ${acu/h}")
        compte(t,h/acu)
        i+=1
        println(s"$i ... $h / $acu = ${h/acu}")
        compte(t,h/acu)
      }
    }
  }
  
  /**
   * 
   */
  def copy(l:List[Int],c:Int):List[Int]=l match {
      case Nil    => Nil
      case (h::t) if (c>=0) => h::(copy(t,c-1))
      case _      => Nil
  }
  
  /**
   * 
   */
  def combi(a:Int,l:List[Int],c:Int):List[List[Int]]=l match {
    case Nil => List(List(a))
    case (h::t) => if (l.length>c) List(a::copy(l,c)):::(combi(a,l,c+1))
                   else combi(a,t,0)
  }
  
  /**
   * 
   */
  def combinatoire(l:List[Int]):List[List[Int]]=l match{
    case Nil => Nil
    case (h::t)=>combi(h,t,0):::(combinatoire(t))
  }
  
}