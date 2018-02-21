import scala.util.Random


object CompteBon extends App {
  
  private var i:Int=1
  
  override def main(arg:Array[String]):Unit={
    val n=randTotal(100,999)
    println(s"Tirage du nombre $n")
    val p=choixDePlak.toList
    println(s"Tirage des plaques numérotées...."+p)
    
    println(s"Résolution....")
    val l=combinatoire(choixDePlak.toList)
    l.map(x=>compte(x.tail,x.head,n))
    
  }
  
  /**
   * 
   */
  def choixDePlak:Array[Int]={
    val p=Plak.pl
    val z=p.size-1
    var l=Array[Int](Plak.toNumber(p(randTotal(0,z))),
                     Plak.toNumber(p(randTotal(0,z))),
                     Plak.toNumber(p(randTotal(0,z))),
                     Plak.toNumber(p(randTotal(0,z))),
                     Plak.toNumber(p(randTotal(0,z))),
                     Plak.toNumber(p(randTotal(0,z))))
    l
  }
  
  /**
   * 
   */
  def randTotal(u:Int,v:Int):Int={
    var r:Int = -1
    Array
    while (r<=u || r>=v){
      r=Random.nextInt()
    }
    r
  }
  
  /**
   * 
   */
  private def compte(l:List[Int],acu:Int,n:Int):Unit= l match {
    case Nil    => ()
    case (h::t) => {
      if (acu==n) println(s"$acu == $n Gagné!")
      i+=1
      println(s"$i ... $h + $acu = ${h+acu}") 
      compte(t,acu+h,n)
      i+=1
      println(s"$i ... $acu - $h = ${Math.abs(acu-h)}")
      compte(t,Math.abs(acu-h),n)
      i+=1
      println(s"$i ... $acu * $h = ${acu*h}")
      compte(t,acu*h,n)
      if (h>0 && acu>0) {
          i+=1
          println(s"$i ... $acu / $h = ${acu/h}")
          compte(t,h/acu,n)
          i+=1
          println(s"$i ... $h / $acu = ${h/acu}")
          compte(t,h/acu,n)
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