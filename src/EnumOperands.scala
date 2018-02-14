/**
 * 
 */
object Plak {
      
    /**
     *  
     */
   def toNumber(n:String):Int= n match{
     case "hundred" => 100
     case "seventyfive" => 75
     case "fifty" => 50
     case "twentyfive"=>25
     case "ten"=>10
     case "nine"=>9
     case "eight"=>8
     case "seven"=>7
     case "six"=>6
     case "five"=>5
     case "four"=>4
     case "three"=>3
     case "two"=>2
     case "one"=>1
   }
  
  
  val pl=List[String]("hundred", "seventyfive", "fifty", "twentyfive", "ten",
             "nine", "eight", "seven", "six", "five", "four", "three", "two", "one",
             "hundred", "seventyfive", "fifty", "twentyfive", "ten",
             "nine", "eight", "seven", "six", "five", "four", "three", "two", "one")
  
  
}