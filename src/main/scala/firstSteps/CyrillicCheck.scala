package firstSteps

import scala.io.StdIn

object CyrillicCheck extends App{
  def checkCyrillic( str: String): Boolean =
    """\s*\p{IsCyrillic}+(\p{IsCyrillic}|\s)*""".r.matches(str) //&& """(\p{Upper}|\s)*""".r.matches(str)
  
  //println("Input your text:")
  //println(s"Your text is ${ if(checkCyrillic(StdIn.readLine())) "Cyrillic" else "not Cyrillic"}")
  
  def checkInput(rule:String=>Boolean):String = {
    var result = ""
    do result = StdIn.readLine() while(!rule(result))
    result
  }
  val inputData = checkInput(checkCyrillic).toUpperCase
}
