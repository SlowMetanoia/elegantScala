package firstSteps

object PrimalGeneration extends App{
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //Не совсем натуральные числа
  val N = series[Int](0)(_ + 1)
  def erotothfenFilter: Int => Seq[Int] = n=>{
    val naturals = N.take(n).toArray
    for{
      i<-naturals
      if (i!=0)&&(i!=1)
    } {
      var x = 2*i
      while (x<naturals.length) {
        naturals(x) = 0
        x = x + i
      }
    }
    naturals.filter(_!=0)
  }

  def piFunc(x:Int):Int = math.pow(math.log(x),2).round.toInt+1

  def isPiRight = N.drop(2).map{n=>
    (
      n,
      erotothfenFilter(n).length,
      piFunc(n)
    )
  }

  println(isPiRight.take(10000).mkString("\n").drop(1000))
  //println(erotothfenFilter(1000))
  /*
  def presiceErotothfenFilter: Int => Seq[Int] = {
    case x: Int if x < 2 => throw new IllegalArgumentException("Argument should be 2 or more")
    case 2 => Seq(2)
    case 3 => Seq(2, 3)
    case 4 => Seq(2, 3)
    case 5 => Seq(2, 3, 5)
    case x: Int =>
      def bigBadFilter[T]: Iterable[T => Boolean] => (T => Boolean) = filters => {
        filters
          .reduce { (f1, f2) =>
            value => {
              f1(value) && f2(value)
            }
          }
      }

      val filters =
        presiceErotothfenFilter((math.sqrt(x) + 1).round.toInt)
          .map { n =>
            (x: Int) => {
              (x < n) && ((x + 1) % n == 0)
            }
          }

      N.take(x)
        .filter(bigBadFilter(filters))
  }
  println(s"presiceErotothfenFilter(10000) = ${presiceErotothfenFilter(10000)}")*/
}
