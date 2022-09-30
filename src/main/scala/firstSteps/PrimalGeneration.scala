package firstSteps

object PrimalGeneration extends App{
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //Не совсем натуральные числа
  val N = series[Int](0)(_ + 1)
  //Решето Эратосфена
  def erotothfenFilter: Int => Seq[Int] = n=>{
    val naturals = N.take(n).toArray
    for{
      i<-naturals
      if i>1
    } {
      var x = 2*i
      while (x<naturals.length) {
        naturals(x) = 0
        x = x + i
      }
    }
    naturals.filter(_!=0)
  }

  //Приближение Лежандра для Пи-функции
  //Нужна была для дебага
  def lI(x:Int) = (x / (math.log(x) - 1.08366)).round + 1
  //Что-то тоже для дебага
  def isPiRight = N.drop(2).map{n=>
    (
      n,
      erotothfenFilter(n).length,
      lI(n)
    )
  }
  //Первые сколько-то простых
  val thousandPrimalNumbers = erotothfenFilter(100000).drop(1)
  
  
}
