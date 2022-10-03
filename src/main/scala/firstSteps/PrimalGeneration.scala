package firstSteps

import scala.util.Random

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
  val somePrimeNumbers = erotothfenFilter(100000).drop(1)

  val prime = somePrimeNumbers(Random.nextInt(somePrimeNumbers.length))

  def bigRandomInt(l:BigInt,r:BigInt):BigInt = {
    val x2 = series[BigInt](1)(_*2)
    val length = r - l
    val byteNumber = x2.find(_>length).get.toInt
    val c = (1 to byteNumber).map(x=>BigInt(Byte.MaxValue)).product - r
    val rnd = Random.nextBytes(byteNumber).map(BigInt(_)).reduce(_*Byte.MaxValue+_)
    l+rnd-c
  }
  def primeIncreasing(s: BigInt, edge:BigInt):BigInt = s match {
    case s if s > edge=> s
    case s => {
      var rnd = bigRandomInt(s, 2*(2*s + 1))
      while (rnd % 2 != 0) rnd = bigRandomInt(s, 2*(2*s + 1))
      val candidate = s*rnd+1
    }
  }
}
