package firstSteps

import scala.annotation.tailrec

object pg extends App{
  val rand = new scala.util.Random()
  //Ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //Не совсем натуральные числа
  val N = series[Int](0)(_ + 1)
  //Решето Эратосфена
  def erotothfenFilter: Int => Seq[Int] = n=> {
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
  //Первые сколько-то простых
  val somePrimeNumbers = erotothfenFilter(10000).drop(1)
  //Генерирование больших простых чисел с применением критерия полингтона
  def generateBigPrime(edge:BigInt):BigInt  = {
    
    // выбираем случайное простое число
    val initialPrime = somePrimeNumbers(rand.nextInt(somePrimeNumbers.length))

    //Получение большого случайного числа в границах от l до r
    def bigRandomInt(l:BigInt,r:BigInt)={
      val cut = r - l
      val cutBytes = cut.toByteArray
      val resultBytes = rand.nextBytes(cutBytes.length - 1)
      val cutLeadingByte = cutBytes.head
      val resultLeadingByte = cutLeadingByte.sign match {
        case 0 => 0.byteValue
        case 1 => rand.nextInt(cutLeadingByte).byteValue
        case -1 => rand.nextInt(255 + cutLeadingByte).byteValue
      }
      BigInt(resultBytes.prepended(resultLeadingByte))
    }

    //медленное возведение в степень
    def pow(x: BigInt, y: BigInt): BigInt = {
      var result:BigInt = 1
      var c = y
      while(c!=0) {
        result *= x
        c-=1
      }
      result
    }

    //увеличение числа с проверкой методом Поллингтона
    @tailrec
    def primeIncreasing(s: BigInt, edge: BigInt): BigInt = s match {
      //Если s > нужной нам границы - возвращаем s
      case s if s > edge => s
      case s =>
        //выбираем случайное чётное от s до 2(2s+1)
        var rnd = bigRandomInt(s, 2 * (2 * s + 1))
        while (rnd % 2 != 0) rnd = bigRandomInt(s, 2 * (2 * s + 1))
        //вычисляем число-кандидат на простоту
        val candidate = s * rnd + 1
        //если кандидат делится на простое и сам не является известным простым - начинаем сначала
        if (somePrimeNumbers.exists(candidate % _ == 0) && !somePrimeNumbers.contains(candidate))
          primeIncreasing(s, edge)
        else {
          //берём случайное от 1 до кандидата
          val a = bigRandomInt(1, candidate)
          //println(s"candidate = ${ candidate }")
          //println(s"a = ${ a }")
          //проверяем критерием Поллингтона, если всё ok
          if ((a.modPow(candidate - 1, candidate) == 1) && ((pow(a, rnd) - 1).gcd(candidate) == 1)) {
            primeIncreasing(candidate, edge)
          } else primeIncreasing(s, edge)
        }
    }
    primeIncreasing(initialPrime,edge)
  }
  //val rand = new scala.util.Random()
  val two:BigInt = 2
  val x = two pow 20
  println(s"x = ${x}")
  val prime = generateBigPrime(x)
  //val prime = BigInt.probablePrime(1024,rand)
  println(s"prime = ${prime}")
}
