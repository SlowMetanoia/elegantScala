import scala.annotation.tailrec


/**
 * Как-то писал я лабу по защите информации и надо мне было генерировать большие простые числа
 * @param edge - величина, которую должно превышать искомое простое
 */

//Генерирование больших простых чисел с применением критерия полингтона
def generateBigPrime(edge:BigInt):BigInt  = {
  val rand = new scala.util.Random()
  //Ряды
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

  //Первые сколько-то простых
  val somePrimeNumbers = erotothfenFilter(100000).drop(1)

  // выбираем случайное простое число
  val initialPrime = somePrimeNumbers(rand.nextInt(somePrimeNumbers.length))

  //Получение большого случайного числа в границах от l до r
  def bigRandomInt(l: BigInt, r: BigInt): BigInt = {
    val x2 = series[BigInt](1)(_ * 2)
    val length = r - l
    val byteNumber = x2.find(_ > length).get.toInt
    val c = (1 to byteNumber).map(x => BigInt(Byte.MaxValue)).product - r
    val rnd = rand.nextBytes(byteNumber).map(BigInt(_)).reduce(_ * Byte.MaxValue + _)
    l + rnd - c
  }

  //медленное возведение в степень
  def pow(x: BigInt, y: BigInt): BigInt = {
    var c = y
    var z: BigInt = 1
    while (c != 0) {
      z = z * x
      c = c - 1
    }
    z
  }

  //увеличение числа с проверкой методом Полингтона
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
        //проверяем критерием Полингтона, если всё ok
        if ((a.modPow(candidate - 1, candidate) == 1) && ((pow(a, rnd) - 1).gcd(candidate) == 1)) {
          primeIncreasing(candidate, edge)
        } else primeIncreasing(s, edge)
      }
  }
  primeIncreasing(initialPrime,edge)
}

val two:BigInt = 2
val x = two pow 30
val prime = generateBigPrime(x)

//todo: дописать проверку на то, что алгоритм действительно работает

/**
 * В итоге, пока я искал способ по-нормальному проверить, работает ли алгоритм, я нашёл генерацию с.ч.
 * из из java, реализованную из коробки. Самый ценный вывод, который только можно сделать из этого участка.
 */

