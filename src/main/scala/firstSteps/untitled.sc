/*
def f:Int=>Int = n=> math.round(math.exp(n)).toInt
def period:(Int=>Int)=>Int=>LazyList[Int] = f=>n=> f(n) #:: period(f)(f(n))

val values = period(f)(2)

values.take(6).force
*/
for (n<-2 to 6) yield math.round(math.exp(n)).toInt
