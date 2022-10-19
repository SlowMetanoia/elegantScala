
def lazyListSeries[T](prev:T,next:T=>T):LazyList[T] = prev#::lazyListSeries(next(prev),next)
def lazyListIterator[T](prev:T,nextElem:T=>T):Iterator[T] = new Iterator[T]{
  var current = prev
  override def hasNext = true
  override def next() = {
    val old = current
    current = nextElem(current)
    current
  }
}

val s0 = lazyListSeries[Int](0,_+1)
val s1 = lazyListIterator[Int](0,_+1)

s0.take(10)
s1.take(10).toSeq