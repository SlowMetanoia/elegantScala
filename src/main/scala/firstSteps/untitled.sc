/*
def f:Int=>Int = n=> math.round(math.exp(n)).toInt
def period:(Int=>Int)=>Int=>LazyList[Int] = f=>n=> f(n) #:: period(f)(f(n))

val values = period(f)(2)

values.take(6).force
*/



case
class CQCHierarchy( childType: String, parentType: String)
for (n<-2 to 6) yield math.round(math.exp(n)).toInt

val entities: Set[CQCHierarchy] = Set(
  CQCHierarchy("A", "B"),
  CQCHierarchy("C", "A"),
  CQCHierarchy("D", "A"),
  CQCHierarchy("E", "A")
  )
println(entities)
println(entities.map(row => (row.parentType, row.childType)))
val res = entities.map(row => (row.parentType, row.childType)).toMap
println(res)

val res = Map.from(entities.map(row => (row.parentType, row.childType)))

val mp = entities.groupBy(_.parentType).map {
  case (parent, childSet) => parent -> childSet.map(_.childType)
}
val mp2 = entities.groupMap(_.parentType)(_.childType)

/*
entities.foreach {
  case CQCHierarchy(childType, parentType) =>
    mp = mp + (parentType -> mp.getOrElse(parentType, Seq()).appended(childType))
  //mp = mp + (parentType->(mp(parentType).appended( childType)))
}*/
mp

Seq[Int]().reduce(_*_)
