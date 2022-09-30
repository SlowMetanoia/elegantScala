package AlgorithmsAndDataStructures.BinTree

/**
 * 1.
 * Бинарное дерево, хранимое в массиве(Seq в данном случаем нужен для того, что конструкция была иммутабельной)
 * @param elements
 * @tparam T
 */
case class BTree[T]( elements:Seq[T]){
  val RIGHT = 1
  val LEFT = 0
  def root = elements.head
  def child(parent:Int,child:Int) = (parent+1)*2 + child
}
