package AlgorithmsAndDataStructures.BinTree


/**
 * Обыкновенное бинарное дерево. Довольно наивное, но будет работать
 * О, к слову мне тут внезапно пригодился абстрактный класс, что в последний раз было... никогда.
 * Конкретно тут интерфейс не катит, так как нужно зафиксировать параметр типа, чтобы нельзя было в дерево
 * запихать значения разных типов. А интерфейс, к сожалению параметризовать нельзя. Кстати, я слышал,
 * что в будущих версиях scala это планируется изменить.
 */
abstract class NaiveNode[T]
case class NaiveBinTreeNode[T](left:NaiveNode[T],right:NaiveNode[T],value:T) extends NaiveNode[T]
case class NaiveBinTreeLeave[T](value:T) extends NaiveNode
class NaiveBinTree[T](val root:NaiveBinTreeNode[T]){
  def traverse:Seq[T] = {
    def traverseFromNode(node:NaiveNode[T]):Seq[T] = {
      case node:NaiveBinTreeNode[T] => traverseFromNode(node.left) ++ traverseFromNode(node.right).appended(node.value)
      case leave:NaiveBinTreeLeave[T] => Seq(leave.value)
    }
    traverseFromNode(root)
  }
}