package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]

  def get: V
}

class Tree[K, +V](val value: Option[V],val trees: Map[K, Tree[K, V]]) extends PrefixTree[K, V] {

  def put[U >: V](path: Seq[K], value: U): Tree[K, U] = {
    if (path.isEmpty) new Tree[K, U](Some[U](value),trees)
    else if (trees.contains(path.head)) {
      val subTree = trees(path.head).put(path.tail, value)
      new Tree(this.value,trees + (path.head -> subTree))
    }
    else new Tree[K, U]( this.value,trees + (path.head -> put(path.tail, value)),)
  }

  override def hashCode(): Int = {
    val prime = 97
    var hash = 1
    hash = prime * hash + trees.hashCode()
    hash = prime * hash + value.hashCode()
    hash
  }

  override def equals(a: Any): Boolean = {
    a match {
      case tree: Tree[K, V] => tree.trees == trees && tree.value.getOrElse() == value.getOrElse()
      case _ => false
    }
  }

  def sub(path: Seq[K]): Tree[K, V] = {
    if (path.isEmpty) this
    else if (trees.contains(path.head))
      trees(path.head).sub(path.tail)
    else new Tree[K, V](None,Map.empty)
  }

  def get: V = value.get
}
