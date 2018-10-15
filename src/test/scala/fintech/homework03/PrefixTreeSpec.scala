package fintech.homework03

import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int]( None,Map.empty)

    val with42: Tree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be(42)

    val withDouble: Tree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be(42)
    withDouble.sub("ab").sub("cde").get should be(13.0)
  }

  it should "not double data" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](None, Map.empty)

    val res = tree
      .put("a", 1)
      .put("bc", 2)

    assertThrows[NoSuchElementException] {
      res.sub("ba").get
    }
  }
  

  "Equal" should "equal subtrees and equal value" in {
    val tree: Tree[Char, Int] = new Tree[Char, Int](None,Map.empty)

    val tree1: Tree[Char, Int] = tree.put("ab", 5)
    val tree2: Tree[Char, Int] = tree.put("ab", 5)
    val tree3: Tree[Char, Int] = tree.put("ab", 8)
    val tree4: Tree[Char, Int] = tree.put("ac", 5)

    tree1 == tree2 should be(true)
    tree3 == tree2 should be(false)
    tree4 == tree2 should be(false)
  }
}
