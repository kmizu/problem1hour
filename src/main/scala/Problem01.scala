object Problem01 {
  def sumForLoop(list: List[Int]): Int = {
    var sum = 0
    for(x <- list) sum += x
    sum
  }
  def sumWhileLoop(list: List[Int]): Int = {
    var sum = 0
    var lst: List[Int] = Nil
    while(!lst.isEmpty) {
      sum += lst.head
      lst = lst.tail
    }
    sum
  }

  def sumRecursion(list: List[Int]): Int = list match {
    case Nil => 0
    case x::xs => x + sumRecursion(xs)
  }
}
