// simple recursion
def map(list: List[Int], op: Int => Int): List[Int] = {
  if (list == null || list.isEmpty) return list

  op(list.head) :: map(list.tail, op)
}

// tail recursion
def map_tail(list: List[Int], op: Int => Int): List[Int] = {
  val acc = List()
  def innerMap(list: List[Int], acc: List[Int]): List[Int] = {
    if (list == null || list.isEmpty) return acc

    innerMap(list.tail, op(list.head) :: acc)
  }
  innerMap(list.reverse, acc)
}

map(List(1,2,3), _*2)
map_tail(List(1,2,3), _*2)