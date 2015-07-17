// MAP
def map(list: List[Int], op: Int => Int): List[Int] = {
  if (list.isEmpty) list
  else op(list.head) :: map(list.tail, op)
}
map(List(1,2,3), _*2)

// FILTER
def filter(list: List[Int], predicate: (Int) => Boolean): List[Int] = {
  def innerFilter(list: List[Int]): List[Int] = {
    if (list.isEmpty)
      list
    else if (predicate(list.head))
      list.head :: filter(list.tail, predicate)
    else
      filter(list.tail, predicate)
  }
  innerFilter(list)
}
filter(List(1, 2, 3, -10), i => i > 1)

// APPEND
def append(left: List[Int], right: List[Int]): List[Int] = {
  if (left.nonEmpty)
    left.head :: append(left.tail, right)
  else if (right.nonEmpty)
    right.head :: append(left, right.tail)
  else
    Nil
}
append(List(1, 2, 3), List(4, 5))