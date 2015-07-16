// MAP
// simple recursion
def map(list: List[Int], op: Int => Int): List[Int] = {
  if (list == null || list.isEmpty) return list

  op(list.head) :: map(list.tail, op)
}

// tail recursion
def map_tail(list: List[Int], op: Int => Int): List[Int] = {

  def innerMap(list: List[Int], result: List[Int]): List[Int] = {
    if (list == null || list.isEmpty) return result

    innerMap(list.tail, op(list.head) :: result)
  }
  innerMap(list.reverse, Nil)

}

map(List(1,2,3), _*2)
map_tail(List(1,2,3), _-7)

// FILTER
// tail recursion
def filter(list: List[Int], predicate: (Int) => Boolean): List[Int] = {

  def innerFilter(list: List[Int], result: List[Int]): List[Int] = {
    if (list == null || list.isEmpty) return result

    innerFilter(list.tail, if (predicate(list.head)) list.head :: result else result)
  }
  innerFilter(list.reverse, Nil)

}

filter(List(1, 2, 3, -10), i => i > 0)

