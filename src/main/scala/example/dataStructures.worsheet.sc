
/*
data structures
 */

/*
trees
 */

//     {1,2,4,5}
//                         4
//                      /    \
//                  1            5
//                /  \         /  \
//             emp     2    emp   emp
//                    /  \
//                  emp   emp

println("Hello Illustrations")

abstract class IntSet:
  def contains(x: Int): Boolean
  def include(x: Int) : IntSet

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
  override def contains(x: Int): Boolean = {
    if x == elem then true
    else if elem < x then left.contains(x)
    else right.contains(x)
  }

  override def include(x: Int): IntSet = {
    if x < elem then NonEmpty(elem, left.include(x), right)
    else if x > elem then NonEmpty(elem, left, right.include(x))
    else this
  }


class Empty extends IntSet:
  override def contains(x: Int): Boolean = false
  def include(x: Int): IntSet =  NonEmpty(x, Empty(), Empty())

/*
linked lists
 */

//      immutable linked list list(1,2,3) which consists of
//      nil
//      cons

//                          cons(1, cons(2, cons(3, nil)))
//
//                           * -- * -- * --Nil
//                           |    |    |
//                           1    2    3


trait List[T]:
  def isempty:Boolean
  def head: T
  def tail: List[T]

class Cons[T](_head: T, _tail: List[T]) extends List[T]:
  def isempty = false
  def head = _head
  def tail = _tail

// or can also be written as
// class Cons[T]( val head: Int, val tail: Int) extends List[T]:    replacing the line 197 - 200
// params are also taken as fields in the constructor

class Nil_[T] extends List[T]:
  def isempty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

def nth[T](xs:List[T], n: Int ): T =
  if xs.isempty then throw IndexOutOfBoundsException()
  else if n == 0 then xs.head
  else nth(xs.tail,n-1)

nth(Cons(1,Cons(2,Cons(3,Nil_()))),2)


/*
Generic functions
*/
val s = "()fg".toList
println(s.tail.tail.tail.tail.isEmpty)




