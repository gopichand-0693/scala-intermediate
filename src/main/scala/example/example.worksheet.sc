import scala.annotation.tailrec

/*
Functional programming
*/


// pascal triangle

println("pascal triangle")

def pascal(c:Int, r: Int):Int =
  if c == 0 || c == r then 1 else pascal(c-1,r-1)+pascal(c,r-1)
println(pascal(3,3))


// calculating square root using the concepts of recursion

def sqrtIter(number:Double) = {
  def abs(x:Double) =
    if (x < 0)
      -x
    else x

  val estimation = 1.0
  if (number < 0)
    println("no root for negative numbers")
    -0.0
  else
    val result :Double = calculateMean(number, estimation)

  @tailrec
  def calculateMean(number: Double, estimation: Double): Double = {
    val mean:Double = (estimation + number / estimation) / 2
    if abs(mean-estimation) <0.01 then mean else calculateMean(number :Double,mean:Double)
  }
}
val sqrt = sqrtIter(-9)
println("sqrt of above number is "+sqrt)


// calculating factorial

def factorial(x:Int):Int =
  if x==0 then 1 else x*factorial(x-1)
println(factorial(5))

def fact(n: Int): BigInt = {
  @tailrec
  def iter(a: Int, acc: Int): Int={
    if a <= 1 then acc else iter(a-1,acc*a)
  }
  iter(n, 1)
}
println(fact(20))


// calculating fibonacci

def fibRecursive(n: Int): Int = {
  if (n <= 1) n
  else fibRecursive(n - 1) + fibRecursive(n - 2)
}
//println(fibRecursive(3))

@tailrec
def fibNum(a: Int, b: Int , previous: Int): Int = {
  val acc=b+previous // 1
  if a==0 then acc else fibNum(a-1, previous, acc)
}
//println(fibNum(9, 0, 1))


// calculating GCD

@tailrec
def gcd(x: Int, y:Int):Int =
  if y==0 then
    x
  else
    gcd(y,x%y)
println(gcd(6,2))

// palindrome

def palindrome(a: Int): Boolean = {
  @tailrec
  def revTail(a: Int, b: Int = 0): Int = {
    val denom = a % 10
    val remainedNum = a / 10
    val rev = b * 10 + denom
    //println("rev : " + rev.toString())
    if remainedNum <= 9 then rev * 10 + remainedNum else revTail(remainedNum, rev)
  }
  val rev = revTail(a)
  rev == a
}

println(palindrome(12021))

/*---------------------           currying       -----------------------------*/

def cube(a:Int):Int =
  a*a*a

def sumInts(a:Int, b:Int):Int =
  if a>b then 0 else a+sumInts(a+1,b)

def sumCubes(a:Int, b:Int):Int =
  if a>b then 0 else cube(a)+sumCubes(a+1,b)

def sumFactorial(a:Int, b:Int):Int =
  if a>b then 0 else factorial(a) + sumFactorial(a+1,b)

println("sum of Integers: "+sumInts(2,4))
println("sum of Cubes: "+sumCubes(2,4))
println("sum of Factorials: "+sumFactorial(2,4))

def sum(f: (Int,Int) => Int, a:Int,b:Int) : Int =
  f(a, b)

println("complex of sumInts: "+sum(sumInts,2,4))
println("complex of sumCubes: "+sum(sumCubes,2,4))
println("complex of sumFactorials: "+sum(sumFactorial,2,4))

// curried

def sum(f:Int => Int):(Int, Int)=> Int =
  def sumF(a: Int, b: Int): Int =
    if a>b then 0 else f(a) + sumF(a+1,b)
  sumF
// or in a proper curried syntax special as below

def sum_(f:Int => Int)(a: Int, b: Int): Int =
  if a>b then 0 else f(a) + sum_(f)(a+1, b)

def f_(a:Int):Int = a
sum_(f_)(1,3)

// tail recursion is the one where last call of the function is itself
// gcd function for more information refer the notes

// write the tail recursive for the sumInts, sumCubes, SumFacts

def sum(f:Int => Int, a: Int, b: Int): Int =
  def loop(a: Int, acc: Int): Int =
    if a > b then acc else loop(a+1, acc + f(a))
  loop(a,0)

sum(f_,1,3)

def f(a: String)(b: Int)(c: Boolean): String =
  "(" + a + ", " + b + ", " + c + ")"

val g =f("Hi")
val h = g(2)
val i = h(false)
println(i)
print(f("Hello")(1)(true))

/*
Classes and objects
*/

/*---------------------           rational class       -----------------------------*/

class Rational(x:Int, var y:Int):
  def numer = x
  val denom = y

  def addRational(r:Rational):Rational =
    Rational(this.x*r.denom+this.y*r.numer,this.y*r.denom)
    //Rational(this.x*r.y+this.y*r.x,this.y*r.y)
    // we can directly use x, y parameter if we case classes

  def neg():Rational=
    Rational(-1*numer,this.denom)

  override def toString: String = s"$numer/$denom"

val x = Rational(1,2)
val y = Rational(1,3)

println("adding Rational: "+x.addRational(y))

println("neg Rational: "+x.neg())


/* ---------------------             abstraction        ----------------------------- */

class Rationals( x:Int, y:Int):
  require(y>0, "denom must be possitive")

  private def gcd(a:Int, b:Int):Int =
    if b==0 then a else gcd(b,a%b)
  private val g = gcd(x,y)

  def numer = x/g
  def denom = y/g

  def less(that:Rationals):Boolean =
    numer*that.denom < denom+that.numer

  def max(that:Rationals):Rationals =
    if this.less(that) then that else this
end Rationals

val obj1 = Rationals(64,32)

println("obj1.numer: "+obj1.numer)
println("obj1.denom: "+obj1.denom)


extension(r:Rationals)
  def stringForm = s"${r.numer}/${r.denom}"  // 'this' cannot be used here instead navigate substition using 'r'
  infix def min(s: Rationals): String =      // used infix key word
    if r.less(s) then r.stringForm else s.stringForm
  def abs =
    Rationals(r.numer.abs,r.denom.abs)

println(Rationals(1,2).min(Rationals(2,3)))

println(Rationals(1,2) min Rationals(2,3)) // infix notation


//      abstract classes
//     {1,2,4,5}
//                         4
//                      /    \
//                  1            5
//                /  \         /  \
//             emp     2    emp   emp
//                    /  \
//                  emp   emp


abstract class IntSet:               // class is abstract
  def contains(x:Int):Boolean        // no implementation of contains or incl
  def incl(x:Int): IntSet

// there are two scenarios possible
// 1) tree for the empty set
// 2) tree consisting of integer and two sub trees as shown in the above example

// lets put these two definitions and implement them

class Empty extends IntSet:
  def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = NonEmpty(x, Empty(), Empty()) // override key word is for the already implemented method in super class


class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet:
  override def contains(x: Int): Boolean =
    if x < element then
      left.contains(x)
    else if x > element then
      right.contains(x)
    else true

  override def incl(x: Int): IntSet =
    if x < element then
      NonEmpty(element, left.incl(x), right)
    else if x > element then
      NonEmpty(element, left, right.incl(x))
    else
      this
end NonEmpty

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

@tailrec
def nth[T](xs:List[T], n: Int): T =
  if xs.isempty then throw IndexOutOfBoundsException()
  else if n == 0 then xs.head
  else nth(xs.tail,n-1)

nth(Cons(1,Cons(2,Cons(3,Nil_()))),2)

/*
Generic functions
*/
val s = "()fg".toList
println(s.tail.tail.tail.tail.isEmpty)

// like classes functions can also take the type parameter

// for instance here is a function that creates a list consisting of a single element

def singleton[T](elem: T) =
  Cons[T](elem, Nil_[T])

// we can then write
val a = singleton[Int](1)
val b = singleton(true)

// Decomposition

trait Expr:
  def isNumber: Boolean
  def isSum:Boolean
  def leftOper: Expr
  def rightOper: Expr
  def numValue: Int

class Number(n: Int) extends Expr:
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def leftOper: Expr = throw Error("Number.leftOp")
  def rightOper: Expr = throw Error("Number.rightOper")
  def numValue: Int = n

class isSum(e1: Expr, e2: Expr) extends Expr:
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def leftOper: Expr = e1
  def rightOper: Expr = e2
  def numValue: Int = throw Error("Sum.numValue")




