/*
rough work
 */


// 0 1 1 2 3 5 8 13 21 34 55 89

def fibNum(a: Int, b: Int , previous: Int): Int = {
  val acc=b+previous // 1
  if a==0 then acc else fibNum(a-1, previous, acc)
}

//println(fibNum(9, 0, 1))

def fibRecursive(n: Int): Int = {
  if (n <= 1) n
  else fibRecursive(n - 1) + fibRecursive(n - 2)
}

//println(fibRecursive(3))

// palindrome

def palindrome(a: Int): Boolean = {
  def revTail(a: Int, b: Int = 0): Int = {
    val denom = a % 10
    println("denom : " + denom.toString())
    val remainedNum = a / 10
    println("remainedNum : " + remainedNum.toString())
    val rev = b * 10 + denom
    println("rev : " + rev.toString())
    if remainedNum <= 9 then rev * 10 + remainedNum else revTail(remainedNum, rev)
  }
  val rev = revTail(a)
  rev == a
}

println(palindrome(120221))




