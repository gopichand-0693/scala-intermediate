/*
rough work
 */


// 0 1 1 2 3 5 8 13 21 34 55 89

def fibNum(a: Int, b: Int , previous: Int): Int = {
  val acc=b+previous // 1
  if a==0 then acc else fibNum(a-1, previous, acc)
}

println(fibNum(9, 0, 1))

def fibRecursive(n: Int): Int = {
  if (n <= 1) n
  else fibRecursive(n - 1) + fibRecursive(n - 2)
}

println(fibRecursive(3))
