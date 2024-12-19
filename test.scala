def sum_evens(l: List[Int]): Int = {
  var sum = 0
  for (v <- l if v % 2 == 0) {
    sum += v
  }
  sum
}