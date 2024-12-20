def squared_numbers(l: List[Any]): List[Any] = 
  l match {
    case Nil => Nil
    case (h: Int) :: t => Math.pow(h, 2) :: squared_numbers(t)
    case (h: Double) :: t => Math.pow(h, 2) :: squared_numbers(t)
    case (h: List[_]) :: t => squared_numbers(h) ::: squared_numbers(t)
    case _ :: t => squared_numbers(t)
  }

def intersect[A](a: List[A], b: List[A]): List[A] = {
  a match {
    case Nil => Nil
    case h :: t => if (b.contains(h)) h :: intersect(t, b) else intersect(t, b)
  }
}

def symmetric_difference[A](a: List[A], b: List[A]): List[A] = {
  val a_min_b = a.filterNot(b.contains)
  val b_min_a = b.filterNot(a.contains)
  a_min_b ::: b_min_a
}

