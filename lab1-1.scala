def is_palindrome(s: String): Boolean = {
  def reverse_string(s: String): String = {
    def reverse(s: String, rev: String): String = 
      if (s.isEmpty()) rev
      else reverse(s.tail, s"${s.head}$rev")
    
    reverse(s, "")
  }

  s == reverse_string(s)
}

def is_an_anagram(s: String, l: List[String]): Boolean = {
  def remove_char(s: String, c: Char): String = {
    if (s.isEmpty()) s
    else
      if (s.head == c) s.tail
      else s"${s.head}${remove_char(s.tail, c)}"
  }

  def is_anagram(a: String, b: String): Boolean = {
    a.length() == b.length() && {
      var x = a
      for (i <- b) x = remove_char(x, i)
      x == ""
    }
  }

  l.exists(is_anagram(s, _))
} 

def factors(n: Int): Seq[Int] = {
  if (n <= 1) Seq()
  else for (
    x <- 2 until (Math.sqrt(n).toInt + 1)
    if n % x == 0 && factors(x) == Seq()
  ) yield x
}