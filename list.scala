def map[A,B](f: A => B, l: List[A]): List[B] = 
  l match {
    case Nil => Nil
    case hd :: tl => f(hd) :: map(f, tl)
  }

def contains[A](e: A, l: List[A]): Boolean =
  l match {
    case Nil => false
    case hd :: tl => e == hd || contains(e, tl)
  }

def corresponds[A,B](p: (A,B) => Boolean, a: List[A], b: List[B]): Boolean = 
  (a,b) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => false
    case (h1 :: t1, h2 :: t2) => p(h1, h2) && corresponds(p, t1, t2) 
  }

def count[A](p: A => Boolean, l: List[A]): Int = 
  l match {
    case Nil => 0
    case hd :: tl => (if (p(hd)) 1 else 0) + count(p, tl)
  }

def distinct[A](l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case hd :: tl => if (contains(hd, tl)) distinct(tl) else hd :: distinct(tl)
  }

def drop[A](n: Int, l: List[A]): List[A] =
  (n, l) match {
    case (0, _) => l
    case (_, Nil) => Nil
    case (_, _ :: tl) => drop(n - 1, tl)
  }

def exists[A](p: A => Boolean, l: List[A]): Boolean =
  l match {
    case Nil => false
    case hd :: tl => p(hd) || exists(p, tl)
  }

def filter[A](p: A => Boolean, l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case hd :: tl => if (p(hd)) hd :: filter(p, tl) else filter(p, tl)
  }

def filterNot[A](p: A => Boolean, l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case hd :: tl => if (p(hd)) filterNot(p, tl) else hd :: filterNot(p, tl)
  }

def find[A](p: A => Boolean, l: List[A]): Option[A] = 
  l match {
    case Nil => None
    case hd :: tl => if (p(hd)) Some(hd) else find(p, tl)
  }

def flatten(l: List[Any]): List[Any] =
  l match {
    case Nil => Nil
    case (hd: List[_]) :: tl => flatten(hd) ::: flatten(tl)
    case (hd) :: tl => hd :: flatten(tl)
  }

def forall[A](p: A => Boolean, l: List[A]): Boolean =
  l match {
    case Nil => true
    case hd :: tl => p(hd) && forall(p, tl)
  }

def foreach[A](p: A => Unit, l: List[A]): Unit =
  l match {
    case Nil => ()
    case hd :: tl => p(hd); foreach(p, tl)
  }

def groupBy[A,B](f: A => B, l: List[A]) = {
  def group(l: List[A], m: Map[B, List[A]]): Map[B, List[A]] =
    l match {
      case Nil => m
      case hd :: tl =>
        val k = f(hd)
        val new_m = m.updated(k, hd :: m.getOrElse(k, Nil))
        group(tl, new_m)
    }
  group(l, Map())
}

def head[A](l: List[A]): A =
  l match {
    case Nil => throw new NoSuchElementException("head of empty list")
    case hd :: tl => hd
  }

def headOption[A](l: List[A]): Option[A] =
  l match {
    case Nil => None
    case hd :: tl => Some(hd)
  }