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

def fold[A](init: A, f: (A, A) => A, l:List[A]): A =
  l match {
    case Nil => init
    case hd :: tl => f(hd, fold(init, f, tl))
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

def indexOf[A](e: A, l: List[A]): Int = {
  def getIndex(l: List[A], i: Int): Int =
    l match {
      case Nil => -1
      case hd :: tl => if (hd == e) i else getIndex(tl, i + 1)
    }
  getIndex(l, 0)
}

def indices[A](l: List[A]) =
  for (i <- 0 until l.length) yield i

def init[A](l: List[A]): List[A] =
  l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case hd :: Nil => Nil
    case hd :: tl => hd :: init(tl)
  }

def isDefinedAt[A](l: List[A], i: Int): Boolean =
  i >= 0 && i < l.length

def isEmpty[A](l: List[A]): Boolean =
  l.length == 0

def last[A](l: List[A]): A =
  l match {
    case Nil => throw new NoSuchElementException("last of empty list")
    case hd :: Nil => hd
    case hd :: tl => last(tl)
  }

def lastOption[A](l: List[A]): Option[A] =
  l match {
    case Nil => None
    case hd :: Nil => Some(hd)
    case hd :: tl => lastOption(tl)
  }

def lastIndexOf[A](e: A, l: List[A]): Int = {
  def getLastIndex(l: List[A], i: Int): Int =
    l match {
      case Nil => -1
      case hd :: tl => {
        val j = getLastIndex(tl, i + 1)
        if (j == -1) if (hd == e) i else -1 else j
      }
    }
  getLastIndex(l, 0)
}

def length[A](l: List[A]): Int = {
  def len(l: List[A], n: Int): Int =
    l match {
      case Nil => n
      case hd :: tl => len(tl, n + 1)
    }
  len(l, 0)
}

def map[A,B](f: A => B, l: List[A]): List[B] = 
  l match {
    case Nil => Nil
    case hd :: tl => f(hd) :: map(f, tl)
  }

def max[A](l: List[A])(implicit ord: Ordering[A]): A =
  l match {
    case Nil => throw new UnsupportedOperationException("max of empty list")
    case hd :: tl => tl.foldLeft(hd)((a, b) => ord.max(a, b))
  }

def min[A](l: List[A])(implicit ord: Ordering[A]): A =
  l match {
    case Nil => throw new UnsupportedOperationException("min of empty list")
    case hd :: tl => tl.foldLeft(hd)((a, b) => ord.min(a, b))
  }

def partition[A](p: A => Boolean, l: List[A]): (List[A], List[A]) =
  l match {
    case Nil => (Nil, Nil)
    case hd :: tl =>
      val (a, b) = partition(p, tl)
      if (p(hd)) (hd :: a, b) else (a, hd :: b)
  }

def product(l: List[Int]): Int =
  fold(1, (a: Int,b: Int) => a*b, l)

def reduce[A](f: (A, A) => A, l: List[A]): A =
  l match {
    case Nil => throw new UnsupportedOperationException("reduce of empty list")
    case hd :: tl => fold(hd, f, tl)
  }

def reverse[A](l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case hd :: tl => reverse(tl) ::: List(hd)
  }

def slice[A](from: Int, to: Int, l: List[A]): List[A] =
  (from, to, l) match {
    case (_, _, Nil) => Nil
    case (0, 0, _) => Nil
    case (0, _, hd :: tl) => hd :: slice(0, to - 1, tl)
    case (_, _, hd :: tl) => slice(from - 1, to - 1, tl)
  }

def splitAt[A](n: Int, l: List[A]): (List[A], List[A]) =
  (n, l) match {
    case (0, _) => (Nil, l)
    case (_, Nil) => (Nil, Nil)
    case (_, hd :: tl) =>
      val (a, b) = splitAt(n - 1, tl)
      (hd :: a, b)
  }

def sum(l: List[Int]): Int =
  fold(0, (a: Int,b: Int) => a+b, l)

def tail[A](l: List[A]): List[A] =
  l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case hd :: tl => tl
  }

def take[A](n: Int, l: List[A]): List[A] =
  (n, l) match {
    case (0, _) => Nil
    case (_, Nil) => Nil
    case (_, hd :: tl) => hd :: take(n - 1, tl)
  }