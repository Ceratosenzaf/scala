class Editor {
  var cursor: Int = 0
  var text: String = ""

  def x = {
    text = text.substring(0, cursor) + text.substring(cursor + 1)
    if (cursor > text.length) cursor = text.length
  }

  def dw = {
    val i = text.indexOf(" ", cursor)
    text = text.substring(0, cursor) + text.substring(i)
    if (cursor > text.length) cursor = text.length
  }

  def i(c: Char) = {
    text = text.substring(0, cursor) + c + text.substring(cursor)
    cursor += 1
  }

  def iw(s: String) = {
    text = text.substring(0, cursor) + s + text.substring(cursor)
    cursor += s.length
  }

  def l(n: Int = 1) = {
    cursor += n
    if (cursor > text.length) cursor = text.length
  }

  def h(n: Int = 1) = {
    cursor -= n
    if (cursor < 0) cursor = 0
  }

  override def toString: String = s"$text\nCursor at: $cursor"
}

trait Debug extends Editor {
  abstract override def x = {
    println(s"Executing x at cursor $cursor")
    super.x
    println(this)
  }

  abstract override def dw = {
    println(s"Executing dw at cursor $cursor")
    super.dw
    println(this)
  }

  abstract override def i(c: Char) = {
    println(s"Executing i('$c') at cursor $cursor")
    super.i(c)
    println(this)
  }

  abstract override def iw(w: String) = {
    println(s"Executing iw('$w') at cursor $cursor")
    super.iw(w)
    println(this)
  }

  abstract override def l(n: Int = 1) = {
    println(s"Executing l($n) at cursor $cursor")
    super.l(n)
    println(this)
  }

  abstract override def h(n: Int = 1) = {
    println(s"Executing h($n) at cursor $cursor")
    super.h(n)
    println(this)
  }
}

trait UndoRedo extends Editor {
  private var history: List[(String, Int)] = List()
  private var future: List[(String, Int)] = List()

  abstract override def x = {
    saveState(this)
    super.x
    clearFuture
  }

  abstract override def dw = {
    saveState(this)
    super.dw
    clearFuture
  }

  abstract override def i(c: Char) = {
    saveState(this)
    super.i(c)
    clearFuture
  }

  abstract override def iw(w: String) = {
    saveState(this)
    super.iw(w)
    clearFuture
  }

  abstract override def l(n: Int = 1) = {
    saveState(this)
    super.l(n)
    clearFuture
  }

  abstract override def h(n: Int = 1) = {
    saveState(this)
    super.h(n)
    clearFuture
  }

  def u = {
    if (history.nonEmpty) {
      future = (text, cursor) :: future
      val (prevText, prevCursor) = history.head
      text = prevText
      cursor = prevCursor
      history = history.tail
    }
  }

  def ctrlr = {
    if (future.nonEmpty) {
      history = (text, cursor) :: history
      val (nextText, nextCursor) = future.head
      text = nextText
      cursor = nextCursor
      future = future.tail
    }
  }

  private def saveState(instance: Editor) = {
    history = (instance.text, instance.cursor) :: history
  }

  private def clearFuture = {
    future = List()
  }
}

class EditorWithDebug extends Editor with Debug
class EditorWithUndoRedo extends Editor with UndoRedo
class EditorWithDebugAndUndoRedo extends Editor with Debug with UndoRedo