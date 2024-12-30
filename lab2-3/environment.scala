package brainFuck

import scala.collection.mutable._

class BrainFuckEnvironment {
  private val data = Map[Int, Int]().withDefault(_ => 0)
  private var pointer = 0

  def incrementPointer(): Unit = { pointer += 1 }
  def decrementPointer(): Unit = {pointer -= 1 }
  def incrementData(): Unit = { data(pointer) += 1 }
  def decrementData(): Unit = { data(pointer) -= 1 }
  def get(): Int = data(pointer)
  def put(v: Int): Unit = { data(pointer) = v }

  override def toString(): String = s"pointer: $pointer, data: $data"
}