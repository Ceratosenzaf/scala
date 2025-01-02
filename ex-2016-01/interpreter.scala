package longLang

import java.io.{File,FileInputStream,FileOutputStream}

object LongLangInterpreter {
  def remove(a: String) = {
    new File(a).delete()
  }
  def rename(a: String, b: String) = {
    new File(a).renameTo(new File(b))
  }
  def merge(a: String, b: String, c: String) = {
    val f = new FileOutputStream(c)
    f.write(new FileInputStream(a).readAllBytes())
    f.write(new FileInputStream(b).readAllBytes())
  }
  def backup(a: String, b: String) = {
    new FileOutputStream(b).write(new FileInputStream(a).readAllBytes())
  }
}