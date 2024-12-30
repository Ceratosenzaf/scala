package prettyPrint

class PrettyPrintTable(val header: List[String], val rows: List[List[String]]) {
  val newline = sys.props("line.separator")

  def getColMaxLength(i: Int): Int = (header(i) :: rows.map(_(i))).map(_.length).max
  
  def padString(str: String, len: Int): String = f"%%-${len}s".format(str)

  override def toString(): String = {
    def formatRow(row: List[String]): String = {
      row.zipWithIndex.map { case (col, i) =>
        padString(col, getColMaxLength(i))
      }.mkString("| ", " | ", " |")
    }

    val formattedHeader = formatRow(header)
    val formattedRows = rows.map(formatRow)
    val formattedLineSeparator = "-" * formattedHeader.length()

    val fullHeader =
      formattedLineSeparator + newline +
      formattedHeader + newline +
      formattedLineSeparator

    if (rows.isEmpty)
      fullHeader
    else 
      fullHeader + 
      formattedRows.mkString(newline, newline, newline) +
      formattedLineSeparator
  }
}