import scala.annotation.tailrec

object PrintQueue:
  
  def getTotalMiddleAgePageNumbersFrom(rawData: List[String]): Long =
    val (pages, instructions) = buildInstructionsAndPages(rawData)
    pages.filter(isValidPage(instructions, _)).foldLeft(0L): (acc, page) =>
      page(page.length / 2).toLong + acc

  def getTotalMiddleAgePageNumbersOnceCorrectedFrom(rawData: List[String]): Long =
    val (pages, instructions) = buildInstructionsAndPages(rawData)
    @tailrec
    def correctInvalid(instructions: List[(String, String)], page: List[String]): List[String] =
      @tailrec
      def correctOnce(page: List[(String, Int)], acc: Option[(Int,Int)]): Option[(Int,Int)] =
        if page.isEmpty || acc.isDefined then
          acc
        else
          val e = page.head
          val index = instructions.filter(_._2 == e._1)
          .foldLeft(Option.empty[(String, Int)]): (acc, in) =>
            val found = page.tail.find(_._1 == in._1)
            if found.isDefined then found else acc
          index match
            case None => correctOnce(page.tail, acc)
            case Some(i) => Some(e._2, i._2)

      if isValidPage(instructions, page) then page
      else
        val substitution = correctOnce(page.zipWithIndex, Option.empty[(Int,Int)])
        substitution match
          case None => page
          case Some((origin, dest)) =>
            correctInvalid(instructions, page.updated(origin, page(dest)).updated(dest, page(origin)))

    pages.filter(!isValidPage(instructions, _)).map(correctInvalid(instructions, _))
    .foldLeft(0L): (acc, page) =>
        page(page.length / 2).toLong + acc

  private def buildInstructionsAndPages(rawData: List[String]) =
    val (i, p) = rawData.splitAt(rawData.indexOf(""))
    val pages = p.tail.map(_.split(",").toList)
    val instructions = i.map: r =>
      val List(first, second) = r.split("\\|").toList
      (first, second)
    (pages, instructions)

  @tailrec
  private def isValidPage(instructions: List[(String, String)], page: List[String]): Boolean =
    if page.isEmpty then true
    else
      val isValidNumber = instructions.filter(_._2 == page.head)
      .forall(i => !page.tail.contains(i._1))
      if isValidNumber then isValidPage(instructions, page.tail) else false