import scala.annotation.tailrec

object RedNosedReports:
  def countSafeReportsIn(rawData: List[String]): Int =
    buildDataSet(rawData).count: level =>
      isSafe(level.tail, level.head, level.head > level.tail.head, true)

  def countSafeReportsWithProblemDamperIn(rawData: List[String]): Int =
    buildDataSet(rawData).count: level =>
      val levelWithIndex = level.zipWithIndex
      levelWithIndex.foldLeft(List[List[Int]](level)): (acc, l) =>
        levelWithIndex.filter(_._2 != l._2).map(_._1) :: acc
      .count: l =>
        isSafe(l.tail, l.head, l.head > l.tail.head, true)
      > 0

  private def buildDataSet(rawData: List[String]) = rawData.map:
      str => str.split(" ").toList.map(_.toInt)

  @tailrec
  private def isSafe(level: List[Int], head: Int, isDescendingOrder: Boolean, acc: Boolean): Boolean =
    if level.isEmpty then acc
    else if !acc then acc
    else if isDescendingOrder then
      isSafe(level.tail, level.head, isDescendingOrder, head > level.head && (head - level.head) <= 3)
    else
      isSafe(level.tail, level.head, isDescendingOrder, head < level.head && (level.head - head) <= 3)
