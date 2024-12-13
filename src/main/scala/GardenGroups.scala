import scala.annotation.tailrec

object GardenGroups:

  def totalPrice(rawMap: List[String]): Long =
    getGroups(rawMap).map: group =>
      val perimeter = group.foldLeft(0): (acc, elem) =>
        val res = 4 - List((elem._2._1 + 1, elem._2._2), (elem._2._1 - 1, elem._2._2), (elem._2._1, elem._2._2 + 1), (elem._2._1, elem._2._2 - 1)).count(p => group.exists(a => a._2 == p))
        res + acc
      val area = group.size
      area.toLong * perimeter.toLong
    .sum

  def totalPriceWithNumberOfSide(rawMap: List[String]): Long =
    def findPerimeterLines(elem: (String, (Int,Int)), borders: List[(String, (Int,Int))], acc: List[(String, (Int,Int))]): List[(String, (Int,Int))] =
      val adjacent = List((elem._1,(elem._2._1 + 1, elem._2._2)), (elem._1, (elem._2._1 - 1, elem._2._2)), (elem._1, (elem._2._1, elem._2._2 + 1)), (elem._1, (elem._2._1, elem._2._2 - 1))).filter(borders.contains(_))
      if adjacent.isEmpty then
        acc
      else
        adjacent.foldLeft(acc)((a, p) => findPerimeterLines(p, borders.filter(_ != elem), p :: a))

    getGroups(rawMap).map: group =>
      val pointsInPerimeter = group.foldLeft(List[(String, (Int,Int))]()): (acc, elem) =>
        List(("d",(elem._2._1 + 1, elem._2._2)), ("u", (elem._2._1 - 1, elem._2._2)), ("r", (elem._2._1, elem._2._2 + 1)), ("l", (elem._2._1, elem._2._2 - 1))).filter(p => !group.exists(a => a._2 == p._2)) ++ acc
      val perimeter = pointsInPerimeter.foldLeft(List[(String, (Int,Int))]()): (acc, elem) =>
        if List((elem._1, (elem._2._1, elem._2._2 + 1)), (elem._1, (elem._2._1, elem._2._2 - 1)), (elem._1, (elem._2._1 + 1, elem._2._2)), (elem._1, (elem._2._1 - 1, elem._2._2))).forall(p => !pointsInPerimeter.contains(p)) then
          elem :: acc
        else
          val line = findPerimeterLines(elem, pointsInPerimeter.filter(_ != elem), List(elem))
          if line.exists(p => acc.contains(p)) then acc else elem :: acc
      .size.toLong
      val area = group.size.toLong
      area * perimeter
    .sum

  private def getGroups(rawMap: List[String]) =
    val groupsMap = rawMap.zipWithIndex.map(row => row._1.split("").toList.zipWithIndex.map(col => (col._1, (row._2, col._2))))
    groupsMap.flatten.groupBy(_._1).toList.flatMap(g => groupBy(g._2, List()))

  private def areAdjacent(p1: (Int, Int), p2: (Int, Int)) =
    (p1._1 + 1 == p2._1 && p1._2 == p2._2) || (p1._1 - 1 == p2._1 && p1._2 == p2._2) || (p1._2 + 1 == p2._2 && p1._1 == p2._1) || (p1._2 - 1 == p2._2 && p1._1 == p2._1)

  private def adjacent(point: (String, (Int, Int)), points: List[(String, (Int, Int))], acc: Set[(String, (Int, Int))]): Set[(String, (Int, Int))] =
    if points.isEmpty then
      acc
    else
      val ad = points.filter(p => areAdjacent(p._2, point._2) && !acc.contains(p)).toSet
      if ad.isEmpty then
        acc
      else
        ad.foldLeft(acc): (ac, a) =>
          adjacent(a, points.filter(_ != a), ac + a)

  @tailrec
  private def groupBy(group: List[(String, (Int, Int))], acc: List[Set[(String, (Int, Int))]]): List[Set[(String, (Int, Int))]] =
    if group.isEmpty then
      acc
    else
      val ad = adjacent(group.head, group.tail, Set(group.head))
      groupBy(group.filter(!ad.contains(_)), ad :: acc)