object HoofIt:

  def totalTrailheadsScoreIn(rawMap: List[String]): Long = totalTrailheads(rawMap).map(_.toSet).map(_.size).sum

  def totalTrailheadsScoreTwoIn(rawMap: List[String]): Long = totalTrailheads(rawMap).map(_.size).sum

  def totalTrailheads(rawMap: List[String]): Vector[List[(Int,Int)]] =
    def findTrailheads(startPoint: (Long, (Int, Int)), mapa: Vector[Vector[(Long, (Int, Int))]], acc: List[(Int, Int)]): List[(Int, Int)] =
      if startPoint._1 == 9L then startPoint._2 :: acc
      else
        val adjacentPoints = List((startPoint._2._1, startPoint._2._2 + 1), (startPoint._2._1 + 1, startPoint._2._2), (startPoint._2._1, startPoint._2._2 - 1), (startPoint._2._1 - 1, startPoint._2._2))
        val adjacent = adjacentPoints.filter(p => p._1 >= 0 && p._2 >= 0 && p._1 < mapa.size && p._2 < mapa(0).size).map: p =>
          mapa(p._1)(p._2)
        .filter(_._1 == startPoint._1 + 1L)
        if adjacent.isEmpty then acc else adjacent.foldLeft(acc)((a, n) => findTrailheads(n, mapa, a))

    val mapa = rawMap.zipWithIndex.toVector.map: row =>
      row._1.split("").toVector.zipWithIndex.map(col => (col._1.toLong, (row._2, col._2)))
    val startingPoints = mapa.flatten.filter(_._1 == 0L)
    startingPoints.map(s => findTrailheads(s, mapa, List()))