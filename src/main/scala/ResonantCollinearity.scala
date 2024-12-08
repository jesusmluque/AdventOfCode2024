object ResonantCollinearity:

  def countAntinodesIn(rawAntennas: List[String]): Int =
    val antennas = buildAntennasPlaces(rawAntennas)
    val limits = (rawAntennas.size, rawAntennas.head.length)
    antennas.foldLeft(Set[(String, (Int,Int))]()): (acc, antenna) =>
      val sameAntennas = getAllAntennasSameAs(antenna, antennas)
      sameAntennas.foldLeft(acc): (acc2, ant) =>
        val diff = (antenna._2._1 - ant._2._1, antenna._2._2 - ant._2._2)
        List(("#", (antenna._2._1 + diff._1, antenna._2._2 + diff._2)), ("#", (ant._2._1 - diff._1, ant._2._2 - diff._2)))
          .filter: antiNode =>
            antiNode._2._1 >= 0 && antiNode._2._1 < limits._1 && antiNode._2._2 >= 0 && antiNode._2._2 < limits._2
        .toSet ++ acc2
    .size

  def countAntinodesWithReasoningIn(rawAntennas: List[String]): Int =
    val antennas = buildAntennasPlaces(rawAntennas)
    val limits = (rawAntennas.size, rawAntennas.head.length)
    antennas.foldLeft(Set[(String, (Int, Int))]()): (acc, antenna) =>
      val sameAntennas = getAllAntennasSameAs(antenna, antennas)
      sameAntennas.foldLeft(acc): (acc2, ant) =>
        val diff = (antenna._2._1 - ant._2._1, antenna._2._2 - ant._2._2)

        def getRangeOfPositions(a: (String, (Int,Int))) =
          Range(a._2._1, if diff._1 < 0 then -1 else limits._1, diff._1).zip(Range(a._2._2, if diff._2 < 0 then -1 else limits._2, diff._2))
            .toList.filter: pos =>
              pos._1 >= 0 && pos._1 < limits._1 && pos._2 >= 0 && pos._2 < limits._2

        (getRangeOfPositions(antenna).map(("#", _)) ++ getRangeOfPositions(ant).map(("#", _))).toSet ++ acc2
    .size

  private def getAllAntennasSameAs(antenna: (String, (Int,Int)), antennas: Vector[(String, (Int, Int))]) =
    val sameAntennas = antennas.filter: a =>
      a._1 == antenna._1 && a._2 != antenna._2
    sameAntennas

  private def buildAntennasPlaces(rawAntennas: List[String]) =
    rawAntennas.zipWithIndex.toVector.flatMap: row =>
      row._1.split("").zipWithIndex.map: place =>
        (place._1, (row._2, place._2))
    .filter(_._1 != ".")