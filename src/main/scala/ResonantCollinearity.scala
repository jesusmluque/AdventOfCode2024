object ResonantCollinearity:

  def countAntinodesWithoutReasoningIn(rawAntennas: List[String]): Int =
    countAntinodesIn(rawAntennas, generateAntinodeFor(false)(rawAntennas.size, rawAntennas.head.length))

  def countAntinodesWithReasoningIn(rawAntennas: List[String]): Int =
    countAntinodesIn(rawAntennas, generateAntinodeFor(true)(rawAntennas.size, rawAntennas.head.length))

  private def countAntinodesIn(rawAntennas: List[String], generateAntinodes: ((String, (Int, Int)), (String, (Int, Int))) => Set[(String, (Int, Int))]) =
    def buildAntennasPlaces(rawAntennas: List[String]) =
      rawAntennas.zipWithIndex.toVector.flatMap: row =>
        row._1.split("").zipWithIndex.map(place => (place._1, (row._2, place._2))).filter(_._1 != ".")
    def getAllAntennasSameAs(antenna: (String, (Int, Int)), antennas: Vector[(String, (Int, Int))]) =
      antennas.filter(a => a._1 == antenna._1 && a._2 != antenna._2)

    val antennas = buildAntennasPlaces(rawAntennas)
    antennas.foldLeft(Set[(String, (Int, Int))]()): (acc, antenna) =>
      getAllAntennasSameAs(antenna, antennas).foldLeft(acc)((acc2, ant) => generateAntinodes(antenna, ant) ++ acc2)
    .size

  private def generateAntinodeFor(withReasoaning: Boolean)(xsize: Int, ysize: Int)(antenna1: (String, (Int, Int)), antenna2: (String, (Int, Int))) =
    val limits = (xsize, ysize)
    val diff = (antenna1._2._1 - antenna2._2._1, antenna1._2._2 - antenna2._2._2)

    def getRangeOfPositions(a: (String, (Int, Int))) =
      if withReasoaning then
        Range(a._2._1, if diff._1 < 0 then -1 else limits._1, diff._1).zip(Range(a._2._2, if diff._2 < 0 then -1 else limits._2, diff._2))
      else
        List((antenna1._2._1 + diff._1, antenna1._2._2 + diff._2), (antenna2._2._1 - diff._1, antenna2._2._2 - diff._2))
      .filter(pos => pos._1 >= 0 && pos._1 < limits._1 && pos._2 >= 0 && pos._2 < limits._2)

    (getRangeOfPositions(antenna1).map(("#", _)) ++ getRangeOfPositions(antenna2).map(("#", _))).toSet