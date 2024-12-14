object RestroomRedoubt:
  case class Robot(position: (Int, Int), velocity: (Int,Int))
  case class Restroom(maxTall: Int, maxWide: Int, robots: List[Robot], time: Int = 0):
    def draw(): Unit =
      val emptyRestroom = Range(0, maxTall).map(row => List.fill(maxWide)("."))
      val fullRestroom = this.robots.foldLeft(emptyRestroom): (room, robot) =>
        val num = if room(robot.position._2)(robot.position._1) != "." then
          (room(robot.position._2)(robot.position._1).toInt + 1).toString
        else
          "1"
        room.updated(robot.position._2, room(robot.position._2).updated(robot.position._1, num))
      .map(_.mkString(""))
      fullRestroom.foreach(r => println(r))
    private def moveRobot(robot: Robot): Robot =
      val newPosition = ((robot.position._1 + robot.velocity._1) % maxWide, (robot.position._2 + robot.velocity._2) %  maxTall) match
        case (t, w) if t < 0 && w < 0 => (t + maxWide, w +  maxTall)
        case (t, w) if t < 0 => (t + maxWide, w)
        case (t, w) if w < 0 => (t, w +  maxTall)
        case p => p
      Robot(newPosition, robot.velocity)
    def tic: Restroom =
      Restroom(this.maxTall, this.maxWide, this.robots.map: robot =>
        moveRobot(robot), time + 1)
    def quadrantCount: Long =
      val q1 = this.robots.count(r => r.position._1 < maxWide / 2 && r.position._2 <  maxTall / 2).toLong
      val q2 = this.robots.count(r => r.position._1 > maxWide / 2 && r.position._2 <  maxTall / 2).toLong
      val q3 = this.robots.count(r => r.position._1 < maxWide / 2  && r.position._2 >  maxTall / 2).toLong
      val q4 = this.robots.count(r => r.position._1 > maxWide / 2 && r.position._2 >  maxTall / 2).toLong
      q1 * q2 * q3 * q4
    def isSymmetric: Boolean = this.robots.groupBy(_.position).forall(_._2.size == 1)

  def getSafetyFactorFor(rawRestroom: List[String], size: (Int,Int)): Long =
    val (sizeTall, sizeWide) = size
    val initRobots = rawRestroom.map: line =>
      val s"p=${right},${down} v=${vRight},${vDown}" = line: @unchecked
      Robot((right.toInt, down.toInt), (vRight.toInt, vDown.toInt))
    val restroom = Restroom(sizeTall, sizeWide, initRobots)
    Range(0,100).foldLeft(restroom)((acc, time) => acc.tic).quadrantCount

  def findChristmasTree(rawRestroom: List[String], size: (Int,Int)): Long =
    def find(initRoom: Restroom) =
      val candidates = Range(0, 10000).foldLeft(List[Restroom](initRoom), initRoom): (acc, time) =>
        val newRoom = acc._2.tic
        (newRoom :: acc._1, newRoom)
      (candidates._1.filter: restroom =>
         restroom.isSymmetric, candidates._2)

    val (sizeTall, sizeWide) = size
    val initRobots = rawRestroom.map: line =>
      val s"p=${right},${down} v=${vRight},${vDown}" = line: @unchecked
      Robot((right.toInt, down.toInt), (vRight.toInt, vDown.toInt))
    val candidates = find(Restroom(sizeTall, sizeWide, initRobots))
    println(s"Candidates 1: size ${candidates._1.size}")
    candidates._1.foreach(_.draw())
    candidates._1.head.time