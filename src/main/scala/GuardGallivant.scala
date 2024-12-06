import scala.annotation.tailrec

object GuardGallivant:

  def getTotalVisitedPositionByGuardIn(rawMap: List[String]): Int =
    val mapa = rawMap.toVector.map(_.split("").toVector)
    route(mapa, lookForInitPos(mapa, (mapa(0)(0), (0,0))), Set.empty[(Int,Int)]).size

  def getTotalObstructionsIn(rawMap: List[String]): Int =
    val mapa = rawMap.toVector.map(_.split("").toVector)
    val initPos = lookForInitPos(mapa, (mapa(0)(0), (0, 0)))
    val positions = route(mapa, initPos, Set.empty[(Int,Int)])
    positions.foldLeft(0): (acc, next) =>
      val current = mapa.updated(next._1, mapa(next._1).updated(next._2, "#"))
      if isGuardStackIn(current, initPos, Set.empty[(String, (Int, Int))]) then acc + 1 else acc

  @tailrec
  private def lookForInitPos(mapa: Vector[Vector[String]], current: (String, (Int,Int))): (String, (Int,Int)) =
    if mapa(current._2._1)(current._2._2) != "." && mapa(current._2._1)(current._2._2) != "#" then current
    else
      current match
        case (_, (r, c)) if c < mapa(0).size - 1 => lookForInitPos(mapa, (mapa(r)(c + 1), (r, c + 1)))
        case (_, (r, c)) if c == mapa(0).size - 1 => lookForInitPos(mapa, (mapa(r + 1)(0), (r + 1, 0)))
        case (_, (r, c)) if r < mapa.size - 1 => lookForInitPos(mapa, (mapa(r + 1)(c), (r + 1, c)))
        case _ => current

  @tailrec
  private def route(mapa: Vector[Vector[String]], pos: (String, (Int, Int)), acc: Set[(Int,Int)]): Set[(Int,Int)] =
    if isOutOfMap(mapa, pos) then acc
    else if mapa(pos._2._1)(pos._2._2) == "#" then route(mapa, calculateNextDirection(pos), acc)
    else
      pos match
        case  (v@"^", (x,y)) => route(mapa, (v, (x - 1, y)), acc + ((x,y)))
        case  (v@">", (x,y)) => route(mapa, (v, (x, y + 1)), acc + ((x,y)))
        case  (v@"v", (x,y)) => route(mapa, (v, (x + 1, y)), acc + ((x,y)))
        case  (v@"<", (x,y)) => route(mapa, (v, (x, y - 1)), acc + ((x,y)))

  @tailrec
  private def isGuardStackIn(mapa: Vector[Vector[String]], pos: (String, (Int, Int)), acc: Set[(String, (Int, Int))]): Boolean =
    if isOutOfMap(mapa, pos) then false
    else if mapa(pos._2._1)(pos._2._2) == "#" && acc.contains(pos) then true
    else if mapa(pos._2._1)(pos._2._2) == "#" then isGuardStackIn(mapa, calculateNextDirection(pos), acc + pos)
    else
      pos match
        case  (v@"^", (x,y)) => isGuardStackIn(mapa, (v, (x - 1, y)), acc)
        case  (v@">", (x,y)) => isGuardStackIn(mapa, (v, (x, y + 1)), acc)
        case  (v@"v", (x,y)) => isGuardStackIn(mapa, (v, (x + 1, y)), acc)
        case  (v@"<", (x,y)) => isGuardStackIn(mapa, (v, (x, y - 1)), acc)

  private def isOutOfMap(mapa: Vector[Vector[String]], pos: (String, (Int, Int))) =
    pos._2._1 == mapa.size || pos._2._2 == mapa(0).size || pos._2._1 < 0 || pos._2._2 < 0

  private def calculateNextDirection(pos: (String, (Int, Int))) = pos match
    case ("^", (x, y)) => (">", (x + 1, y))
    case (">", (x, y)) => ("v", (x, y - 1))
    case ("v", (x, y)) => ("<", (x - 1, y))
    case ("<", (x, y)) => ("^", (x, y + 1))