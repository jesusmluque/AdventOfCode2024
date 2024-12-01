import scala.annotation.tailrec

object HistorianHysteria {

  private def buildDoubleList(rawData: List[String]) =
    rawData.map { str =>
      val a :: b :: Nil:List[Long] = str.split(" {3}").map(_.toLong).toList
      (a, b)
    }.foldLeft((List[Long](), List[Long]())) { (acc, next) =>
      (next._1 :: acc._1, next._2 :: acc._2)
    }

  def getTotalDistance(rawData: List[String]):Long =
    @tailrec
    def distance(dl:(List[Long], List[Long]), acc:Long):Long =
      if dl._1.isEmpty then
        acc
      else
        val dist = if dl._1.head > dl._2.head then dl._1.head - dl._2.head else dl._2.head - dl._1.head
        distance((dl._1.tail, dl._2.tail), acc + dist)
    val (l1,l2) = buildDoubleList(rawData)
    distance((l1.sorted, l2.sorted), 0L)

  def getTotalProducts(rawData: List[String]): Long =
    @tailrec
    def product(list:List[Long], list2: List[Long], acc: Long): Long =
      if list.isEmpty then
        acc
      else
        product(list.tail, list2, acc + list2.count(_ == list.head) * list.head)
    val (first, second) = buildDoubleList(rawData)
    product(first, second, 0L)
}
