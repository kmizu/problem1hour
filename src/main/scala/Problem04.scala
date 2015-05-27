/**
 * Created by kota_mizushima on 15/05/27.
 */
object Problem04 {
  def sort(xs: List[Int], cmp: (Int, Int) => Int): List[Int] = xs match {
    case Nil => Nil
    case x::Nil => x::Nil
    case _ =>
      val pivot = xs(xs.length / 2)
      sort(xs filter {y => cmp(y, pivot) < 0}, cmp) ++ (xs filter (pivot ==)) ++ sort(xs filter {y => cmp(y, pivot) > 0}, cmp)
  }

  def max(list: List[Int]): Int = sort(list, (x, y) => x % 10 - y % 10).map{_.toString}.mkString.toInt

  def main(args: Array[String]): Unit = {
    println(
      sort(List(50, 2, 1, 9), (x, y) =>  x.toString()(0).toInt - y.toString()(0).toInt).reverseMap(_.toString).mkString
    )
  }
}
