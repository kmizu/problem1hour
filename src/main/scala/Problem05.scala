/**
 * Created by Mizushima on 2015/05/28.
 * See http://www.softantenna.com/wp/software/5-programming-problems/
 */
object Problem05 {
  sealed trait Group
  case class PlusGroup(elements: List[Int]) extends Group
  case class MinusGroup(elements: List[Int]) extends Group
  case class NoneGroup(elements: List[Int]) extends Group

  def sum(groups: List[Group]): Int = groups match {
    case Nil => 0
    case PlusGroup(es)::xs => es.sum + sum(xs)
    case MinusGroup(es)::xs => (-es.sum) + sum(xs)
    case NoneGroup(es)::xs => es.map(_.toString).foldLeft("")(_ + _).toInt + sum(xs)
  }

  def toStrings(groups: List[Group]): List[String] = groups match {
    case Nil => Nil
    case PlusGroup(es)::xs => es.mkString("+") :: toStrings(xs)
    case MinusGroup(es)::xs => ("-" + es.mkString("(", "+", ")")) :: toStrings(xs)
    case NoneGroup(es)::xs => es.map(_.toString).foldLeft("")(_ + _) :: toStrings(xs)
  }


  def combination(list: List[Int]): List[List[Group]] = list match {
    case x::Nil => List(List(PlusGroup(List(x))), List(MinusGroup(List(x))), List(NoneGroup(List(x))))
    case x::xs =>
      def compress(list: List[Group], previous: List[Group]): List[Group] = list match {
        case Nil => previous
        case y::ys =>
          if(previous == Nil) {
            compress(ys, List(y))
          } else {
            (y, previous) match {
              case (PlusGroup(es), PlusGroup(ps)::px) =>
                compress(ys, PlusGroup(ps ++ es)::px)
              case (MinusGroup(es), MinusGroup(ps)::px) =>
                compress(ys, MinusGroup(ps ++ es)::px)
              case (NoneGroup(es), NoneGroup(ps)::px) =>
                compress(ys, NoneGroup(ps ++ es)::px)
              case (g, h) =>
                compress(ys, g::h)
            }
          }
      }
      val groups: List[Group] = List(PlusGroup(List(x)), MinusGroup(List(x)), NoneGroup(List(x)))
      groups.flatMap{g => combination(xs).map{s => g::s}}.map{xs => compress(xs, Nil)}
  }

  def main(args: Array[String]): Unit = {
    println(combination((1 to 9).toList).filter{x => sum(x) == 100}.map{x => toStrings(x)}.map(x => x.mkString("+")))
  }
}
