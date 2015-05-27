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

  def combination(list: List[Int]): List[List[Group]] = list match {
    case Nil => List(List(NoneGroup(Nil)))
    case x::xs =>
      def compress(list: List[Group], previous: List[Group]): List[List[Group]] = list match {
        case Nil => previous::Nil
        case x::xs =>
          Nil
          /*
          xs match {
            case Nil => compress(xs, List(x))
            case a@(PlusGroup(as))::xs =>
          }
          */
      }
      val groups: List[Group] = List(PlusGroup(List(x)), MinusGroup(List(x)), NoneGroup(List(x)))
      groups.map{g => List(g) :: combination(xs)}

      Nil
  }

}
