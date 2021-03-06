/**
 * Created by kota_mizushima on 15/05/27.
 */
object Problem03 {
  lazy val fib: Stream[BigInt] = Stream.cons(0, Stream.cons(1, fib.zip(fib.tail).map{ case (x, y) => x + y }))
  def myFib(n: Int): List[BigInt] = fib.take(n).toList

  def main(args: Array[String]): Unit = {
    println(myFib(100))
  }
}
