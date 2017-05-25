import freemonad.RPNExpr.{RPN, add, end, eval, mul, num, parse, stringify, sub}

import scalaz.Scalaz._
import scalaz.\/

object Main {
  def main(args: Array[String]): Unit = {
    Seq(
      s"expr1: ${expr1}",
      s"expr2: ${expr2}",
      s"stringify(expr1): ${expr1Str}",
      s"stringify(expr2): ${expr2Str}",
      s"parse(stringify(expr1)): ${parse(expr1Str)}",
      s"parse(stringify(expr2)): ${parse(expr2Str)}",
      s"eval(expr1): ${eval(expr1)}",
      s"eval(expr2): ${eval(expr2)}",
      s"eval(expr1 >> expr2): ${value}"
    ).foreach(println)
  }

  val expr1: RPN[Unit] = for {
    _ <- num(8)
    _ <- num(6)
    _ <- num(1)
    _ <- sub
    _ <- mul
  } yield ()

  val expr2: RPN[Unit] = for {
    _ <- num(2)
    _ <- add
    _ <- end
  } yield  ()

  val expr1Str: String = stringify(expr1)

  val expr2Str: String = stringify(expr2)

  val value: String \/ Double = for {
    expr <- parse(stringify(expr1 >> expr2))
    v <- eval(expr)
  } yield v
}
