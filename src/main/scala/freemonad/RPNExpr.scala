package freemonad

import scalaz.Scalaz._
import scalaz.{-\/, Free, Functor, \/, \/-}

sealed trait RPNExpr[+N, +E]
case class Number[N, E](n: N, expr: E) extends RPNExpr[N, E]
case class Add[E](expr: E) extends RPNExpr[Nothing, E]
case class Sub[E](expr: E) extends RPNExpr[Nothing, E]
case class Mul[E](expr: E) extends RPNExpr[Nothing, E]
case class End() extends RPNExpr[Nothing, Nothing]

object RPNExpr {
  type RPNEx[+A] = RPNExpr[Double, A]

  implicit object RPNExprFunctor extends Functor[RPNEx] {
    def map[A, B](fa: RPNEx[A])(f: (A) => B): RPNEx[B] = fa match {
      case Number(n, expr) => Number(n, f(expr))
      case Add(expr) => Add(f(expr))
      case Sub(expr) => Sub(f(expr))
      case Mul(expr) => Mul(f(expr))
      case End() => End()
    }
  }

  type RPN[A] = Free[RPNEx, A]

  def num(n: Double): RPN[Unit] = Free.liftF[RPNEx, Unit](Number(n, ()))
  def add: RPN[Unit] = Free.liftF[RPNEx, Unit](Add(()))
  def sub: RPN[Unit] = Free.liftF[RPNEx, Unit](Sub(()))
  def mul: RPN[Unit] = Free.liftF[RPNEx, Unit](Mul(()))
  def end: RPN[Unit] = Free.liftF[RPNEx, Unit](End())

  def stringify[A](expr: RPN[A]): String = expr.resume.fold({
    case Number(n, e) => n + " " + stringify(e)
    case Add(e) => "+ " + stringify(e)
    case Sub(e) => "- " + stringify(e)
    case Mul(e) => "* " + stringify(e)
    case End() => "."
  }, { _ => "" })

  def parse(s: String): String \/ RPN[Unit] =
    s.split("""\s+""").toList.foldRightM[({type L[A] = String \/ A})#L, RPN[Unit]](Free.point[RPNEx, Unit](())) {
      (s, e) => s match {
        case "+" => \/-(Free.liftF[RPNEx, Unit](Add(())) >> e)
        case "-" => \/-(Free.liftF[RPNEx, Unit](Sub(())) >> e)
        case "*" => \/-(Free.liftF[RPNEx, Unit](Mul(())) >> e)
        case "." => \/-(Free.liftF[RPNEx, Unit](End()))
        case n => try { \/-(Free.liftF[RPNEx, Unit](Number(n.toDouble, ())) >> e) }
          catch { case _: NumberFormatException => -\/("invalid input") }
      }
  }

  def eval(expr: RPN[Unit]): String \/ Double = {
    def calc(stack: List[Double], expr: RPN[Unit]): String \/ Double = expr.resume.fold({
      case Number(n, e) => calc(n :: stack, e)
      case Add(e) => stack match {
        case n1 :: n2 :: ns => calc(n2 + n1 :: ns, e)
        case _ => -\/("invalid expression")
      }
      case Sub(e) => stack match {
        case n1 :: n2 :: ns => calc(n2 - n1 :: ns, e)
        case _ => -\/("invalid expression")
      }
      case Mul(e) => stack match {
        case n1 :: n2 :: ns => calc(n2 * n1 :: ns, e)
        case _ => -\/("invalid expression")
      }
      case End() => stack.headOption.toRightDisjunction("invalid expression")
    }, { _ => stack.headOption.toRightDisjunction("invalid expression") })
    calc(List(), expr)
  }

  // example
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

  val value: String \/ Double = for {
    expr <- parse(stringify(expr1 >> expr2))
    v <- eval(expr)
  } yield v
}
