package freemonad

// import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.{Free, Functor, Show}

sealed trait Toy[+A, +Next]
case class Output[A, Next](a: A, next: Next) extends Toy[A, Next]
case class Bell[Next](next: Next) extends Toy[Nothing, Next]
case class Done() extends Toy[Nothing, Nothing]

/*
case class Fix[F[_]](f: F[Fix[F]])

sealed trait FixE[F[_], E]
object FixE {
  case class Fix[F[_], E](f: F[FixE[F, E]]) extends FixE[F, E]
  case class Throwy[F[_], E](e: E) extends FixE[F, E]

  def catchy[F[_]: Functor, E1, E2](ex: => FixE[F, E1])
            (f: E1 => FixE[F, E2]): FixE[F, E2] = ex match {
    case Fix(x) => Fix[F, E2](Functor[F].map(x) { catchy(_)(f)})
    case Throwy(e) => f(e)
  }
}

case class IncompleteException()
 */

object ToyChar {
  type ToyChar[+A] = Toy[Char, A]

  /*
  def fix(toy: ToyChar[Fix[ToyChar]]): Fix[ToyChar] = Fix[ToyChar](toy)

  def fixe[E](toy: ToyChar[FixE[ToyChar, E]]): FixE[ToyChar, E] = FixE.Fix[ToyChar, E](toy)
   */

  implicit object toyCharFunctor extends Functor[ToyChar] {
    def map[A, B](fa: ToyChar[A])(f: A => B): ToyChar[B] = fa match {
      case Output(a, next) => Output(a, f(next))
      case Bell(next) => Bell(f(next))
      case Done() => Done()
    }
  }

  /*
  def output(a: Char): Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Output(a, Free.point(())))
  def bell: Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Bell(Free.point(())))
  def done: Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Done())
   */

  def output(a: Char): Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Output(a, ()))
  def bell: Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Bell(()))
  def done: Free[ToyChar, Unit] = Free.liftF[ToyChar, Unit](Done())

  val subroutine: Free[ToyChar, Unit] = output('A')

  val program: Free[ToyChar, Unit] = for {
    _ <- subroutine
    _ <- bell
    _ <- done
  } yield ()

  def showProgram[R: Show](p: Free[ToyChar, R]): String =
    p.resume.fold({
      case Output(a, next) => "output " + Show[Char].shows(a) + "\n" + showProgram(next)
      case Bell(next) => "bell\n" + showProgram(next)
      case Done() => "done\n"
    }, { r =>
      "return " + Show[R].shows(r) + "\n"
    })

  def pretty[R: Show](p: Free[ToyChar, R]): Unit = print(showProgram(p))
}
