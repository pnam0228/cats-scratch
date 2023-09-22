import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.{EitherValues, OptionValues}
import cats._
import cats.data._
import cats.syntax.all._
import cats.implicits.toBifunctorOps
import org.scalatest.matchers.should

import java.util.UUID
import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits._

class ScratchSpec extends AnyFlatSpec with OptionValues with EitherValues with should.Matchers {
  import ScratchSpec._
  // Option syntax
  "Option syntax" should "be cool" in {
    val someInt = 1.some
    someInt shouldBe Some(1)

    val noneInt = none[Int]
    noneInt shouldBe None

//    def findNumGreaterThan(nums: List[Int], target: Int) = nums.foldRight(None) { case (n, r) => if (n > target) Some(n) else r }
    def findNumGreaterThanRemastered(nums: List[Int], target: Int) =
      nums.foldRight(none[Int]) { case (n, r) => if (n > target) Some(n) else r }

    assert(true)
  }

  "Either syntax" should "be nice with it" in {
    val left0 = Left[String, Int]("hello")
    val left1 = "hello".asLeft[Int]
    left0 shouldBe left1
    left1.recover { case "hello" => 0 }.right.value shouldBe 0

    assert(true)
  }

  "Tuple of futures" should "be cool as ice" in {
    case class Voltron(module1: String, module2: String, module3: String, module4: String, module5: String)

    def fetchModule(module: String) = Future {
      println(module)
      module
    }

    def forVoltron = for {
      a <- fetchModule("Command Jet Explorer")
      b <- fetchModule("Strato Weapons Module")
      c <- fetchModule("Advanced Recon Helicopter #1")
      d <- fetchModule("Advanced Recon Helicopter #2")
      e <- fetchModule("Falcon Jet Fighter")
    } yield Voltron(a, b, c, d, e)

    def tupleVoltron = (
      fetchModule("Command Jet Explorer"),
      fetchModule("Strato Weapons Module"),
      fetchModule("Advanced Recon Helicopter #1"),
      fetchModule("Advanced Recon Helicopter #2"),
      fetchModule("Falcon Jet Fighter")
    ).mapN(Voltron)

    forVoltron.foreach { _ =>
      println("-".repeat(50))
      tupleVoltron
    }

    assert(true)
  }

  "OptionT" should "be smooth" in {
    val userDb = Map(1L -> User(1L, "Obiwan"), 2L -> User(2L, "Obitwo"))
    val x = OptionT(Future.successful(Some(1)))

    def findUserById(id: Long): Future[Option[User]] = Future.successful(userDb.get(id))
    def openAccount(user: User): Future[String] =
      Future(UUID.nameUUIDFromBytes(s"${user.id}-${user.name}".getBytes()).toString)
        .andThen { case Success(accountId) =>
          println(s"account $accountId opened")
        }

    def onboardUser(id: Long) =
      for {
        user <- findUserById(id).flatMap(_.fold(Future.failed[User](new NoSuchElementException("user not found")))(Future.successful))
        accountId <- openAccount(user)
      } yield accountId

    def onboardUserRedux(id: Long) =
      for {
        user <- OptionT(findUserById(id)).getOrElseF(Future.failed(new NoSuchElementException("user not found")))
        accountId <- openAccount(user)
      } yield accountId

    (onboardUser(1L), onboardUserRedux(1L)).mapN((accountId1, accountId2) => accountId1 shouldBe accountId2)

    assert(true)
  }

  "EitherT" should "be good" in {
    sealed trait ServiceException extends Product with Serializable {
      def message: String
      def toJavaException: Exception
    }

    object ServiceException {
      case class ResourceNotFound(message: String) extends ServiceException {
        override val toJavaException: Exception = new NoSuchElementException(message)
      }

      case class IrreconcilableState(message: String) extends ServiceException {
        override val toJavaException: Exception = new IllegalStateException(message)
      }

      case class UnauthorizedAccess(message: String) extends ServiceException {
        override val toJavaException: Exception = new IllegalAccessException(message)
      }

      case class InvalidRequest(message: String) extends ServiceException {
        override val toJavaException: Exception = new IllegalArgumentException(message)
      }
    }

    type ExceptionOr[A] = EitherT[Future, ServiceException, A]

    val exceptionOrInt: ExceptionOr[Int] = EitherT(Future.successful(1.asRight[ServiceException]))
    val exceptionOrIntRedux: ExceptionOr[Int] = 1.pure[ExceptionOr]

    // EitherT[F[_], A, B]

    def createUserServiceCall(userId: Long, name: String): Future[User] = (for {
      validatedName <- validateName(name)
      createdUser <- createUser(userId, validatedName).leftWiden[ServiceException]
    } yield createdUser).foldF(e => Future.failed(e.toJavaException), Future.successful)

    def validateName(name: String): ExceptionOr[String] =
      if (name.length < 100) name.pure[ExceptionOr]
      else EitherT.leftT(ServiceException.IrreconcilableState("bad name boo"))

    def createUser(userId: Long, name: String): EitherT[Future, ServiceException.UnauthorizedAccess, User] =
      EitherT.rightT(User(userId, name))

  }
}

object ScratchSpec {
  case class User(id: Long, name: String)
}
