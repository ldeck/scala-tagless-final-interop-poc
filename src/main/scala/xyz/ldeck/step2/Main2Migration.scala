package xyz.ldeck.step2

import cats.Functor
import cats.effect.{IO, IOApp}
import xyz.ldeck.step1.{InMemoryUserRepository, UserService}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Random

implicit val ec: ExecutionContext = ExecutionContext.global

trait EffectWrapper[F[_]] {
  def wrap[A](value: => A): F[A]
}

// Define instances of the EffectWrapper typeclass

// For scala.concurrent.Future
implicit def futureEffectWrapper(using ec: ExecutionContext): EffectWrapper[Future] =
  new EffectWrapper[Future] {
    def wrap[A](value: => A): Future[A] = Future(value)
  }

// For cats.effect.IO
implicit val ioEffectWrapper: EffectWrapper[IO] = new EffectWrapper[IO] {
  def wrap[A](value: => A): IO[A] = IO(value)
}

// Define a typeclass for the user repository operations
trait UserRepository[F[_]] {
  def getUser(id: Int): F[Option[String]]
  def saveUser(id: Int, name: String): F[Unit]
}

// Define instances of the UserRepository typeclass using the EffectWrapper

private var users: Map[Int, String] = Map.empty

//// For scala.concurrent.Future
implicit val futureUserRepository: UserRepository[Future] = new UserRepository[Future] {
  def getUser(id: Int): Future[Option[String]] = Future(users.get(id))
  def saveUser(id: Int, name: String): Future[Unit] = Future(users += (id -> name)).map(_ => ())
}

//
//// For cats.effect.IO
implicit val ioUserRepository: UserRepository[IO] = new UserRepository[IO] {
  def getUser(id: Int): IO[Option[String]] = IO(users.get(id))
  def saveUser(id: Int, name: String): IO[Unit] = IO(users += (id -> name)).map(_ => ())
}

// After defining the effect wrapper and typeclass instances,
// you can migrate the UserRepository and UserService to work with the abstract effect wrapper:

class UserRepositoryWrapper[F[_]: EffectWrapper: Functor] extends UserRepository[F] {

  def getUser(id: Int): F[Option[String]] =
    implicitly[EffectWrapper[F]].wrap(users.get(id))

  def saveUser(id: Int, name: String): F[Unit] = {
    // By importing cats.syntax.functor._,
    // we can use the void method to discard the result of users += (id -> name) and return F[Unit] instead.
    import cats.syntax.functor._
    implicitly[EffectWrapper[F]].wrap(users += (id -> name)).void
  }
}

class UserService[F[_]: UserRepository](userRepository: UserRepository[F]) {
  def getUserName(id: Int): F[Option[String]] =
    userRepository.getUser(id)

  def createUser(id: Int, name: String): F[Unit] =
    userRepository.saveUser(id, name)
}

object Main2Migration extends IOApp.Simple {

  import cats.instances.future._
  import cats.syntax.functor._

  // Create the UserRepositoryWrapper for scala.concurrent.Future
  private val futureUserRepositoryWrapper: UserRepository[Future] = new UserRepositoryWrapper[Future]
  private val futureUserService: UserService[Future] = new UserService[Future](futureUserRepositoryWrapper)

  // Create the UserRepositoryWrapper for cats.effect.IO
  private val ioUserRepositoryWrapper: UserRepository[IO] = new UserRepositoryWrapper[IO]()
  private val ioUserService: UserService[IO] = new UserService[IO](ioUserRepositoryWrapper)

  private val random = Random

  def run: IO[Unit] =
    for {
      // scala.concurrent.Future
      _ <- IO.fromFuture(IO(
        futureUserService.createUser(
          id = 1,
          name = random.alphanumeric.take(n = 10).mkString)
        )
      )
      futureName <- IO.fromFuture(IO(futureUserService.getUserName(id = 1)))
      _ <- IO(println(s"Future result: $futureName"))

      // cats.effect.IO
      _ <- ioUserService.createUser(
          id = 2,
          name = random.alphanumeric.take(n = 10).mkString
        )
      ioName <- ioUserService.getUserName(id = 2)
      _ <- IO(println(s"Cats IO result: $ioName"))

    } yield ()
}
