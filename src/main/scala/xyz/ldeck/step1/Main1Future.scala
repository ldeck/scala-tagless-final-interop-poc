package xyz.ldeck.step1

import cats.effect.{IO, IOApp}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

// 1. Define a simple user repository
trait UserRepository {
  def getUser(id: Int): Future[Option[String]]
  def saveUser(id: Int, name: String): Future[Unit]
}

// 2. Add Future implementation
class InMemoryUserRepository(using ec: ExecutionContext) extends UserRepository {
  private var users: Map[Int, String] = Map.empty

  def getUser(id: Int): Future[Option[String]] = Future(users.get(id))
  def saveUser(id: Int, name: String): Future[Unit] = Future(users += (id -> name)).map(_ => ())
}

// 3. Define a service that uses the UserRepository
class UserService(userRepository: UserRepository)(using ec: ExecutionContext) {
  def getUserName(id: Int): Future[Option[String]] =
    userRepository.getUser(id)

  def createUser(id: Int, name: String): Future[Unit] =
    userRepository.saveUser(id, name)
}


object Main1Future extends IOApp.Simple {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private val userRepository = new InMemoryUserRepository()
  private val userService = new UserService(userRepository)
  private val random = Random

  def run: IO[Unit] =
    for {
      _ <- IO.fromFuture(IO(
        userService.createUser(
          id = 1,
          name = random.alphanumeric.take(n = 10).mkString)
        )
      )

      userName <- IO.fromFuture(IO(userService.getUserName(1)))

      _ <- IO(println(s"Future result: $userName"))

    } yield ()
}
