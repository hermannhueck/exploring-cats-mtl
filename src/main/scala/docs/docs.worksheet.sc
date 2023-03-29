// See:
// - https://typelevel.org/cats-mtl/index.html

// ----- What is MTL? ---------------

import cats._
import cats.data._
import cats.syntax.all._
import cats.mtl._
import cats.mtl.implicits._

// using MonadTransformers EitherT and StateT
def checkState01: EitherT[StateT[List, Int, *], Exception, String] = for {
  currentState: Int <- EitherT.liftF(StateT.get[List, Int])
  result: String    <- if (currentState > 10)
                         EitherT.leftT[StateT[List, Int, *], String](new Exception("Too large"))
                       else
                         EitherT.rightT[StateT[List, Int, *], Exception]("All good")
} yield result

checkState01.value.run(10)
checkState01.value.run(11)

// using MonadError and Stateful
def checkState02[F[_]](implicit S: Stateful[F, Int], E: MonadError[F, Exception]): F[String] = for {
  currentState: Int <- S.get
  result: String    <- if (currentState > 10)
                         E.raiseError(new Exception("Too large"))
                       else
                         E.pure("All good")
} yield result

val materializedProgram: EitherT[StateT[List, Int, *], Exception, String] =
  checkState02[EitherT[StateT[List, Int, *], Exception, *]]

materializedProgram.value.run(10)
materializedProgram.value.run(11)

// ----- Ask -----------------------

// @annotation.nowarn("cat=unused")
// def functionAsk[E]: Ask[E => *, E] =
//   new Ask[E => *, E] {
//     def applicative: Applicative[E => *] = implicitly
//     def ask: E => E                      = identity
//   }

// ----- Local -----------------------

def calculateContentLength[F[_]: Applicative](implicit F: Ask[F, String]): F[Int] =
  F.ask.map(_.length)

def calculateModifiedContentLength[F[_]: Applicative](implicit F: Local[F, String]): F[Int] =
  F.local(calculateContentLength[F])("Prefix " + _)

val result = calculateModifiedContentLength[Reader[String, *]].run("Hello")

def both[F[_]: Monad](implicit F: Local[F, String]): F[(Int, Int)] = for {
  length         <- calculateContentLength[F]
  modifiedLength <- calculateModifiedContentLength[F]
} yield (length, modifiedLength)

val res = both[Reader[String, *]].run("Hello")

// ----- Tell -----------------------

case class ServiceParams(option1: String, option2: Int)

case class ServiceResult(userId: Int, companies: List[String])

@annotation.nowarn("cat=unused")
def serviceCall[F[_]: Monad](params: ServiceParams): F[ServiceResult] =
  // a fake call to some external service, replace with real implementation
  ServiceResult(0, List("Raven Enterprises")).pure[F]

def serviceCallWithLog[F[_]: Monad](params: ServiceParams)(implicit F: Tell[F, Chain[String]]): F[ServiceResult] =
  for {
    _      <- F.tell(Chain.one(show"Call to service with ${params.option1} and ${params.option2}"))
    result <- serviceCall[F](params)
    _      <- F.tell(Chain.one(show"Service returned: userId: ${result.userId}; companies: ${result.companies}"))
  } yield result

val (log, result02): (Chain[String], ServiceResult) =
  serviceCallWithLog[Writer[Chain[String], *]](ServiceParams("business", 42)).run

// ----- Listen -----------------------

def sendToServer[F[_]: Monad](logs: Chain[String]): F[Unit] =
  // impure implementation for demonstrative purposes, please don't do this at home
  Monad[F].pure(println(show"Sending to server: $logs"))

def sendLogsToServer[F[_]: Monad, A](logProgram: F[A])(implicit F: Listen[F, Chain[String]]): F[A] =
  logProgram
    .listen
    .flatMap { case (a, logs) =>
      sendToServer[F](logs).as(a)
    }

def logging[F[_]: Monad](implicit F: Tell[F, Chain[String]]): F[Unit] =
  // Example of some logging activity in your application
  for {
    _ <- F.tell(Chain.one("First log"))
    _ <- F.tell(Chain.one("Second log"))
  } yield ()

val result03 = sendLogsToServer(logging[Writer[Chain[String], *]]).value

// ----- Raise -----------------------

def parseNumber[F[_]: Applicative](in: String)(implicit F: Raise[F, String]): F[Int] = {
  if (in.matches("-?[0-9]+"))
    in.toInt.pure[F]
  else
    F.raise(show"'$in' could not be parsed as a number")
}

val valid = parseNumber[Either[String, *]]("123")

val invalid = parseNumber[Either[String, *]]("123abc")

// ----- Handle -----------------------

def notRecovered[F[_]: Applicative](implicit F: Raise[F, String]): F[Boolean] =
  parseNumber[F]("foo")
    .map(n => if (n > 5) true else false)

def recovered[F[_]: Applicative](implicit F: Handle[F, String]): F[Boolean] =
  parseNumber[F]("foo")
    .handle[String](_ => 0) // Recover from error with fallback value
    .map(n => if (n > 5) true else false)

val err = notRecovered[Either[String, *]]

val result04 = recovered[Either[String, *]]

// ----- Stateful -----------------------

case class ServiceResult02(id: Int, companies: List[String])

def serviceCall[F[_]: Monad](id: String): F[ServiceResult02] = {
  // a fake call to some external service, impure, so don't do this at home!
  println(show"Called service with $id")
  ServiceResult02(0, List("Raven Enterprises")).pure[F]
}

type Cache = Map[String, ServiceResult02]

def cachedServiceCall[F[_]: Monad](id: String)(implicit F: Stateful[F, Cache]): F[ServiceResult02] =
  for {
    cache  <- F.get
    result <- cache.get(id) match {
                case Some(res) => res.pure[F]
                case None      => serviceCall[F](id)
              }
  } yield result

def serviceCallAndWriteToCache[F[_]: Monad](id: String)(implicit F: Stateful[F, Cache]): F[ServiceResult02] =
  for {
    result <- serviceCall[F](id)
    cache  <- F.get
    _      <- F.set(cache.updated(id, result))
  } yield result

def invalidate[F[_]](implicit F: Stateful[F, Cache]): F[Unit] =
  F.set(Map.empty)

@annotation.nowarn("cat=unused")
def program[F[_]: Monad](implicit F: Stateful[F, Cache]): F[ServiceResult02] = for {
  result1     <- cachedServiceCall[F]("ab94d2")
  result2     <- cachedServiceCall[F]("ab94d2") // This should use the cached value
  _           <- invalidate[F]
  freshResult <- cachedServiceCall[F]("ab94d2") // This should access the service again
} yield freshResult

val initialCache: Cache = Map.empty

val (result05, cache) =
  program[State[Cache, *]]
    .run(initialCache)
    .value

// ----- Chronicle -----------------------

type Failures = NonEmptyChain[String]

case class Username(value: String)
case class Password(value: String)

case class User(name: Username, pw: Password)

def validateUsername[F[_]: Monad](u: String)(implicit F: Chronicle[F, Failures]): F[Username] = {
  if (u.isEmpty)
    F.confess(NonEmptyChain.one("Can't be empty"))
  else if (u.contains("."))
    F.dictate(NonEmptyChain.one("Dot in name is deprecated")).map(_ => Username(u))
  else
    Username(u).pure[F]
}

def validatePassword[F[_]: Monad](p: String)(implicit F: Chronicle[F, Failures]): F[Password] = {
  if (p.length < 8)
    F.confess(NonEmptyChain.one("Password too short"))
  else if (p.length < 10)
    F.dictate(NonEmptyChain.one("Password should be longer")).map(_ => Password(p))
  else
    Password(p).pure[F]
}

def validateUser[F[_]: Monad](name: String, password: String)(implicit F: Chronicle[F, Failures]): F[User] =
  (validateUsername[F](name), validatePassword[F](password)).mapN(User)

val luka: Ior[NonEmptyChainImpl.Type[String], User] =
  validateUser[Ior[Failures, *]]("Luka", "secret")

val john: Ior[NonEmptyChainImpl.Type[String], User] =
  validateUser[Ior[Failures, *]]("john.doe", "secret123")

val jane: Ior[NonEmptyChainImpl.Type[String], User] =
  validateUser[Ior[Failures, *]]("jane", "reallysecurepassword")
