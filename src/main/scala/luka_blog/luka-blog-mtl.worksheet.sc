// See:
// - https://www.signifytechnology.com/blog/2018/10/a-comprehensive-introduction-to-cats-mtl-by-luka-jacobowitz
// - https://typelevel.org/blog/2018/10/06/intro-to-mtl.html

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

// These are just String for simplicity
type Config = String
type Result = String

def getConfig: IO[Config] = ???

def serviceCall(c: Config): IO[Result] = ???

/*
def readerProgram: ReaderT[IO, Config, Result] = for {
  config <- ReaderT.ask[IO, Config]
  result <- ReaderT.liftF(serviceCall(config))
} yield result

def main: IO[Result] = getConfig.flatMap(readerProgram.run)
 */

import cats.mtl._

def readerProgram[F[_]: Monad: LiftIO](implicit A: Ask[F, Config]): F[Result] = for {
  config <- A.ask
  result <- serviceCall(config).to[F]
} yield result

val materializedProgram: Kleisli[IO, String, Result] =
  readerProgram[ReaderT[IO, Config, *]]

def main: IO[Result] = getConfig.flatMap(materializedProgram.run)

type ApplicativeConfig[F[_]] = Ask[F, Config]

def readerProgram2[F[_]: Monad: LiftIO: ApplicativeConfig]: F[Result] = ???

def validConfig(c: Config): Boolean = ???

sealed trait AppError
case object InvalidConfig extends AppError

type MonadAppError[F[_]] = MonadError[F, AppError]

def program[F[_]: MonadAppError: ApplicativeConfig: LiftIO]: F[Result] = for {
  config <- Ask[F, Config]
              .ask
              .ensure(InvalidConfig)(validConfig)
  result <- serviceCall(config).to[F]
} yield result

type EitherApp[A] = EitherT[IO, AppError, A]
type Stack[A]     = ReaderT[EitherApp, Config, A]

val materializedProgram2: Stack[Result] = program[Stack]

def main2: IO[Either[AppError, Result]] =
  EitherT
    .liftF(getConfig)
    .flatMap(materializedProgram2.run)
    .value

// MonadError instances for ReaderT
//
// def monadErrorForReaderT[F[_], E, R](implicit F: MonadError[F, E]): MonadError[ReaderT[F, R, ?], E] =
//   new MonadError[ReaderT[F, R, ?], E] {
//     def raiseError[A](e: E): ReaderT[F, R, A] =
//       ReaderT.liftF(F.raiseError(e))

//     def handleErrorWith[A](fa: ReaderT[F, R, A])(f: E => ReaderT[F, R, A]): ReaderT[F, R, A] =
//       ReaderT.ask[F, R].flatMap { r =>
//         ReaderT.liftF(fa.run(r).handleErrorWith(e => f(e).run(r)))
//       }
//   }

// same program written without mtl:
//
def programWithMT: Stack[Result] = for {
  config <- ReaderT.ask[EitherApp, Config]
  _      <- if (validConfig(config)) ().pure[Stack]
            else ReaderT.liftF[EitherApp, Config, Unit](EitherT.leftT(InvalidConfig))
  result <- ReaderT.liftF(EitherT.liftF[IO, AppError, Result](serviceCall(config)))
} yield result

type Env      = String
type Request  = String
type Response = String

type Result2 = List[Response]

def initialEnv: Env = ???

def updateEnv(r: Response, env: Env): Env = ???

def requests: List[Request] = ???

def newServiceCall(c: Config, req: Request, e: Env): IO[Response] = ???

type StatefulEnv[F[_]] = Stateful[F, Env]

def requestWithState[F[_]: Monad: StatefulEnv: LiftIO](c: Config, req: Request): F[Response] = for {
  env      <- Stateful[F, Env].get
  response <- newServiceCall(c, req, env).to[F]
  _        <- Stateful[F, Env].modify(updateEnv(response, _))
} yield response

def program3[F[_]: MonadAppError: StatefulEnv: ApplicativeConfig: LiftIO]: F[Result2] = for {
  config    <- Ask[F, Config]
                 .ask
                 .ensure(InvalidConfig)(validConfig)
  responses <- requests.traverse(req => requestWithState[F](config, req))
} yield responses

def materializedProgram3: StateT[EitherT[ReaderT[IO, Config, *], AppError, *], Env, Result2] =
  program3[StateT[EitherT[ReaderT[IO, Config, *], AppError, *], Env, *]]

def main3: IO[Either[AppError, (Env, Result2)]] =
  getConfig.flatMap(conf =>
    materializedProgram3
      .run(initialEnv) // Run the StateT layer
      .value           // Run the EitherT layer
      .run(conf) // Run the ReaderT layer
  )
