// See:
// - https://www.signifytechnology.com/blog/2018/10/a-comprehensive-introduction-to-cats-mtl-by-luka-jacobowitz
// - https://typelevel.org/blog/2018/10/06/intro-to-mtl.html

import cats.data._
import cats.effect._

// ----- ReaderT ---------------

// These are just String for simplicity
type Config = String
type Result = String

def getConfig: IO[Config] = ???

def serviceCall(c: Config): IO[Result] = ???

def readerProgram: ReaderT[IO, Config, Result] = for {
  config <- ReaderT.ask[IO, Config]
  result <- ReaderT.liftF(serviceCall(config))
} yield result

def main: IO[Result] = getConfig.flatMap(readerProgram.run)

// ----- StateT ---------------

// Again we use String here for simplicity, in real code this would be something else
type Env      = String
type Request  = String
type Response = String

def initialEnv: Env = ???

def request(r: Request, env: Env): IO[Response] = ???

def updateEnv(r: Response, env: Env): Env = ???

// We also need some fake requests
def req1: Request = ???
def req2: Request = ???
def req3: Request = ???
def req4: Request = ???

def requestWithState(r: Request): StateT[IO, Env, Response] = for {
  env  <- StateT.get[IO, Env]
  resp <- StateT.liftF(request(r, env))
  _    <- StateT.modify[IO, Env](updateEnv(resp, _))
} yield resp

@annotation.nowarn("cat=unused")
def stateProgram: StateT[IO, Env, Response] = for {
  resp1 <- requestWithState(req1)
  resp2 <- requestWithState(req2)
  resp3 <- requestWithState(req3)
  resp4 <- requestWithState(req4)
} yield resp4

def main2: IO[(Env, Response)] = stateProgram.run(initialEnv)
