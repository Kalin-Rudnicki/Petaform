package petaform.core

import harness.zio.*
import harness.zio.test.*
import slyce.core.*
import zio.test.*
import zio.test.Assertion.*

object ParserSpec extends DefaultHarnessSpec {

  private def makeParseTest(name: String)(str: String, exp: RawPetaformAST): TestSpec =
    test(name) {
      for {
        _ <- Logger.log.error(Formatting.rawAST(exp))
        res <- RawPetaformAST.fromSource(Source(str, None))
      } yield assert(res)(equalTo(exp))
    }

  override def spec: TestSpec =
    suite("ParserSpec")(
      makeParseTest("case-1")(
        """a:
          |  b: "1"
          |  c: "2"
          |d:
          |  e: "3"
          |  f: "4"""".stripMargin,
        RawPetaformAST.Obj.makeUnsafe(
          "a" -> RawPetaformAST.Obj.makeUnsafe(
            "b" -> is"1",
            "c" -> is"2",
          ),
          "d" -> RawPetaformAST.Obj.makeUnsafe(
            "e" -> is"1",
            "f" -> is"2",
          ),
        ),
      ),
    )

}
