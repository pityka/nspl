package org.nspl.cli

import utest._
import fastparse.all._

object ParserTests extends TestSuite {
  val tests = Tests {
    "parser" - {

      assert(Parser.whitespace.parse("    \t  ") == Parsed.Success((), 7))

      assert(
        Parser.obj.parse("""boo[\]]""") == Parsed
          .Success(Parser.Object("boo",
                                 Map("_default_" -> Seq(Parser.Value("]")))),
                   7))

      assert(
        Parser.expression.parse(
          "fun1[--arg1 boo --arg2 boo2 --arg3 boo3 --arg2 boo[]] fun2[def[boo] def[boo] --arg4 boo4]") ==
          Parsed.Success(
            Seq(
              Parser.Object(
                "fun1",
                Map(
                  "arg3" -> Seq(Parser.Value("boo3")),
                  "arg2" ->
                    Seq(Parser.Value("boo2"), Parser.Object("boo", Map())),
                  "arg1" -> Seq(Parser.Value("boo"))
                )
              ),
              Parser.Object(
                "fun2",
                Map(
                  "_default_" ->
                    Seq(Parser.Object(
                          "def",
                          Map("_default_" -> Seq(Parser.Value("boo")))),
                        Parser.Object(
                          "def",
                          Map("_default_" -> Seq(Parser.Value("boo"))))),
                  "arg4" -> Seq(Parser.Value("boo4"))
                )
              )
            ),
            89
          ))

      assert(
        Parser.namedArguments.parse(
          "--arg1 boo --arg2 boo2 --arg3 boo3 --arg2 boo[] --arg4") ==
          Parsed.Success(
            Map(
              "arg1" -> Seq(Parser.Value("boo")),
              "arg3" -> Seq(Parser.Value("boo3")),
              "arg4" -> Seq(Parser.Value("")),
              "arg2" ->
                Seq(Parser.Value("boo2"), Parser.Object("boo", Map()))
            ),
            54
          ))

    }
  }
}
