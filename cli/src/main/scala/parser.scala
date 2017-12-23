package org.nspl.cli

import fastparse.all._

object Parser {
  sealed trait Node {
    def asString: String
    def asStringOpt: Option[String]
    def getFirstInOpt(field: String): Option[String]
    def getFirstIn(field: String): String
    def getFirstInOrDefault(field: String): String

  }
  case class Value(s: String) extends Node {
    def asString = s
    def asStringOpt = Some(s)
    def getFirstInOpt(field: String): Option[String] = None
    def getFirstIn(field: String) =
      throw new RuntimeException(s"expected object with $field, got value $s")
    def getFirstInOrDefault(field: String): String =
      throw new RuntimeException(s"expected object with $field, got value $s")
  }
  case class Object(name: String, arguments: Map[String, Seq[Node]])
      extends Node {
    def asStringOpt: Option[String] = None
    def asString = throw new RuntimeException("call asString on Object")
    def getFirstInOpt(field: String): Option[String] =
      arguments.get(field).flatMap(_.headOption).flatMap(_.asStringOpt)
    def getFirstIn(field: String): String = {
      arguments.get(field) match {
        case Some(Value(s) +: xs) => s
        case _ =>
          throw new RuntimeException(
            s"expected object with $field, got $arguments")
      }
    }
    def getFirstInOrDefault(field: String): String = {
      arguments.get(field).orElse(arguments.get("_default_")) match {
        case Some(Value(s) +: xs) => s
        case _ =>
          throw new RuntimeException(
            s"expected object with $field or default, got $arguments")
      }
    }

  }

  def castToNode(v: AnyRef) = v match {
    case obj: Object => obj
    case str: String =>
      Value(str)
  }

  val whitespace: P[Unit] = P(CharIn(" \t\r\n").rep)
  val name: P[String] = P(CharPred(_.isLetterOrDigit).rep(min = 1).!)
  val escape = P("\\" ~ "]".!)
  val escapedChar: P[String] = P(
    (("\\" ~ "]".!) |
      ("\\" ~ "[".!) |
      CharPred(c => c != ']' && c != '[').!))
  val escapedString: P[String] = P(
    (escapedChar ~ !"--").rep.map(_.mkString("")))

  val namedArguments: P[Map[String, Seq[Node]]] =
    P(
      (whitespace ~ "--" ~ name ~ whitespace ~ (obj | escapedString))
        .rep(min = 1))
      .map(_.map {
        case (name, v) => (name, castToNode(v))
      }.groupBy(_._1).map {
        case (name, seq) => (name, seq.map(_._2))
      })

  val defaultArgument: P[Map[String, Seq[Node]]] =
    P(
      (whitespace ~ !"--" ~ whitespace ~ (obj | escapedString.filter(
        _.size > 0))).rep).map(vs =>
      Map("_default_" -> vs.map(v => castToNode(v))).filterNot(_._2.isEmpty))

  val obj: P[Object] =
    P(
      name ~ "[" ~ defaultArgument.?.map(_.getOrElse(Map())) ~ whitespace ~ namedArguments.?.map(
        _.getOrElse(Map())) ~ "]")
      .map {
        case (n: String,
              default: Map[String, Seq[Node]],
              named: Map[String, Seq[Node]]) =>
          Object(n, default ++ named)
      }

  val expression: P[Seq[Object]] = P((whitespace ~ obj ~ whitespace).rep ~ End)

  def apply(s: String) = expression.parse(s)
}
