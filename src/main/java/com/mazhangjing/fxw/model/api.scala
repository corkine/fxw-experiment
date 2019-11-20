package com.mazhangjing.fxw.model

import java.io.{PrintWriter, StringWriter}

import org.slf4j.Logger

case class Participant(id:String, gender: Gender, age:Int, major: String, grade: String)

class Gender
object Male extends Gender {
  override def toString: String = "男"
}
object Female extends Gender {
  override def toString: String = "女"
}

case class Answer(content:String)

object Utils {
  def sub(in:String): String = {
    if (in.length < 10) in
    else in.substring(0, 10) + "..."
  }
  def warn(e: Throwable, op: => Unit = null)(implicit logger:Logger): Unit = {
    val sb = new StringWriter()
    val sw = new PrintWriter(sb)
    op
    e.printStackTrace(sw)
    logger.warn(sb.toString)
    sb.close()
    sw.close()
  }
}

trait Question[T <: Answer] {
  val content: String
  val answers: Seq[T]
  def score(answer: T): Int
}

abstract class Quiz[Q <: Question[_], A <: Answer, U] {
  def fill(question: Q, answer: A): Unit
  def answers: Seq[A]
  def scores(by: U): Int
}

class Emotion
object Positive extends Emotion
object Negative extends Emotion

class Effect
object Improve extends Effect
object Trust extends Effect
object Human extends Effect
object Join extends Effect

class Load
object Inside extends Load
object Outside extends Load
object Relation extends Load

class Same
object NoDifferent extends Same

abstract class VideoInOut(val kind: Emotion, val in: String) {
  def out(in: String): String
}

