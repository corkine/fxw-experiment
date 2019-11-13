package com.mazhangjing.fxw

import org.scalatest.{FunSuite, Matchers}

class ModelDesignTest extends FunSuite with Matchers {

  test("Design API") {
    //最终需要的数据
    //Participant
    //QuizScoresSum 知识、前测情绪、后测情绪、代理
    //Quiz, Question, Answer
    case class Answer(content:String)
    trait Question[T <: Answer] {
      def score(answer:T)
    }
    abstract class Quiz[Q <: Question[_], A <: Answer]
      (questions: Seq[(Q,A)]) {
      def fill(question: Q, answer: A): Unit
      def answers: Seq[A]
      def scores: Seq[Int]
    }
    abstract class VideoInOut(val kind: Emotion, val in: String) {
      def out(in: String): String
    }
    class Emotion
    object Positive extends Emotion
    object Negative extends Emotion
    //表情文件
  }

}
