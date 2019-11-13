package com.mazhangjing.fxw.model

import org.scalatest.FunSuite

import scala.util.Random

class MeasureQuizTest extends FunSuite {

  test("EmotionMeasureQuiz Init") {
    val e = new EmotionMeasureQuiz
    println(e.intro)
    assert(e.intro nonEmpty)
    println(e.questions.mkString("\n"))
    assert(e.questions nonEmpty)
    println(e.withAnswerQuestions.mkString("\n"))
    assert(e.answers nonEmpty)
    println(e.answers.mkString("\n"))
    e.withAnswerQuestions.map(_._2)
      .foreach(c => assert(c == Degree.empty))
  }

  test("EmotionMeasureQuiz Set") {
    val e = new EmotionMeasureQuiz
    val choosedIndex = Random.shuffle(e.questions.indices.toSet).head
    val choosedQuestion = e.questions(choosedIndex)
    println(choosedQuestion.answers.mkString("\n"))
    assert(choosedQuestion.answers nonEmpty)

    e.fill(choosedQuestion, choosedQuestion.answers.head)
    println(e.withAnswerQuestions.mkString("\n"))
    println(e.answers.mkString("\n"))
    assert(e.answers.contains(choosedQuestion.answers.head))
    assert(e.withAnswerQuestions.exists(_._2 == choosedQuestion.answers.head))
    assert(!e.withAnswerQuestions.exists(_._2 == choosedQuestion.answers.tail))
  }

  test("EmotionMeasureQuiz Get") {
    val e = new EmotionMeasureQuiz
    val choosedQuestion = e.questions.head
    e.fill(choosedQuestion, choosedQuestion.answers.head)
    assertThrows[IllegalStateException] {
      val i = e.scores(Positive)
    }
  }

  test("EmotionMeasureQuiz Get Well") {
    val e = new EmotionMeasureQuiz
    e.questions.foreach(q => {
      e.fill(q, e.questions.head.answers.head)
    })
    val i = e.scores(Positive)
    println(i)
    assert(i == e.questions.length / 2)
  }

}
