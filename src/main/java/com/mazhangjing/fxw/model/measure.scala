package com.mazhangjing.fxw.model

import com.mazhangjing.fxw.Logger

class Degree(content:String, val point: Point) extends Answer(content)
object Degree {
  val empty = Degree("EMPTY", Point.empty)
  def isEmpty(chooseAns: Degree): Boolean = chooseAns == empty
  def apply(content: String, point: Point): Degree =
    new Degree(content, point)
}

case class Point(num:Int){
  assert(num >= -1 & num <= 10)
  override def toString: String = num.toString
}
object Point {
  val empty: Point = Point(-1)
}

abstract class MeasureQuestion[K](val kind: K, val content:String)
  extends Question[Degree] with Logger {
  override def score(answer: Degree): Int = answer.point.num

  override def toString: String = {
    s"[MeasureQuestion] ${Utils.sub(content)}"
  }
}

object MeasureQuestion {
  val desp1 = Array("完全不符合","比较不符合","不确定","比较符合","完全符合")
  val desp2 = Array("完全不赞同","比较不赞同","不确定","比较赞同","完全赞同")
  val desp3 = Array("非常不赞同","很不赞同","有点不赞同","不确定","有点赞同","很赞同","非常赞同")

  def five[K](content: String, kind: K)
             (implicit desp: Array[String]): MeasureQuestion[K] =
    new MeasureQuestion[K](kind, content) {
      override val answers: Seq[Degree] = desp.indices.map(index => {
        val num = index + 1
        Degree(desp(index), Point(num))
      })
    }

  def ten[K](content: String, kind: K): MeasureQuestion[K] =
    new MeasureQuestion[K](kind, content) {
      override val answers: Seq[Degree] = handlePointCount11
    }

  def seven[K](content: String, kind: K): MeasureQuestion[K] =
    new MeasureQuestion[K](kind, content) {
      override val answers: Seq[Degree] = handlePointCount7
    }

  private def handlePointCount11: Seq[Degree] = {
    0 to 10 map { i => {
      if (i == 0) Degree("0（完全不符合）", Point(0))
      else if (i == 10) Degree("10（完全符合）", Point(10))
      else Degree(Point(i).toString, Point(i))
    }}
  }

  private def handlePointCount7: Seq[Degree] = {
    desp3.indices.map(i => {
      val n = i + 1
      Degree(desp3(i), Point(n))
    })
  }
}

abstract class MeasureQuiz[K] extends Quiz[MeasureQuestion[K], Degree, K] with Logger {

  val intro: String
  val questions: Array[MeasureQuestion[K]]
  lazy val withAnswerQuestions: Array[(MeasureQuestion[K], Degree)] =
    questions.map(mq => (mq, Degree.empty))

  override def fill(question: MeasureQuestion[K], answer: Degree): Unit = {
    logger.debug(s"Fill $question with $answer")
    val index = questions.indexOf(question)
    if (index < 0) throw new Exception("容器不存在此 Question")
    withAnswerQuestions(index) = (question, answer)
  }

  override def answers: Seq[Degree] = withAnswerQuestions.map(_._2)

  override def scores(by: K): Int = {
    if (withAnswerQuestions.exists(_._2 == Degree.empty))
      throw new IllegalStateException("没有完全完成题目")
    else if (by == null) {
      withAnswerQuestions
        .map(aq => aq._1.score(aq._2)).sum
    } else {
      withAnswerQuestions
        .filter(_._1.kind == by)
        .map(aq => aq._1.score(aq._2)).sum
    }
  }
}

class EmotionMeasureQuiz extends MeasureQuiz[Emotion] {

  val intro: String = "下是生活中可能出现的情绪感受，请根据你此时此刻的情绪状态，选择符合你的选项。"

  private implicit val d: Array[String] =
    MeasureQuestion.desp1

  val questions: Array[MeasureQuestion[Emotion]] = Array(
    MeasureQuestion.five("1. 感兴趣的", Positive),
    MeasureQuestion.five("2. 精神活力高的", Positive),
    MeasureQuestion.five("3. 劲头足的", Positive),
    MeasureQuestion.five("4. 热情的", Positive),
    MeasureQuestion.five("5. 自豪的", Positive),
    MeasureQuestion.five("6. 警觉性高的", Positive),
    MeasureQuestion.five("7. 备受鼓舞的", Positive),
    MeasureQuestion.five("8. 意志坚定的", Positive),
    MeasureQuestion.five("9. 注意力集中的", Positive),
    MeasureQuestion.five("10. 有活力的", Positive),
    MeasureQuestion.five("11. 心烦的", Negative),
    MeasureQuestion.five("12. 心神不宁的", Negative),
    MeasureQuestion.five("13. 内疚的", Negative),
    MeasureQuestion.five("14. 恐惧的", Negative),
    MeasureQuestion.five("15. 敌意的", Negative),
    MeasureQuestion.five("16. 易怒的", Negative),
    MeasureQuestion.five("17. 害羞的", Negative),
    MeasureQuestion.five("18. 紧张的", Negative),
    MeasureQuestion.five("19. 坐立不安的", Negative),
    MeasureQuestion.five("20. 害怕的", Negative)
  )

}

class AgentMeasureQuiz extends MeasureQuiz[Effect] {

  val intro = "以下是关于刚在学习视频中屏幕左侧的教学代理的评价，然后根据自己的实际情况，选择相应的选项。"

  private implicit val d: Array[String] = MeasureQuestion.desp2

  val questions: Array[MeasureQuestion[Effect]] = Array(
    MeasureQuestion.five("1.代理促使我更深入地思考学习内容。", Improve),
    MeasureQuestion.five("2.代理使教学变得有趣。", Improve),
    MeasureQuestion.five("3.代理鼓励我反思我正在学习的内容。", Improve),
    MeasureQuestion.five("4.代理使我保持注意力。",Improve),
    MeasureQuestion.five("5.代理有效地呈现了学习材料。", Improve),
    MeasureQuestion.five("6.代理帮助我专注于学习内容。", Improve),
    MeasureQuestion.five("7.代理使我专注于相关信息。", Improve),
    MeasureQuestion.five("8.代理促进了我对学习内容的了解。", Improve),
    MeasureQuestion.five("9.代理是有趣的。", Improve),
    MeasureQuestion.five("10.代理是令人愉悦的。", Improve),
    MeasureQuestion.five("11.代理是知识渊博的。", Trust),
    MeasureQuestion.five("12.代理是智能的。", Trust),
    MeasureQuestion.five("13.代理是有用的。", Trust),
    MeasureQuestion.five("14.代理是有帮助的。", Trust),
    MeasureQuestion.five("15.代理与教师相似。", Trust),
    MeasureQuestion.five("16.代理是有个性的。", Human),
    MeasureQuestion.five("17.代理的情绪很自然。", Human),
    MeasureQuestion.five("18.代理与人类相似。", Human),
    MeasureQuestion.five("19.代理的动作很自然。", Human),
    MeasureQuestion.five("20.代理表达了情绪。", Human),
    MeasureQuestion.five("21.代理富有表现力。", Join),
    MeasureQuestion.five("22.代理是热情的。", Join),
    MeasureQuestion.five("23.代理是令人愉快的。", Join),
    MeasureQuestion.five("24.代理是激励人心的。", Join),
    MeasureQuestion.five("25.代理是友好的。", Join)
  )
}

class ConMeasureQuiz extends MeasureQuiz[Load] {

  val intro = "以下的所有问题指的都是你刚刚进行的学习活动。请选择符合你的选项。其中，0代表完全不符合，10代表完全符合。"

  val questions: Array[MeasureQuestion[Load]] = Array(
    MeasureQuestion.ten("1.学习视频的内容是非常复杂的。", Inside),
    MeasureQuestion.ten("2.学习视频所涉及的问题是非常复杂的。", Inside),
    MeasureQuestion.ten("3.学习视频中所提及的概念是非常复杂的。", Inside),
    MeasureQuestion.ten("4.我投入了非常多的心理努力在学习视频的复杂内容上。", Inside),

    MeasureQuestion.ten("5.学习视频中的解释和说明是非常不清楚的。", Outside),
    MeasureQuestion.ten("6.学习视频中的解释和说明都充满了不清晰的语言。", Outside),
    MeasureQuestion.ten("7.从学习的角度看，学习视频的解释和说明是非常无效的。", Outside),
    MeasureQuestion.ten("8.我投入了很多的心理努力在学习视频的无效解释和说明上。", Outside),

    MeasureQuestion.ten("9.此次学习确实增加了我对学习视频中所含内容的理解。", Relation),
    MeasureQuestion.ten("10.此次学习确实增加了我对学习视频中所涉及问题的理解。", Relation),
    MeasureQuestion.ten("11.此次学习增加了我对学习视频中提及的概念的理解。", Relation),
    MeasureQuestion.ten("12.此次学习确实增强了我对相关内容的知识及理解。", Relation),
    MeasureQuestion.ten("13. 在此次学习中我投入了很多心理努力来増加我的知识和理解。", Relation),
  )
}

class ManualConMeasureQuiz extends ConMeasureQuiz {

  override val questions: Array[MeasureQuestion[Load]] = Array(
    MeasureQuestion.ten("1.此次学习确实增加了我对学习视频中所含内容的理解。", Relation),
    MeasureQuestion.ten("2.学习视频的内容是非常复杂的。", Inside),
    MeasureQuestion.ten("3.学习视频中的解释和说明是非常不清楚的。", Outside),
    MeasureQuestion.ten("4.此次学习确实增加了我对学习视频中所涉及问题的理解。", Relation),
    MeasureQuestion.ten("5.学习视频所涉及的问题是非常复杂的。", Inside),
    MeasureQuestion.ten("6.学习视频中的解释和说明都充满了不清晰的语言。", Outside),
    MeasureQuestion.ten("7.此次学习增加了我对学习视频中提及的概念的理解。", Relation),
    MeasureQuestion.ten("8.学习视频中所提及的概念是非常复杂的。", Inside),
    MeasureQuestion.ten("9.从学习的角度看，学习视频的解释和说明是非常无效的。", Outside),
    MeasureQuestion.ten("10. 在此次学习中我投入了很多心理努力来増加我的知识和理解。", Relation),
    MeasureQuestion.ten("11.我投入了非常多的心理努力在学习视频的复杂内容上。", Inside),
    MeasureQuestion.ten("12.我投入了很多的心理努力在学习视频的无效解释和说明上。", Outside),
    MeasureQuestion.ten("13.此次学习确实增强了我对相关内容的知识及理解。", Relation),
  )

}

class MotiveMeasureQuiz extends MeasureQuiz[Same] {

  val intro = "针对刚才的学习，请仔细阅读每一项，然后根据自己的实际情况，选择相应的选项。"

  val questions: Array[MeasureQuestion[Same]] = Array(
    MeasureQuestion.seven("1.它激起了我的好奇心。", NoDifferent),
    MeasureQuestion.seven("2.它令人感兴趣。", NoDifferent),
    MeasureQuestion.seven("3.它很有趣。", NoDifferent),
    MeasureQuestion.seven("4.我想继续研究它。", NoDifferent),
    MeasureQuestion.seven("5.它使我感到好奇。", NoDifferent),
    MeasureQuestion.seven("6.它令人愉悦。", NoDifferent),
    MeasureQuestion.seven("7.它使我想进一步探索它。", NoDifferent),
    MeasureQuestion.seven("8.我愿意回去后用它参与到进一步的研究中去。", NoDifferent),
  )

  override def scores(by: Same): Int = {
    super.scores(by)
    withAnswerQuestions.map(qa => qa._1.score(qa._2)).sum
  }
}