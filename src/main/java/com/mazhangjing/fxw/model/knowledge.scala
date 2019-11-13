package com.mazhangjing.fxw.model

import com.mazhangjing.fxw.Logger

class Options(content:String, val index: Index) extends Answer(content)

object Options {
  val empty = new Options("EMPTY", X)
  def apply(index: Index, content: String): Options =
    new Options(content, index)
}

class Index
object A extends Index {
  override def toString: String = "A"
}
object B extends Index {
  override def toString: String = "B"
}
object C extends Index {
  override def toString: String = "C"
}
object D extends Index {
  override def toString: String = "D"
}
object E extends Index {
  override def toString: String = "E"
}
object X extends Index {
  override def toString: String = "X"
}

abstract class KnowledgeQuestion(val content: String,
                                 val answers: Seq[Options],
                                 expectAnswer: Index,
                                 givenScore: Int) extends Question[Options] with Logger

object KnowledgeQuestion {

  def onlyOne(content: String, answers: Seq[Options],
              rightAnswer: Index, givenScore: Int): KnowledgeQuestion =
    new KnowledgeQuestion(content, answers, rightAnswer, givenScore) {
      override def score(answer: Options): Int = answer match {
        case _ if answer.index == rightAnswer => givenScore
        case _ => 0
      }
    }

  def assess(content: String, answers: Seq[Options]): KnowledgeQuestion =
    new KnowledgeQuestion(content, answers, X, 0) {
      override def score(answer: Options): Int = answer.index match {
        case A => 0
        case B => 1
        case C => 2
        case D => 3
        case E => 4
        case _ =>
          logger.warn("不可能的答案"); Int.MinValue
      }
    }

  val knowledgeQuestions: Seq[KnowledgeQuestion] = Seq(
    KnowledgeQuestion.onlyOne(
      "1. 当细胞处于静息状态时，其膜内外的电位特点是（ ）。",
      Seq(
        Options(A,"内正外负"),
        Options(B,"内正外正"),
        Options(C,"内负外正"),
        Options(D,"内负外负")), C, 2
    ),
    KnowledgeQuestion.onlyOne(
      "2. 当细胞接收到刺激处于兴奋状态时，其膜内外离子的主要流动特点是（ ）。",
      Seq(
        Options(A, "Na+外流，K+外流"),
        Options(B, "Na+内流，K+外流"),
        Options(C, "Na+外流，K+内流"),
        Options(D, "Na+内流，K+内流")),B, 2
    ),
    KnowledgeQuestion.onlyOne(
      "3. 神经冲动的化学性突触传递过程可简单概括为（ ）传递的过程。",
      Seq(
        Options(A, "电—电—化学"),
        Options(B, "化学—电—化学"),
        Options(C, "化学—化学—电"),
        Options(D, "电—化学—电")
      ), D, 2
    ),
    KnowledgeQuestion.onlyOne(
      "4. 神经纤维在兴奋过程中，膜内电位由+30mV变为-70mV的过程称为（ ）。",
      Seq(
        Options(A, "超极化"),
        Options(B, "复极化"),
        Options(C, "去极化"),
        Options(D, "反极化")
      ), B, 2
    ),
    KnowledgeQuestion.onlyOne(
      "5. 神经冲动传导到神经末梢，在递质释放之前必须进入突触前膜的离子是（ ）。",
      Seq(
        Options(A, "Na+"),
        Options(B, "Cl-"),
        Options(C, "Ca2+"),
        Options(D, "K+")
      ), C, 2
    ),
    KnowledgeQuestion.onlyOne(
      "6. 神经递质在被释放之前，储存在（ ）中。",
      Seq(
        Options(A, "突触囊泡"),
        Options(B, "突触后膜"),
        Options(C, "突触间隙"),
        Options(D, "突触小体")
      ), A, 2
    ),
    KnowledgeQuestion.onlyOne(
      "7. 受体的功能是（ ）。",
      Seq(
        Options(A, "为细胞代谢活动提供能量"),
        Options(B, "完成跨细胞膜的信息传递"),
        Options(C, "为细胞内物质合成提供原料"),
        Options(D, "实现跨细胞膜的物质转运")
      ), B, 2
    ),
    KnowledgeQuestion.onlyOne(
      "8. 在神经系统中，由神经元产生的通过作用于特定受体而发挥调节信息传递效率的化学物质称为（ ）。",
      Seq(
        Options(A, "神经调质"),
        Options(B, "神经激素"),
        Options(C, "神经递质"),
        Options(D, "神经生长因子")
      ), C, 2
    ),
    KnowledgeQuestion.onlyOne(
      "9. 兴奋性突触后电位(EPSP)的产生,是由于突触后膜对（ ）的离子通透性提高。",
      Seq(
        Options(A, "Na+和K+，尤其是K+"),
        Options(B, "Ca2+和K+，尤其是Ca2+"),
        Options(C, "Na+和K+，尤其是Na+"),
        Options(D, "K+和Cl-，尤其是Cl-")
      ), C, 2
    ),
    KnowledgeQuestion.onlyOne(
      "10. 抑制性突触后电位(IPSP)的产生,主要是由于突触后膜对（ ）的离子通透性提高。",
      Seq(
        Options(A, "Na+和K+，尤其是K+"),
        Options(B, "Ca2+"),
        Options(C, "Na+和K+ "),
        Options(D, "Cl-")
      ), D, 2
    )
  )

  val knowledgeQuestionFeedback: Seq[KnowledgeQuestion] = Seq(
    KnowledgeQuestion.assess(
      "11. 你对神经冲动传递过程的相关知识了解多少？（ ）",
      Seq(
        Options(A, "非常少"),
        Options(B, "比较少"),
        Options(C, "中等"),
        Options(D, "比较多"),
        Options(E, "非常多")
      )
    ),
    KnowledgeQuestion.onlyOne(
      "12. 高中时期，你是理科生吗？（ ）\n如果是“3+1+2”模式，问题为：你选择了生物吗？",
      Seq(
        Options(A, "是"),
        Options(B, "否")
      ), A, 2
    ),
    KnowledgeQuestion.onlyOne(
      "13. 你的专业有与人体解剖或神经生理有关的课程吗？ （ ）",
      Seq(
        Options(A, "是"),
        Options(B, "否")
      ), A, 2
    ),
    KnowledgeQuestion.assess(
      "14. 在高中阶段，假设生物满分100分，你估计您的生物分数会处于 （ ）。",
      Seq(
        Options(A, "低于40分"),
        Options(B, "40分—60分"),
        Options(C, "60分—80分"),
        Options(D, "80分—100分")
      )
    )
  )
}

class KnowledgeQuiz extends Quiz[KnowledgeQuestion, Options, Int] with Logger {

  val questions: Seq[KnowledgeQuestion] =
    KnowledgeQuestion.knowledgeQuestions ++ KnowledgeQuestion.knowledgeQuestionFeedback

  val withAnswers: Array[(KnowledgeQuestion, Options)] =
    questions.map((_, Options.empty)).toArray

  override def fill(question: KnowledgeQuestion, answer: Options): Unit = {
    logger.debug(s"Fill $question with $answer")
    val index = questions.indexOf(question)
    if (index < 0) throw new NoSuchElementException("没有此元素")
    else withAnswers(index) = (question, answer)
  }

  override def answers: Seq[Options] = withAnswers.map(_._2)

  override def scores(by: Int): Int = {
    if (withAnswers.exists(qa => qa._2 == Options.empty))
      throw new IllegalStateException("尚未完全回答完毕题目")
    withAnswers.map(qa => {
      qa._1.score(qa._2)
    }).sum
  }
}
