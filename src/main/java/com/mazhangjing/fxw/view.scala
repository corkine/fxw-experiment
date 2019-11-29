package com.mazhangjing.fxw

import java.io.{File, FileReader, FileWriter}
import java.time.{Duration, LocalDate, LocalDateTime}
import java.util
import java.util.Properties
import java.util.concurrent.TimeUnit

import com.mazhangjing.fxw.FXWExperiment._
import com.mazhangjing.fxw.model._
import com.mazhangjing.lab.LabUtils.goNextScreenSafe
import com.mazhangjing.lab.{LabUtils, _}
import com.mazhangjing.scalafx.utils.OutFocus
import javafx.event.Event
import javafx.scene.Scene
import javafx.scene.input.KeyCode
import org.slf4j
import org.slf4j.LoggerFactory
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{HPos, Insets, Pos, VPos}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control.TextFormatter.Change
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.media.{Media, MediaPlayer, MediaView}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, Text, TextFlow}
import scalafx.scene.{Scene => SScene}
import scalafx.stage.Stage
import scalafx.util.StringConverter

import scala.util.{Failure, Success, Try}


object FXWExperiment extends Logger {
  val version: String =
    """
      |0.0.0 2019-11-09 编写模型
      |0.0.1 2019-11-10 JavaFx/ScalaFx 实现模型
      |0.0.2 2019-11-11 实现视频播放和录像，测试了 Windows 的兼容性
      |0.0.3 2019-11-11 修正了视频选择的问题和数据处理问题，修改了指导语
      |0.0.4 2019-11-12 添加了视频播放的多线程控制，当摄像头准备好允许播放。添加了播放暂停功能。修改了记录功能。
      |0.0.5 2019-11-12 修改了一些指导语的细节问题
      |0.0.6 2019-11-13 修改了认知量表的顺序、更改了视频指导语、更改了前测问卷的可选性以及答案的收集判断。
      |0.0.7 2019-11-13 被试信息收集界面字体大小调整，题目内容调整。
      |0.0.8 2019-11-20 修复了被试数据保存问题，修复性别保存的易读性问题，添加学习时长的保存记录，添加了卷首语，提供了 CSS 自定义功能，重构了 UI，大多使用 CSS 定义，方便后续更改，允许表格自定义每列列宽和总列宽，提供跳过视频选项。
      |0.0.9 2019-11-21 添加了 properties 驱动的默认值配置。
      |0.1.0 2019-11-22 修复宽度问题
      |1.0.0 2019-11-22 完整版本
      |1.0.1 2019年11月29日 修复注释问题
      |1.0.2 2019年11月29日 添加了对生成视频帧率的选项支持
      |""".stripMargin

  def VERSION(in:String):String = in.split("\n").map(_.trim).reverse.head

  val default = new Properties()
  logger.info("Loading Properties from fxw/default.properties")
  default.load(new FileReader(new File("fxw/default.properties")))
  logger.info("Setting " + default.toString)

  val inf = 100000000
  val min2: Int = 1000 * 60 * 2

  var imageWidthDefined: Int =
    default.getProperty("imageWidth","800").toInt
  val totalIntro = "totalIntro.png"
  val learnIntro = "learnIntro.png"
  val learnIntro2 = "learnIntro2.png"
  val finishIntro = "finishIntro.png"
  val videoMark: (String, String) = ("normal.mp4", "positive.mp4")

  var knowledgeScreenContentWrapWidth: Int =
    default.getProperty("contentWrapWidth","720").toInt

  var measureQuizContentGapLineVisible = true

  var useCameraRecord = true
  var useNormalEmotion = true
  var playVideoUntilCameraInit = true
  var allowSpaceControlMediaPause = true
  var notUsePreKnowledgeQuestion = false
  var useRandomConMeasureQuiz = true

  var emotionMeasureQuizRowWidths: String =
    default.getProperty("emotionWidths","1400 30 14 14 14 14 14")//1 + 5
  var agentMeasureQuizRowWidths: String =
    default.getProperty("agentWidths","1400 30 14 14 14 14 14")//1 + 5
  var conMeasureQuizRowWidths: String =
    default.getProperty("congWidths","1700 30 7 2 2 2 2 2 2 2 2 2 7")//1 + 11
  var motiveMeasureQuizRowWidths: String =
    default.getProperty("motiveWidths","1400 30 10 10 10 10 10 10 10")//1 + 7
  def ROWS(in:String): Array[Int] = in.trim.split(" ").map(_.trim.toInt).tail
  def DEFINED_WIDTH(in:String): Int = in.trim.split(" ").map(_.trim.toInt).head

  var skipVideo = false
  var frameRate: Int = default.getProperty("frameRate","-1").toInt


  def load(name:String): Try[File] = {
    val file = new File(s"fxw/$name")
    if (file.exists()) Success(file)
    else Failure(new NoSuchElementException("没有此文件"))
  }

  def alert(info:String): Unit = {
    new Alert(AlertType.Information) {
      headerText = "警告"
      contentText = info
    }.showAndWait()
  }

  def go(doIt: => Unit): HBox = {
    new HBox {
      padding = Insets(0,0,20,0)
      alignment = Pos.Center
      children = Seq(
        new Button("确定") {
          minWidth = 100
          minHeight = 30
          onAction = _ => {
            doIt
          }
        }
      )
    }
  }
}

object Data {
  val logger: slf4j.Logger = LoggerFactory.getLogger(getClass)

  var user: Participant = _
  val knowledgeQuiz = new KnowledgeQuiz()
  val emotionMeasureQuizA = new EmotionMeasureQuiz()
  val emotionMeasureQuizB = new EmotionMeasureQuiz()
  val conMeasureQuiz: ConMeasureQuiz = {
    logger.info(s"Init Data.conMeasureQuiz with Random? $useRandomConMeasureQuiz")
    if (useRandomConMeasureQuiz) {
      new ManualConMeasureQuiz()
    } else {
      new ConMeasureQuiz()
    }
  }
  val agentMeasureQuiz = new AgentMeasureQuiz()
  val motiveMeasureQuiz = new MotiveMeasureQuiz()
  var learnUseSeconds: Int = -1

  def writeToCSV(): Unit = {
    val name = user.id + "_" + LocalDate.now().toString + ".csv"
    val sb = new StringBuilder

    def quizSerialNumber(quiz: MeasureQuiz[_], word:String): Array[String] =
      quiz.questions.indices.map(i => {s"$word-${i + 1}"}).toArray
    def kQuizSerialNumber(quiz: KnowledgeQuiz, word:String): Array[String] =
      quiz.questions.indices.map(i => {s"$word-${i + 1}"}).toArray

    val header = {
      {
        if (notUsePreKnowledgeQuestion) Array[String]()
        else kQuizSerialNumber(knowledgeQuiz, "K") ++ Array("K-SUM")
      } ++
        quizSerialNumber(emotionMeasureQuizA, "EA") ++ Array("E1-P-AVG", "E1-N-AVG") ++
        quizSerialNumber(emotionMeasureQuizB, "EB") ++ Array("E2-P-AVG", "E2-N-AVG") ++
        quizSerialNumber(conMeasureQuiz, "C") ++ Array("C-A-AVG", "C-B-VG", "C-C-AVG") ++
        quizSerialNumber(agentMeasureQuiz, "A") ++
        Array("A-A-AVG", "A-B-AVG", "A-C-AVG", "A-D-AVG", "A-AVG") ++
        quizSerialNumber(motiveMeasureQuiz, "M") ++ Array("M-AVG")
    }

    val content = {
      {
        if (notUsePreKnowledgeQuestion) Array[String]()
        else (knowledgeQuiz.answers.map(_.index) ++
          Array(knowledgeQuiz.scores(0))).toArray
      } ++
        emotionMeasureQuizA.answers.map(_.point) ++
        Array(emotionMeasureQuizA.scores(Positive) * 1.0 / 10,
          emotionMeasureQuizA.scores(Negative) * 1.0 / 10) ++
        emotionMeasureQuizB.answers.map(_.point) ++
        Array(emotionMeasureQuizB.scores(Positive) * 1.0 / 10,
          emotionMeasureQuizB.scores(Negative) * 1.0 / 10) ++
        conMeasureQuiz.answers.map(_.point) ++
        Array(conMeasureQuiz.scores(Inside) * 1.0 / 4,
          conMeasureQuiz.scores(Outside) * 1.0 / 4,
          conMeasureQuiz.scores(Relation) * 1.0 / 5) ++
        agentMeasureQuiz.answers.map(_.point) ++
        Array(agentMeasureQuiz.scores(Improve) * 1.0 / 10,
          agentMeasureQuiz.scores(Trust) * 1.0 / 5,
          agentMeasureQuiz.scores(Human) * 1.0 / 5,
          agentMeasureQuiz.scores(Join) * 1.0 / 5,
          agentMeasureQuiz.scores(null) * 1.0 / 25) ++
        motiveMeasureQuiz.answers.map(_.point) ++
        Array(motiveMeasureQuiz.scores(NoDifferent) * 1.0 / 8)
    }

    if (header.length != content.length) {
      logger.warn(s"Head 和 Content 不匹配： ${header.mkString(", ")}, ${content.mkString(", ")}")
    }

    Array("ID", "Gender", "Age", "Major", "Grade", "LearnUseSeconds").foreach(i => sb.append(i + ", "))
    header.foreach(word => sb.append(word).append(", "))
    sb.append("\n")
    Array(user.id, user.gender.toString, user.age.toString,
      user.major, user.grade, learnUseSeconds.toString).foreach(i => sb.append(i + ", "))
    content.foreach(word => sb.append(word).append(", "))

    val writer = new FileWriter(new File(name))
    writer.write(sb.toString())
    writer.close()
  }
}

class FXWExperiment extends Experiment with Logger {

  override protected def initExperiment(): Unit =
    trials.add(new BasicTrial().initTrial())

  override def saveData(): Unit = try {
    Data.writeToCSV()
  } catch {
    case e: Throwable => Utils.warn(e, {
      logger.warn("Save Data Error")
    })
  }

}

class BasicTrial extends Trial {
  override def initTrial(): Trial = {
    //User
    screens.add(new ParticipantDataScreen().initScreen())
    //All Intro
    screens.add(new IntroductionScreen(load(totalIntro)).initScreen())
    if (!notUsePreKnowledgeQuestion) {
      //Knowledge
      Data.knowledgeQuiz.questions.foreach(f => {
        screens.add(new KnowledgeScreen(f).initScreen())
      })
    }
    //PreEmotion
    screens.add(new MeasureScreen[Emotion](Data.emotionMeasureQuizA).initScreen())
    //Video Intro
    screens.add(new IntroductionScreen(load(learnIntro)).initScreen())
    //Video
    if (!skipVideo) screens.add(new VideoPlayAndFeedBackScreen(useNormalEmotion).initScreen())
    //Video Intro 2
    screens.add(new IntroductionScreen(load(learnIntro2)).initScreen())
    //AfterEmotion
    screens.add(new MeasureScreen[Emotion](Data.emotionMeasureQuizB).initScreen())
    //Measure Quiz
    screens.add(new MeasureScreen[Load](Data.conMeasureQuiz).initScreen())
    screens.add(new MeasureScreen[Effect](Data.agentMeasureQuiz).initScreen())
    screens.add(new MeasureScreen[Same](Data.motiveMeasureQuiz).initScreen())
    //Finish Intro
    screens.add(new IntroductionScreen(load(finishIntro)).initScreen())
    this
  }
}

class ParticipantDataScreen extends ScreenAdaptor {

  def checkInputAndGo(in:(String,Gender,String,String,String)): Unit = {
    logger.info("Checking UserInfo")
    if (in._1.isEmpty || in._3.isEmpty || in._4.isEmpty || in._5.isEmpty) {
      new Alert(AlertType.Warning) {
        headerText = "检测到不完全/错误的输入"
        contentText = "请检查后再试。"
      }.showAndWait()
    } else {
      Data.user = Participant(in._1, in._2, in._3.toInt, in._4, in._5)
      logger.info(s"Save UserInfo to ${Data.user}")
      goNextScreenSafe
    }
  }

  val introPane: Label = new Label {
    styleClass.add("welcome_text")
    text = "欢迎参加生物知识学习实验！"
  }

  val gridPane: GridPane = new GridPane {
    styleClass.add("ParticipantGridPane")
    val id_l = new Label("编号")
    val id_i: TextField = new TextField {
      promptText = "输入编号"
    }
    GridPane.setConstraints(id_l, 0,0)
    GridPane.setConstraints(id_i, 1,0)
    val gender_l = new Label("性别")
    val gender_i: ChoiceBox[Gender] = new ChoiceBox[Gender] {
      items = ObservableBuffer(Male, Female)
      delegate.getSelectionModel.selectFirst()
      converter = new StringConverter[Gender] {
        override def fromString(string: String): Gender = Male
        override def toString(t: Gender): String = t match {
          case Male => "男"
          case Female => "女"
        }
      }
    }
    GridPane.setConstraints(gender_l, 0,1)
    GridPane.setConstraints(gender_i, 1,1)
    val age_l = new Label("年龄")
    val age_i: TextField = new TextField {
      promptText = "输入年龄"
      textFormatter = new TextFormatter[String]((t: Change) => {
        if (t.getText.matches("[0-9]*")) t else null
      })
    }
    GridPane.setConstraints(age_l, 0,2)
    GridPane.setConstraints(age_i, 1,2)
    val major_l = new Label("专业")
    val major_i: TextField = new TextField {
      promptText = "输入专业"
    }
    GridPane.setConstraints(major_l, 0,3)
    GridPane.setConstraints(major_i, 1,3)
    val grade_l = new Label("年级")
    val grade_i: ChoiceBox[String] = new ChoiceBox[String] {
      items = ObservableBuffer(
        "大一", "大二", "大三", "大四", "研一", "研二", "研三", "博一", "博二", "博士三或者更高", "其它"
      )
      delegate.getSelectionModel.selectFirst()
    }
    GridPane.setConstraints(grade_l, 0,4)
    GridPane.setConstraints(grade_i, 1,4)
    val ok: Button = new Button("确定") {
      onAction = _ => {
        checkInputAndGo(id_i.text(), gender_i.value(), age_i.text(), major_i.text(), grade_i.value())
      }
    }
    GridPane.setConstraints(ok, 0, 5)
    children = Seq(
      id_l, id_i, gender_l, gender_i, age_l, age_i, major_l, major_i, grade_l, grade_i, ok
    )
  }

  override def initScreen(): Screen = {
    layout = new VBox {
      alignment = Pos.Center
      children = Seq(
        introPane, gridPane
      )
    }.delegate
    duration = inf
    this
  }

  override def eventHandler(event: Event, experiment: Experiment, scene: Scene): Unit = { }
}

class IntroductionScreen(val imageFile: Try[File]) extends ScreenAdaptor {

  override def initScreen(): Screen = {
    layout = new BorderPane {
      center = new ImageView {
        preserveRatio = true
        fitWidth = imageWidthDefined
        imageFile match {
          case Success(value) =>
            image = new Image("file:" + value)
          case Failure(exception) =>
            alert(s"无法打开文件 ${exception.getMessage}")
        }
      }
      bottom = go({
        logger.info("Go Next Called")
        goNextScreenSafe
      })
    }.delegate
    duration = min2
    this
  }

  override def eventHandler(event: Event, experiment: Experiment, scene: Scene): Unit = { }
}

class KnowledgeScreen(val question: KnowledgeQuestion) extends ScreenAdaptor {

  val optionChoosed = new ObjectProperty[Options]

  override def initScreen(): Screen = {
    layout = new BorderPane {

      top = new HBox {
        styleClass.add("knowledgeScreenHeaderIntro")
        children = Seq(new Text("请您根据现有的知识情况选择相符的选项"))
      }

      center = new GridPane {
        styleClass.add("knowledgeScreenHeaderContentGridPane")
        val contentString: Text = new Text(question.content) {
          styleClass.add("knowledgeScreenHeaderContent")
          wrappingWidth = knowledgeScreenContentWrapWidth
        }
        GridPane.setConstraints(contentString, 0,0,6,1)
        children.add(contentString)

        val tg: ToggleGroup = new ToggleGroup {
          selectedToggle.addListener(_ => {
            logger.debug(s"Select Toggle ${delegate.getSelectedToggle}")
            val option = delegate.getSelectedToggle.getUserData.asInstanceOf[Options]
            optionChoosed.set(option)
          })
        }
        question.answers.foreach(answer => {
          logger.debug(s"Answer now is $answer")
          val button = new RadioButton {
            styleClass.add("knowledgeScreenHeaderOption")
            toggleGroup = tg
            text = answer.content
            userData = answer
          }
          children.add(button)
          GridPane.setConstraints(button, {
            answer.index match {
              case A => 0
              case B => 1
              case C => 2
              case D => 3
              case E => 4
              case _ => logger.warn("不存在的问题选项排版"); 5
            }
          }, 1)
        })

        bottom = go({
          logger.info("Go Next Called, Checking User Feedback now...")
          checkOptions()
        })
      }
    }.delegate
    duration = inf
    this
  }

  def checkOptions(): Unit = {
    if (optionChoosed() == null) alert("请选择至少一个选项再继续")
    else {
      Data.knowledgeQuiz.fill(question, optionChoosed())
      goNextScreenSafe
    }
  }

  override def eventHandler(event: Event, experiment: Experiment, scene: Scene): Unit = { }

}

class MeasureScreen[K](val quiz: MeasureQuiz[K],
                       val useDescriptionInEachItem:Boolean = false) extends ScreenAdaptor {

  val answers: Seq[Degree] = quiz.questions.head.answers
  val selectedNowAnswer = new ObjectProperty[(MeasureQuestion[K],Degree)]()
  selectedNowAnswer onChange {
    logger.debug(s"Selected Answer ${selectedNowAnswer()}")
    quiz.fill(selectedNowAnswer()._1, selectedNowAnswer()._2)
  }

  lazy val (percents, defined_width) = quiz match {
    case _: ConMeasureQuiz => (ROWS(conMeasureQuizRowWidths), DEFINED_WIDTH(conMeasureQuizRowWidths))
    case _: AgentMeasureQuiz => (ROWS(agentMeasureQuizRowWidths), DEFINED_WIDTH(agentMeasureQuizRowWidths))
    case _: MotiveMeasureQuiz => (ROWS(motiveMeasureQuizRowWidths), DEFINED_WIDTH(motiveMeasureQuizRowWidths))
    case _: EmotionMeasureQuiz => (ROWS(emotionMeasureQuizRowWidths), DEFINED_WIDTH(emotionMeasureQuizRowWidths))
  }

  override def initScreen(): Screen = {
    layout = new BorderPane { bp =>
      top = new TextFlow(new Text(quiz.intro)) {
        styleClass.add("measureQuizIntro")
      }
      center = new HBox { hb =>
        styleClass.add("measureQuizHeaderContentAnswerHBox")
        children = Seq(
          new GridPane { gpa =>
            styleClass.add("measureQuizHeaderContentAnswerGridPane")
            prefWidth = defined_width
            logger.info(s"Defined Width is $defined_width")
            //放置标题文本
            answers.indices.foreach(i => {
              val a = answers(i)
              val t = new Text(a.content) {
                styleClass.add("measureQuizHeaderText")
              }
              children.add(t)
              GridPane.setConstraints(t, i + 1, 0,
                1,1, HPos.Center, VPos.Center)
            })
            //定义标题行各列宽度
            percents.indices.foreach(index => {
              val rowPercent = percents(index)
              logger.debug(s"Setting For index $index and Value is $rowPercent")
              columnConstraints.add(index, new ColumnConstraints {
                delegate.setPercentWidth(rowPercent)
              }.delegate)
            })
            val measureContent: ScrollPane = new ScrollPane with OutFocus {
              prefWidth = defined_width
              styleClass.add("measureQuizContentAnswerScrollPane")
              content = new GridPane { gpb =>
                prefWidth <== gpa.width
                padding = Insets(0)
                styleClass.add("measureQuizContentAnswerGridPane")
                //定义内部各列宽度
                percents.indices.foreach(index => {
                  val rowPercent = percents(index)
                  gpb.columnConstraints.add(index, new ColumnConstraints() {
                    delegate.setPercentWidth(rowPercent)
                  }.delegate)
                })
                //写入内部各行内容
                var currentLine = 0
                quiz.questions.indices.foreach(questionLine => {
                  val questionNow = quiz.questions(questionLine)
                  val questionText = new TextFlow(new Text(questionNow.content) {
                    styleClass.add("measureQuizContextText")
                  })
                  GridPane.setConstraints(questionText, 0, currentLine)
                  children.add(questionText)
                  val answersForQuestion = questionNow.answers
                  val tg = new ToggleGroup() {
                    selectedToggle.addListener(_ => {
                      val nowParsed =
                        selectedToggle.value.getUserData.asInstanceOf[(MeasureQuestion[K], Degree)]
                      selectedNowAnswer.set(nowParsed)
                    })
                  }
                  answersForQuestion.indices.foreach(answerIndex => {
                    val answerNow = answersForQuestion(answerIndex)
                    val radioButton = new RadioButton(
                      if (useDescriptionInEachItem || answers.length >= 10) answerNow.content else "") {
                      toggleGroup = tg
                      userData = (questionNow, answerNow)
                    }
                    GridPane.setConstraints(
                      radioButton, answerIndex + 1, currentLine,
                      1, 1, HPos.Center, VPos.Center)
                    children.add(radioButton)
                  })
                  if (measureQuizContentGapLineVisible) {
                    val s = new Separator()
                    GridPane.setConstraints(s, 0, currentLine + 1,
                      answers.length + 1, 1)
                    children.add(s)
                    currentLine += 2
                  } else {
                    currentLine += 1
                  }
                })
              }
            }
            GridPane.setConstraints(measureContent, 0,
              1, answers.length + 1, 1)
            children.add(measureContent)
          }
        )
      }
      bottom = go({
        logger.info("Click Go Now...")
        checkAnswerAndGo()
      })
    }.delegate
    duration = inf
    this
  }

  def checkAnswerAndGo(): Unit = {
    if (quiz.withAnswerQuestions.exists(qa => qa._2 == Degree.empty)) {
      alert("没有完全完成本问卷，请滚动鼠标并且重新检查。")
//      logger.debug("Computing Scores")
//      val quiz1 = quiz.asInstanceOf[EmotionMeasureQuiz]
//      val p = quiz1.scores(Positive)
//      val n = quiz1.scores(Negative)
//      logger.debug(s"Pos $p, Neg $n")
    } else {
      goNextScreenSafe
    }
  }

  override def eventHandler(event: Event, experiment: Experiment, scene: Scene): Unit = { }

}

class VideoPlayAndFeedBackScreen(val isNormal:Boolean = true) extends ScreenAdaptor {

  lazy val media: Media = new Media((load(if (isNormal) videoMark._1 else videoMark._2) match {
    case Success(value) => value
    case Failure(_) => throw new RuntimeException("没有找到文件")
  }).toURI.toString)

  val isPlaying = BooleanProperty(false)

  val isPause = BooleanProperty(false)

  val mediaView: MediaView = new MediaView {
    preserveRatio = true
    smooth = true
  }

  var startMS: Long = _

  override def initScreen(): Screen = {
    layout = new StackPane {
      style = "-fx-background-color: black"
      children = Seq(
        mediaView,
        new HBox {
          visible <== !isPlaying
          alignment = Pos.Center
          spacing = 7
          children = Seq(
            new ProgressIndicator {
              maxWidth = 30
            },
            new Label("正在准备...") {
              font = Font.font(25)
              textFill = Color.White
            }
          )
        }
      )
    }.delegate
    duration = inf
    this
  }

  override def callWhenShowScreen(): Unit = {
    mediaView.fitWidth.bind(getScene.widthProperty())
    if (useCameraRecord) {
      new Thread(() => {
        logger.info("Call Camera Action ...")
        val id = if (Data.user != null &&
          Data.user.id != null) Data.user.id else "0000"
        SimpleCameraUtil.recordToFile(new File({
          if (isNormal) id + "_Normal"
          else id + "_Positive"
        }), 2 * 60 + 18)
      }).start()
      if (playVideoUntilCameraInit) {
        new Thread(() => {
          while (!SimpleCameraUtil.isRecordingNow) {
            TimeUnit.MILLISECONDS.sleep(500)
          }
          logger.info("Camera Record Start with Sync, Playing Media Now...")
          Platform.runLater(() => playMedia())
        }).start()
      } else {
        logger.info("Camera Record Start with not Sync, Playing Media Now...")
        playMedia()
      }
    } else {
      logger.info("No Camera Record Request, Playing Media Now...")
      playMedia()
    }
  }

  override def callWhenLeavingScreen(): Unit = {
    logger.info("Stopping Record Call now, Recording Timer End and Update Data...")
    Data.learnUseSeconds = ((System.currentTimeMillis() - startMS) * 1.0 / 1000).toInt
    SimpleCameraUtil.stopRecordNow()
  }

  def playMedia(): Unit = {
    if (mediaView.mediaPlayer() != null) {
      mediaView.mediaPlayer().stop()
    }
    val player = new MediaPlayer(media)
    mediaView.mediaPlayer = player
    player.play()
    isPlaying.set(true)
    player.onEndOfMedia = {
      logger.debug("Media Play Stopped")
      stopMedia()
      goNextScreenSafe
    }
    player.onPaused = {
      isPause.set(true)
    }
    player.onPlaying = {
      isPause.set(false)
    }
    isPause.addListener((_,o,n) => {
      if (o && !n) player.play()
      else if (!o && n) player.pause()
    })
    logger.info("Start Play Time Record...")
    startMS = System.currentTimeMillis()
  }

  def stopMedia(): Unit = {
    val player = mediaView.mediaPlayer()
    if (player != null) {
      player.stop()
    }
    isPlaying.set(false)
  }

  override def eventHandler(event: Event, experiment: Experiment, scene: Scene): Unit = {
    if (allowSpaceControlMediaPause) {
      LabUtils.ifKeyButton(KeyCode.SPACE, event) {
        isPause.set(!isPause())
      }
    }
  }

}

object FXWApplication extends JFXApp with Logger {

  val v = DoubleProperty(0.0)

  val fullScreenChoose = BooleanProperty(true)

  val expertTo: LocalDateTime = LocalDate.parse("2022-11-27").atStartOfDay()

  if (Duration.between(LocalDateTime.now(), expertTo).toDays < 0) {
    alert("错误的软件许可，请联系软件开发和维护人员。")
    System.exit(-1)
  } else {
    logger.info("Checked Passed")
  }

  val gridPane: GridPane = new GridPane {
    padding = Insets(20)
    alignment = Pos.Center
    hgap = 15
    vgap = 15

    def add(lab:String, variable:Int, line: Int)(op: Int => Unit): Unit = {
      val label = new Label(lab)
      val textField = new TextField {
        text = variable.toString
        promptText = lab
        text.addListener(_ => {
          op(text().toInt)
        })
      }
      GridPane.setConstraints(label, 0, line)
      GridPane.setConstraints(textField, 1, line)
      children.addAll(label, textField)
    }

    def add(lab:String, variable:String, line: Int)(op: String => Unit): Unit = {
      val label = new Label(lab)
      val textField = new TextField {
        text = variable
        promptText = lab
        text.addListener(_ => {
          op(text())
        })
      }
      GridPane.setConstraints(label, 0, line)
      GridPane.setConstraints(textField, 1, line)
      children.addAll(label, textField)
    }

    def add(lab:String, variable:Boolean, line: Int)(op: Boolean => Unit): Unit = {
      val label = new Label(lab)
      val choose = new CheckBox {
        selected = variable
        onAction = _ => op(selected())
      }
      GridPane.setConstraints(label, 0, line)
      GridPane.setConstraints(choose, 1, line)
      children.addAll(label, choose)
    }

    val optionStr: Label = new Label("配置中心") {
      font = Font.font(30)
    }
    children.add(optionStr)
    GridPane.setConstraints(optionStr, 0, 0)
    import FXWExperiment._
    add("指导语图片的宽度（像素）", imageWidthDefined, 1) {
      imageWidthDefined = _
    }
    add("内容超过此宽度换行", knowledgeScreenContentWrapWidth, 2) {
      knowledgeScreenContentWrapWidth = _
    }
    add("显示量表行线", measureQuizContentGapLineVisible, 3) {
      measureQuizContentGapLineVisible = _
    }

    add("情绪量表(总宽度、每列比例)", emotionMeasureQuizRowWidths, 4) {
      emotionMeasureQuizRowWidths = _
    }
    add("代理量表(总宽度、每列比例)", agentMeasureQuizRowWidths, 5) {
      agentMeasureQuizRowWidths = _
    }
    add("认知量表(总宽度、每列比例)", conMeasureQuizRowWidths, 6) {
      conMeasureQuizRowWidths = _
    }
    add("动机量表(总宽度、每列比例)", motiveMeasureQuizRowWidths, 7) {
      motiveMeasureQuizRowWidths = _
    }

    add("在被试观看过程中录像", useCameraRecord, 8) {
      useCameraRecord = _
    }
    add("使用正常情绪视频（而非积极）",useNormalEmotion, 9) {
      useNormalEmotion = _
    }
    add("当摄像头初始化完毕后再播放视频", playVideoUntilCameraInit, 10) {
      playVideoUntilCameraInit = _
    }
    add("允许被试使用空格暂停视频", allowSpaceControlMediaPause, 11) {
      allowSpaceControlMediaPause = _
    }

    add("不使用前测知识问卷", notUsePreKnowledgeQuestion, 12) {
      notUsePreKnowledgeQuestion = _
    }
    add("使用随机的认知量表问卷", useRandomConMeasureQuiz, 13) {
      useRandomConMeasureQuiz = _
    }

    add("跳过视频", skipVideo, 14) {
      skipVideo = _
    }

    val fullScreenCheck: CheckBox = new CheckBox("使用全屏") {
      selected <==> fullScreenChoose
    }
    GridPane.setConstraints(fullScreenCheck, 0, 15)
    children.add(fullScreenCheck)

    val runBtn: Button = new Button("开始") {
      onAction = _ => runExperiment(stage)
    }
    GridPane.setConstraints(runBtn, 0, 17)
    children.add(runBtn)
  }

  val scrollPane: ScrollPane = new ScrollPane with OutFocus {
    hbarPolicy = ScrollBarPolicy.Never
  }

  stage = new PrimaryStage {
    title = VERSION(version)
    scene = new SScene(500, 800) {
      root = scrollPane
      onShown = _ => {
        scrollPane.content = gridPane
      }
    }
  }

  def runExperiment(stage: Stage): Unit = {
    val helper: ExperimentHelper = new SimpleExperimentHelperImpl(new ExpRunner {
      override def initExpRunner(): Unit = {
        setEventMakerSet(null)
        val set = new util.HashSet[OpenedEvent]()
        set.add(OpenedEvent.KEY_PRESSED)
        setOpenedEventSet(set)
        setExperimentClassName("com.mazhangjing.fxw.FXWExperiment")
        setTitle("Experiment")
        setVersion("0.0.1")
        setFullScreen(false)
      }
    })
    helper.initStage(stage)
    helper.getScene.getStylesheets.add("file:fxw/custom.css")
    stage.setTitle("FXWExperiment")
    stage.setFullScreen(fullScreenChoose())
    stage.show()
    logger.debug(s"Test Value Change defineWidth $imageWidthDefined")
  }
}
