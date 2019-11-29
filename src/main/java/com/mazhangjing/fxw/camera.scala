package com.mazhangjing.fxw

import java.io.File
import java.util.concurrent.atomic.AtomicReference

import com.github.sarxos.webcam.Webcam
import com.mazhangjing.fxw.model.Utils
import org.bytedeco.ffmpeg.global.avutil
import org.bytedeco.javacv.FFmpegFrameRecorder
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.concurrent.Task
import scalafx.embed.swing.SwingFXUtils
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox, Label}
import scalafx.scene.image.{ImageView, WritableImage}
import scalafx.scene.layout.{BorderPane, HBox}

trait CameraUtil {
  def recordToFile(file:File, time:Int)
}

object SimpleCameraUtil extends CameraUtil with Logger {

  @volatile @transient private var isRecording: Boolean = false

  @volatile @transient private var stopRecord: Boolean = true

  def isRecordingNow: Boolean = isRecording

  def stopRecordNow(): Unit = {
    stopRecord = true
  }

  override def recordToFile(file: File, time: Int): Unit = {
    try {
      stopRecord = false
      val savedFileName = file.getName
      logger.info(s"Do Record to $savedFileName in $time seconds(Duplicate, until single call) now...")
      import org.bytedeco.javacv.OpenCVFrameGrabber
      val grabber = new OpenCVFrameGrabber(0)
      grabber.start()
      var grabbedImage = grabber.grab()
      grabber.setFrameRate(30)
      val recorder =
        new FFmpegFrameRecorder(s"$savedFileName.mp4",
          grabber.getImageWidth, grabber.getImageHeight)
      //recorder.setVideoCodec(13)
      recorder.setFormat("mp4")
      recorder.setPixelFormat(avutil.AV_PIX_FMT_YUV420P)
      if (FXWExperiment.frameRate != -1) {
        logger.info(s"Use Custom FrameRate ${FXWExperiment.frameRate}")
        recorder.setFrameRate(FXWExperiment.frameRate)
      }
      recorder.setVideoBitrate(10 * 1024 * 1024)
      recorder.start()

      var imageOutputWell = true
      while (!stopRecord && imageOutputWell) {
        isRecording = true
        grabbedImage = grabber.grab()
        if (grabbedImage == null) imageOutputWell = false
        recorder.record(grabbedImage)
      }
      recorder.stop()
      grabber.stop()
      logger.info(s"Done Record to $savedFileName")
    } catch {
      case e: Throwable => Utils.warn(e, {
        logger.warn("试图录制视频出错，请稍后重试...")
      })
    } finally {
      isRecording = false
      stopRecord = true
    }
  }
}

object CameraApp extends JFXApp with Logger {

  val obList = new ObservableBuffer[String]
  var webCam: Webcam = _
  val camIsRunning = BooleanProperty(false)
  val camContent = new ObjectProperty[javafx.scene.image.Image]

  stage = new PrimaryStage {
    title = "Camera Demo"
    scene = new Scene(600, 400) { sc =>
      root = new BorderPane {
        top = new HBox {
          alignment = Pos.Center
          padding = Insets(20,0,0,0)
          spacing = 10
          children = Seq(
            new Label("Choose Camera"),
            new ComboBox[String](obList) {
              delegate.getSelectionModel.selectedItemProperty().addListener((e,o,n) => {
                if (n != null) {
                  logger.info(s"init webcam $n"); initCam(n)
                }
              })
            }
          )
        }
        center = new ImageView {
          fitHeight <== sc.height/2
          fitWidth <== sc.width /1.5
          image <== camContent
        }
        bottom = new HBox {
          spacing = 10
          padding = Insets(0,0,20,0)
          alignment = Pos.Center
          children = Seq(
            new Button("Start") {
              disable <== camIsRunning
              onAction = _ => camIsRunning.set(true)
            },
            new Button("Stop") {
              disable <== !camIsRunning
              onAction = _ => camIsRunning.set(false)
            },
            new Button("Help"),
            new Button("About")
          )
        }
      }
      import scala.collection.JavaConverters._
      onShowing = _ => {
        obList.append(Webcam.getWebcams.asScala.map(_.getName):_*)
        camIsRunning.addListener((e,o,n) => {
          if (o && !n) webCam.close()
          else if (!o && n) {
            webCam.open()
            startStream()
          }
        })
      }
    }
  }

  def initCam(name: String): Unit = {
    val webCamTask = new Task[Void](() => {
      if (webCam != null) {
        camIsRunning.set(false)
      }
      webCam = Webcam.getWebcamByName(name)
      webCam.open()
      camIsRunning.set(true)
      null
    }) {}
    val thread = new Thread(webCamTask)
    thread.setDaemon(true)
    thread.start()
  }

  def startStream(): Unit = {
    val task = new Task[Void](() => {
      val ref = new AtomicReference[WritableImage]()
      while (camIsRunning()) {
        try {
          val img = webCam.getImage
          if (img != null) {
            ref.set(SwingFXUtils.toFXImage(img, ref.get()))
            img.flush()
            Platform.runLater(() => {
              camContent.set(ref.get().delegate)
            })
          }
        } catch {
          case e: Throwable => logger.warn(e.getMessage)
        }
      }
      null
    }) {}
    val thread = new Thread(task)
    thread.setDaemon(true)
    thread.start()
  }
}