package com.mazhangjing.fxw

import org.bytedeco.ffmpeg.global.avutil
import org.bytedeco.javacv.{CanvasFrame, FFmpegFrameRecorder}
import org.scalatest.FunSuite

class JavaCvTest extends FunSuite {

  test("Code") {
    import org.bytedeco.javacv.OpenCVFrameGrabber
    val grabber = new OpenCVFrameGrabber(0)
    grabber.start()
    var grabbedImage = grabber.grab()
    println(grabbedImage.imageWidth)

    val frame = new CanvasFrame("Cam")
    frame.setCanvasSize(grabbedImage.imageWidth, grabbedImage.imageHeight)

    println(s"Frame Rate ${grabber.getFrameRate}")
    grabber.setFrameRate(30)

    println("grabber.getImageWidth", grabber.getImageWidth)
    val recorder = new FFmpegFrameRecorder("result.mp4", grabber.getImageWidth, grabber.getImageHeight)
    //recorder.setVideoCodec(13)
    recorder.setFormat("mp4")
    recorder.setPixelFormat(avutil.AV_PIX_FMT_YUV420P)
    //recorder.setFrameRate(30)
    recorder.setVideoBitrate(10 * 1024 * 1024)
    recorder.start()

    var imageOutputWell = true
    while (frame.isVisible && imageOutputWell) {
      grabbedImage = grabber.grab()
      if (grabbedImage == null) imageOutputWell = false
      frame.showImage(grabbedImage)
      recorder.record(grabbedImage)
    }
    recorder.stop()
    grabber.stop()
    frame.dispose()
  }

  test("Simple Run") {
    import org.bytedeco.opencv.global.opencv_imgcodecs._
    import org.bytedeco.opencv.global.opencv_imgproc._
    import org.bytedeco.opencv.opencv_core._

    val filename = "fxw/afterAgent.png"
    val image = imread(filename)
    if (image != null) {
      GaussianBlur(image, image, new Size(3, 3), 0)
      imwrite(filename, image)
    }
  }

}
