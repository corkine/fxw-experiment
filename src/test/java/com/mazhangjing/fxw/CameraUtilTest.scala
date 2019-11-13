package com.mazhangjing.fxw

import java.io.File

import org.scalatest.FunSuite

class CameraUtilTest extends FunSuite {
  val cu: SimpleCameraUtil.type = SimpleCameraUtil
  test("API") {
    //In Psy4J Component
    new Thread(() => {
      cu.recordToFile(new File("."), 4)
    })
  }

  test("Viewer") {

  }
}
