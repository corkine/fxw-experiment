package com.mazhangjing.scalafx.utils

import javafx.scene.control
import scalafx.beans.property.{BooleanProperty, DoubleProperty, IntegerProperty, StringProperty}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{RadioButton, ToggleButton}
import scalafx.scene.image.Image
import scalafx.scene.paint.Paint

import scala.reflect.ClassTag

trait OutFocus {
  this: scalafx.scene.Node =>
  this.style = "-fx-focus-color: transparent;-fx-background-insets: 0; -fx-background-color: transparent, white, transparent, white;"
}

trait CenterV {
  this: scalafx.scene.layout.VBox =>
  this.padding = Insets(15)
  this.spacing = 15
  this.alignment = Pos.Center
}

trait CenterH {
  this: scalafx.scene.layout.HBox =>
  this.padding = Insets(15)
  this.spacing = 15
  this.alignment = Pos.Center
}

/*
class N
sealed class N10 extends N
sealed class N15 extends N
sealed class N20 extends N

class LeftV[T: ClassTag] {
  this: scalafx.scene.layout.VBox =>
  val number: Int =
    if (reflect.classTag[T].toString().contains("N10")) 10
    else if (reflect.classTag[T].toString().contains("N15")) 15
    else if (reflect.classTag[T].toString().contains("N20")) 20
    else 15
  this.padding = Insets(number)
  this.spacing = number
  this.alignment = Pos.CenterLeft
}*/

trait LeftV {
  this: scalafx.scene.layout.VBox =>
  this.padding = Insets(15)
  this.spacing = 15
  this.alignment = Pos.CenterLeft
}

trait LeftH {
  this: scalafx.scene.layout.HBox =>
  this.padding = Insets(15)
  this.spacing = 15
  this.alignment = Pos.CenterLeft
}

trait FastToggle {
  this: scalafx.scene.control.ToggleGroup =>
  def ! = {
    import scala.collection.JavaConverters._
    this.toggles.asScala.map(i => {
      if (i.getClass.equals(classOf[javafx.scene.control.RadioButton])) {
        new RadioButton(delegate = i.asInstanceOf[control.RadioButton])
      } else if (i.getClass.equals(classOf[javafx.scene.control.ToggleButton])) {
        new ToggleButton(delegate = i.asInstanceOf[javafx.scene.control.ToggleButton])
      } else null
      /*val tg = if (reflect.classTag[T].toString().contains("scalafx.scene.control.RadioButton")) {
        new ToggleButton(delegate = jn.asInstanceOf[javafx.scene.control.RadioButton])
      } else if (reflect.classTag[T].toString().contains("scalafx.scene.control.ToggleButton")) {
        new ToggleButton(delegate = jn.asInstanceOf[javafx.scene.control.ToggleButton])
      } else { null }*/
    })
  }
}

class ImagePattern(image:scalafx.scene.image.Image,
                   x:Double, y:Double,
                   width:Double, height:Double,
                   proportional:Boolean) extends {
  override val delegate: javafx.scene.paint.ImagePattern =
    new javafx.scene.paint.ImagePattern(image.delegate, x, y, width, height, proportional)
} with Paint(delegate) {
  def opaque: Boolean = delegate.isOpaque
}
