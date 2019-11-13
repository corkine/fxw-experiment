package com.mazhangjing.scalafx.utils

import org.slf4j.LoggerFactory
import scalafx.scene.control.Button

trait Log4J {
  private val log = LoggerFactory.getLogger(getClass)
  def warn(info:String): Unit = log.warn(info)
  def warn(info:String, error:Throwable): Unit = log.warn(info, error)
  def info(content:String): Unit = log.info(content)
  def info(content:String, argument1: AnyRef): Unit = log.info(content, argument1)
  def debug(info:String): Unit = log.debug(info)
  def debug(info:String, argument1: AnyRef): Unit = log.debug(info, argument1)
}

trait Tools {
  def button(text:String, action: => Unit): Button = new Button(text) {
    onAction = _ => action
  }
}
