package com.mazhangjing.fxw

import org.slf4j
import org.slf4j.LoggerFactory

trait Logger {
  implicit val logger: slf4j.Logger = LoggerFactory.getLogger(getClass)
}



