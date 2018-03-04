package com.farmersedge.aries.core

import jep.Jep

object JepTest {

  def main(args: Array[String]): Unit = {
    val jep = new Jep()
    jep.runScript("test.py")
  }

}
