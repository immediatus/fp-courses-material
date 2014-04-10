package com.epam

import swing._

object Main extends TetrisApp

trait TetrisApp extends SimpleSwingApplication {
  import fpp._
  import FPP._

  val instances = new AnyRef
        with FormInstance
        with RenderInstance
        with MachineInstance
        with PieceInstance

  import instances._

  val blockSize: Int = 25
  val (width, height) = (10, 20)

  implicit val machineModule: MachineModule = resolveMachineModule()
  implicit val rendererModule: RenderModule = resolveRenderModule(width, height)(blockSize)
  implicit val formModule = resolveFormModule(width, height)(blockSize)

  lazy val top: Frame = formModule.gui.mainFrame
}
