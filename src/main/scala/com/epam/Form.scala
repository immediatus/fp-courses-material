package com.epam

import swing._
import event._


case class GUI(
  size: Dimension,
  timer: Timer,
  canvas: Panel,
  mainFrame: Frame)


trait FormModule {
  val gui: GUI
}

trait FormInstance {
  import fpp._
  import FPP._

  def resolveFormModule
    (width: Int, height:Int)
    (blockSize: Int)
    (implicit
      machine: MachineModule,
      renderModule: RenderModule,
      f: (Int, Int) => Int => RenderModule => MachineModule => FormModule): FormModule =
        f(width, height) $ blockSize $ renderModule $ machine

     implicit val toFormModule: (Int, Int) => Int => RenderModule => MachineModule => FormModule = {
      case (width, height) =>
        blockSize =>
          renderModule =>
            machineModule => new FormModule {

        var _machine: Machine = machineModule.init(width, height)

        //----------------------------------------------------------
        // UI initialization functions
        //----------------------------------------------------------
        lazy val gui: GUI = time("MainFrame init time") {
          val timer = timerInit $ 300
          val size = sizeInit $ (width, height)
          val canvas = canvasInit $ size
          val frame = frameInit $ canvas $ size

          timer.start

          canvas.listenTo(timer)
          canvas.listenTo(canvas.keys)

          //-- setup canvas key press & timer events
          canvas.reactions += {
            case TimerEvent(_) =>
              (onTime $ _machine) match {
                case Some(machine) =>
                  _machine = machine
                  canvas.repaint
                case _ =>
              }

            case KeyPressed(_, key, _, _) =>
              (onKeyPress $ _machine $ key) match {
                case Some(machine) =>
                  _machine = machine
                  canvas.repaint
                case _ =>
              }
          }

          //-- setup frame close event
          frame.reactions += {
            case WindowOpened(_) =>
              timer.start
            case WindowClosing(_) =>
              timer.stop
            case WindowActivated(_) =>
              timer.start
            case WindowDeactivated(_) =>
              timer.stop
          }

          GUI(
            timer = timer,
            size = size,
            canvas = canvas,
            mainFrame = frame)
        }

        lazy val sizeInit: ((Int, Int)) => Dimension = {
          case (w, h) =>
            new Dimension(w * blockSize, h * blockSize)
        }


        lazy val timerInit: Int => Timer =
          timeout =>
            new Timer(timeout)


        lazy val canvasInit: Dimension => Panel =
          sz =>
            new Panel {
              preferredSize = sz
              focusable = true

              override def paint(graphics: Graphics2D): Unit = onDraw $ graphics
            }


        lazy val frameInit: Panel => Dimension => Frame =
          cnt =>
            sz =>
              new MainFrame {
                title = "TTRS"
                resizable = false
                size = sz
                minimumSize = sz
                contents = cnt
              }

        //----------------------------------------------------------
        // Panel draw function
        //----------------------------------------------------------
        lazy val onDraw: Graphics2D => Graphics2D =
          graphics => {
            renderModule.render $
              graphics $
              _machine.state $
              ("SCORE: " +
                  _machine.score +
                  " - " +
                  (_machine.status match {
                    case OnGame => "PLAYING"
                    case GameOver => "GAME OVER"
              }))
          }


        //----------------------------------------------------------
        // Key processing functions
        //----------------------------------------------------------
        type KeyProc = Machine => Key.Value => Option[Machine]

        val onKeyPress: KeyProc =
          machine => {
            case key =>
              (for {
                  s <- onRestart(machine)
                  u <- onRotate(machine)
                  l <- onLeft(machine)
                  r <- onRight(machine)
                } yield (s :: u :: l :: r :: Nil) find (s => !s.isEmpty) join
              ) $ key
          }

        val onLeft: KeyProc =
          machine => {
            case Key.Left => Option {
              machineModule.transition $ machine $ machineModule.left
            }
            case _ => None
          }

        val onRight: KeyProc =
          machine => {
            case Key.Right => Option {
              machineModule.transition $ machine $ machineModule.right
            }
            case _ => None
          }

        val onRotate: KeyProc =
          machine => {
            case Key.Up => Option {
              machineModule.transition $ machine $ machineModule.rotate
            }
            case _ => None
          }

        val onRestart: KeyProc =
          machine => {
            case Key.Space => Option {
              machineModule.init(width, height)
            }
            case _ => None
          }


        //----------------------------------------------------------
        // Timer processing function
        //----------------------------------------------------------
        val onTime: Machine => Option[Machine] =
          machine =>
            Option {
              machineModule.transition $ machine $ machineModule.down
            }
      }
  }

  def time[A](info: String)(a: => A): A = {
    ((now: Long) =>
        (a, (System.nanoTime - now) / 1000)
    ) >>> {
      case (result, micros) =>
          println(s"$info: $micros microseconds")
          result
    } $ System.nanoTime
  }
}
