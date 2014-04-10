package com.epam

import java.awt.{Color, Font}
import swing._


trait RenderModule {
  val render: Graphics2D => State => String => Graphics2D
}


trait RenderInstance {
  import fpp._
  import FPP._

  def resolveRenderModule(width: Int, height: Int)(blockSize: Int)
    (implicit renderModule: (Int, Int) => Int => RenderModule): RenderModule = renderModule(width, height)(blockSize)

    implicit val renderModule: (Int, Int) => Int => RenderModule = {
      case (width, height) =>
        blockSize => new RenderModule {

          val size: Dimension = new Dimension(width * blockSize, height * blockSize)
          val background        : Color = new Color(200, 200, 200)
          val foreground        : Color = new Color(200, 200, 200)
          val borderColor       : Color = new Color(50, 50, 50)
          val statusBackground  : Color = new Color(100, 100, 100)
          val statusForeground  : Color = new Color(200, 200, 200)

          lazy val render: Graphics2D => State => String => Graphics2D =
            graphics => state => message =>
              (clear >>>
                (drawState $ state $ borderColor $ 1) >>>
                (drawState $ state $ foreground $ blockSize / 5) >>>
                (drawState $ state $ borderColor $ blockSize / 3) >>>
                (drawMessage $ message)) $ graphics


          lazy val clear: Graphics2D => Graphics2D =
            graphics => {
              graphics setColor background
              graphics fillRect (0, 0, size.width, size.height)
              graphics setColor statusBackground
              graphics fillRect (0, size.height - blockSize, size.width, blockSize)
              graphics
            }

            lazy val drawState: State => Color => Int => Graphics2D => Graphics2D =
              state => color => border => graphics => {
                graphics setColor color
                for(Block((x, y)) <- state.blocks)
                  graphics fill (new Rectangle(
                      x * blockSize + border,
                      y * blockSize - blockSize + border,
                      blockSize - border * 2,
                      blockSize - border * 2)
                  )
                graphics
              }

          lazy val drawMessage: String => Graphics2D => Graphics2D =
            message => graphics => {
                graphics setColor statusForeground

                graphics setFont (new Font(
                  Font.MONOSPACED,
                  Font.BOLD,
                  blockSize / 2)
                )

                graphics drawString (message, 4, size.height - blockSize / 4)
                graphics
              }

        }
    }
}

