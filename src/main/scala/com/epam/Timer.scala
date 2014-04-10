package com.epam

import scala.swing.Publisher
import scala.swing.event.Event
import javax.swing.{Timer => SwingTimer}
import java.awt.event.ActionListener


case class TimerEvent (source: AnyRef) extends Event

case class Timer(delay: Int) extends Publisher {
  component =>

  private val _timer = new SwingTimer(delay, new ActionListener {
    def actionPerformed(e: java.awt.event.ActionEvent) {
      publish(new TimerEvent(component))
    }
  })

  def start(): Timer = { _timer.start; this }
  def stop(): Timer = { _timer.stop; this }
}

