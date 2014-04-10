package com.epam


case class Piece(
  pos: (Int, Int),
  locals: Seq[(Int, Int)])

case class Block(
  pos: (Int, Int))


trait PieceModule {
  val init:       Unit => Piece

  val moveBy:     (Int, Int) => Piece => Piece
  val rotate:     Piece => Piece

  val blocks:     Piece => Seq[Block]
}

trait PieceInstance {
  import fpp._
  import FPP._


  def resolvePieceModule(x: Unit)(implicit module: PieceModule): PieceModule = module
  implicit val toPieceModule: PieceModule =
    new PieceModule {

      //----------------------------------------------------------
      // Create new Piece
      //----------------------------------------------------------
      val init: Unit => Piece = {
        case () => Piece(
          pos = (0, 1),
          locals = Seq((-1, 0), (0, 0), (1, 0), (0, 1)))
        }

      //----------------------------------------------------------
      // Move Piece to (dx, dy)
      //----------------------------------------------------------
      val moveBy: (Int, Int) => Piece => Piece = {
        case(dx, dy) => piece => piece.pos match {
          case (x, y) => piece.copy(pos = (x + dx, y + dy))
        }
      }

      //----------------------------------------------------------
      // Rotate Piece to 90 degree
      //----------------------------------------------------------
      val rotate:  Piece => Piece =
          piece => {
            ((cos: Int, sin: Int) =>
              piece.copy(
                  locals =
                    for((x, y) <- piece.locals)
                      yield (x * cos - y * sin, x * sin + y * cos)
              )
            ) $ 0 $ -1
          }

      //----------------------------------------------------------
      // Get all blocks of Piece
      //----------------------------------------------------------
      val blocks: Piece => Seq[Block] = {
        case Piece((x, y), locals) =>
          for((dx, dy) <- locals) yield Block(pos = (x + dx, y + dy))
        }
    }
}
