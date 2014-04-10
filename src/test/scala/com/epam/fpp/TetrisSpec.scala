import org.specs2._

import com.epam._
import com.epam.fpp._
import FPP._


class TetrisSpec extends  mutable.Specification with matcher.DataTables {

  "Piece module" should {

    val instances = new AnyRef with PieceInstance
    import instances._
    val pieceModule: PieceModule = resolvePieceModule()

    "moveBy function correctly move pieces across the board" in {
      "Initial Position " | "Move Vector" | "Resulting Position"  |
        (10, 10)          !   ( 1, 0)     ! (11, 10)              |
        (10, 10)          !   (-1, 0)     ! (9 , 10)              |
        (10, 10)          !   ( 0, 1)     ! (10, 11)              |
        (10, 10)          !   ( 0,-1)     ! (10,  9)              |> {
        (init, moveVec, expect) => pieceModule.moveBy.tupled(moveVec)(Piece(init, Seq())).pos mustEqual expect
      }
    }
  }

}
