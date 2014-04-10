package com.epam



case class Machine(
  state: State,
  score: Int,
  status: Status)

case class State(
  current: Piece,
  gridSize: (Int, Int),
  blocks: Seq[Block])

trait Status
object OnGame extends Status
object GameOver extends Status


trait MachineModule {
  val left:       Machine => Machine
  val right:      Machine => Machine
  val rotate:     Machine => Machine
  val down:       Machine => Machine

  val init:       (Int, Int) => Machine
  val transition: Machine => (Machine => Machine) => Machine
}

trait MachineInstance {
  import fpp._
  import FPP._
  import Kleisli._


  def resolveMachineModule()(
    implicit pieceModule: PieceModule,
    f: PieceModule => MachineModule): MachineModule = f(pieceModule)

  implicit val toMachineModule: PieceModule => MachineModule =
    pieceModule =>
      new MachineModule {

        val left:   Machine => Machine  = buildTrans(pieceModule.moveBy(-1, 0))
        val right:  Machine => Machine  = buildTrans(pieceModule.moveBy(+1, 0))
        val down:   Machine => Machine  = buildTrans(pieceModule.moveBy(0, +1))
        val rotate: Machine => Machine  = buildTrans(pieceModule.rotate)

        //----------------------------------------------------------
        // Init Machine with State
        //----------------------------------------------------------
        lazy val init: (Int, Int) => Machine = {
          case (gWidth, gHeight) =>
            Machine(
              state = State(
                  current = pieceModule.moveBy(gWidth / 2, 0) $ pieceModule.init(),
                  gridSize = (gWidth, gHeight),
                  blocks = Seq()),
              score = 0,
              status = OnGame
            )
        }

        //----------------------------------------------------------
        // Apply State to State transition in scope of Machine
        //----------------------------------------------------------
        lazy val transition: Machine => (Machine => Machine) => Machine = {
          machine =>
            machineFunc =>
              machineFunc $ machine
        }


        //----------------------------------------------------------
        // Delete complited rows
        //----------------------------------------------------------
        lazy val clearState: Machine => Machine = {
          case machine @ Machine(state @ State(_, (gWidth, _), _), score, _) => {

            val deleteRows: Seq[Block] => State => State =
              bls => st =>
                st copy (
                  blocks = st.blocks filterNot { bls contains _ }
                )

            val shiftRows: Seq[Int] => State => State =
              rows => st =>
                (st /: rows) {
                  case (st, key) =>
                      st copy (
                        blocks = st.blocks map {
                          case bl @ Block((x, y)) => if(y <= key) Block(pos = (x, y + 1)) else bl
                        }
                      )
                  }

            val rows = state.blocks
              .groupBy {
                case Block((x, y)) => y
              }.filter {
                case (y, blocks) => blocks.size == gWidth
              }

              (((deleteRows $ rows.values.toSeq.flatten) >>>
                (shiftRows $ rows.keys.toSeq.sorted)) $ state, rows.keys.size) match {
                  case (st, sc) =>
                    machine copy (
                    state = st,
                    score = score + sc)
              }
          }
        }

        //----------------------------------------------------------
        // Cheak field state
        //----------------------------------------------------------
        lazy val checkState: Machine => Machine = {
          case machine @ Machine(state @ State(_, (gW, _), blocks), _, status) =>
            ((stts: Status) =>
              (isOver: Boolean) =>
                stts match {
                  case OnGame if (isOver) => machine copy ( status = GameOver)
                  case _ => machine
                }) $ status $ (
                  pieceModule.blocks(pieceModule.moveBy(gW / 2, 0) $ pieceModule.init()).
                  filter { blocks contains _ }.
                  size != 0)
        }

        //----------------------------------------------------------
        // Generate next Figure
        //----------------------------------------------------------
        lazy val addPiece: Machine => Machine =
          machine => {
            val addNewPiece: Machine => Machine = {
              case mch @ Machine(st @ State(crnt, (gW, _), blks), _, _) =>
                mch.copy(
                  state = st copy (
                    current = pieceModule.moveBy(gW / 2, 0) $ pieceModule.init(),
                    blocks = blks ++ pieceModule.blocks(crnt)
                  )
                )
             }

            (addNewPiece >>> clearState >>> checkState) $ machine
          }

        //----------------------------------------------------------
        // Build State transformation function
        //----------------------------------------------------------
        lazy val buildTrans: (Piece => Piece) => Machine => Machine =
          pieceFunc => {
            case machine @  Machine(state @ State(current, _, _), _, status) => {

              val goDownTerrainValidator: ((Piece, Machine)) => ((Piece, Machine)) = {
                case (ps, mch @ Machine(st, _, _)) =>
                  (validateTerrainPiece $ st $ (pieceModule.moveBy(0, 1) $ ps)) match {
                    case Some(_)  => (current, mch)
                    case None     =>
                      ((m: Machine) => m match {
                        case Machine(State(piece, _, _), _, _) => (piece, m)
                      }) $ (addPiece $ mch)
                  }
              }

              val terrainValidator: ((Piece, Machine)) => ((Piece, Machine)) = {
                case (ps, mch @ Machine(st, _, _)) =>
                  (validateTerrainPiece $ st $ ps) match {
                    case Some(p)  =>  (p, mch)
                    case None     =>  goDownTerrainValidator(current, mch)
                  }
              }

              val borderValidator: ((Piece, Machine)) => ((Piece, Machine)) = {
                case (ps, mch @ Machine(st, _, _)) =>
                  (validateBorderPiece $ state $ ps) match {
                    case Some(p)  => (p, mch)
                    case None     => (current, mch)
                  }
              }

              //-- update machine state if game is not over
              status match {
                case GameOver => machine
                case OnGame =>
                    ((x: (Piece, Machine)) => x match {
                      case (ps, mch @ Machine(st, _, _)) =>
                        mch copy (
                          state = load $ st $ ps
                        )
                    }) $ (
                      (terrainValidator >>> borderValidator) $
                      (pieceFunc $ current,
                        machine copy (
                          state = unload $ state $ current)
                        ))
              }
            }
          }

        //----------------------------------------------------------
        // Validate Piece position in Field (borders touch)
        //----------------------------------------------------------
        lazy val validateBorderPiece: State => Piece => Option[Piece] = {
          case State(_, (gWidth, gHeight), _) =>
            piece =>
              if(pieceModule.blocks(piece) exists {
                case Block((x, y)) => x < 0 || y < 0 || x >= gWidth || y >= gHeight }
              ) None else Some(piece)
          }

        //----------------------------------------------------------
        // Validate Piece position in Field (terrain touch)
        //----------------------------------------------------------
        lazy val validateTerrainPiece: State => Piece => Option[Piece] =
          state =>
            piece => {

            val validateField: State => Piece => Option[Piece] = {
              case State(_, _, blocks) => pc =>
                if(pieceModule.blocks(pc) exists {
                  blocks contains _ }) None else Some(pc)
            }

            val validateBottom: State => Piece => Option[Piece] = {
              case State(_, (_, gHeight), _) => (pc: Piece) =>
                if(pieceModule.blocks(pc) exists {
                  case Block((_, y)) => y >= gHeight }) None else Some(piece)
            }

            (kleisli(validateField $ state) >=> kleisli(validateBottom $ state)) run piece
          }

        //----------------------------------------------------------
        // Add Piece to current Field
        //----------------------------------------------------------
        lazy val load: State => Piece => State =
          state =>
            piece => {
              ((bs: Seq[Block]) =>
                  state.copy(
                    current = piece,
                    blocks = state.blocks ++ bs
                  )
              ) $ pieceModule.blocks(piece)
            }

        //----------------------------------------------------------
        // Remove Piece from current Field
        //----------------------------------------------------------
        lazy val unload: State => Piece => State =
          state =>
            piece => {
              ((poss: Seq[(Int, Int)]) =>
                state.copy(
                  blocks = state.blocks filterNot {
                    case Block(pos) => poss contains pos
                  })
              ) $ pieceModule.blocks(piece).map{ case Block(pos) => pos }
            }
      }
}
