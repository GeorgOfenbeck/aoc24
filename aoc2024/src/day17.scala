import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import scala.annotation.tailrec

object day17 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day17_sample2.txt"
    } else {
      os.resource / "day17.txt"
    }
  }

  val inputStrings: Vector[String] = os.read.lines(filePath).toVector

  case class Registers(a: Long, b: Long, c: Long)
  case class State(program: Vector[Int], regs: Registers, ip: Int, output: List[Int]) {

    def execute(state: State): State = {
      val operation = state.program(state.ip)
      operation match
        case 0 =>
          adv(state)
        case 1 =>
          bxl(state)
        case 2 =>
          bst(state)
        case 3 =>
          jnz(state)
        case 4 =>
          bxc(state)
        case 5 =>
          out(state)
        case 6 =>
          bdv(state)
        case 7 =>
          cdv(state)
        case _: Int => {
          assert(false)
          state
        }
    }

    def runStepByStep(): State = {
      var state = this
      while (state.ip < state.program.size) {
        printState(state)
        println("Press Enter to continue...")
        scala.io.StdIn.readLine()
        state = execute(state)
      }
      state
    }
    def run(): State = {
      var state = this
      while (state.ip < state.program.size) {
        state = execute(state)
      }
      state
    }
  }

  def readInput(input: Vector[String]): State = {
    val registerPattern = """Register (\w): (\d+)""".r
    val programPattern  = """Program: ([\d,]+)""".r

    val registers = {
      input
        .collect {
          case registerPattern("A", value) =>
            "A" -> value.toInt
          case registerPattern("B", value) =>
            "B" -> value.toInt
          case registerPattern("C", value) =>
            "C" -> value.toInt
        }
        .toMap
    }

    val program = input
      .collectFirst { case programPattern(values) =>
        values.split(",").map(_.toInt).toVector
      }
      .getOrElse(Vector.empty)
    val regs = Registers(registers("A"), registers("B"), registers("C"))
    State(program, regs, 0, List.empty)
  }

  def div(state: State): Long = {
    val numerator      = state.regs.a
    val denominatorpow = getOperant(state = state, combo = true)
    var base           = 2
    if (denominatorpow == 0) {
      base = 1
    } else {
      for (i <- 1 until denominatorpow.toInt) {
        base = base << 1
      }
    }
    val div = numerator / base
    div
  }

  def adv(state: State): State = {
    state.copy(ip = state.ip + 2, regs = state.regs.copy(a = div(state)))
  }

  def bxl(state: State): State = {
    val operand = getOperant(combo = false, state = state)
    val b       = state.regs.b
    val result  = operand ^ b
    state.copy(ip = state.ip + 2, regs = state.regs.copy(b = result))
  }

  def bst(state: State): State = {
    val operand = getOperant(combo = true, state = state)
    val result  = operand % 8
    state.copy(ip = state.ip + 2, regs = state.regs.copy(b = result))
  }

  def jnz(state: State): State = {
    if (state.regs.a == 0) {
      return state.copy(ip = state.ip + 2)
    } else {
      val operand = getOperant(combo = false, state = state)
      if (operand != state.ip) {
        return state.copy(ip = operand.toInt)
      } else {
        return state.copy(ip = state.ip + 2)
      }
    }
  }

  def bxc(state: State): State = {
    // val operand = getOperant(combo = false, state = state)
    val b      = state.regs.b
    val c      = state.regs.c
    val result = c ^ b
    state.copy(ip = state.ip + 2, regs = state.regs.copy(b = result))
  }

  def out(state: State): State = {
    val operand = getOperant(combo = true, state = state)
    val mod     = operand % 8
    state.copy(ip = state.ip + 2, output = mod.toInt +: state.output)
  }

  def bdv(state: State): State = {
    state.copy(ip = state.ip + 2, regs = state.regs.copy(b = div(state)))
  }

  def cdv(state: State): State = {
    state.copy(ip = state.ip + 2, regs = state.regs.copy(c = div(state)))
  }

  def getOperant(combo: Boolean, state: State): Long = {
    if (combo == true) {
      val operand = state.program(state.ip + 1)
      operand match
        case 0 =>
          0
        case 1 =>
          1
        case 2 =>
          2
        case 3 =>
          3
        case 4 =>
          state.regs.a
        case 5 =>
          state.regs.b
        case 6 =>
          state.regs.c
        case _ => {
          assert(false)
          0
        }
    } else {
      state.program(state.ip + 1)
    }
  }

  @main
  def day17_01(): Unit = {
    val initalState = readInput(inputStrings)

    val a  = State(Vector(2, 6), Registers(0, 0, 9), 0, List.empty)
    val ax = a.run()
    assert(ax.regs.b == 1)

    val b  = State(Vector(5, 0, 5, 1, 5, 4), Registers(10, 0, 0), 0, List.empty)
    val bx = b.run()
    assert(bx.output.reverse == List(0, 1, 2))

    val d  = State(Vector(1, 7), Registers(0, 29, 0), 0, List.empty)
    val dx = d.run()
    assert(dx.regs.b == 26)

    val f  = State(Vector(4, 0), Registers(0, 2024, 43690), 0, List.empty)
    val fx = f.run()
    assert(fx.regs.b == 44354)

    val c  = State(Vector(0, 1, 5, 4, 3, 0), Registers(2024, 0, 0), 0, List.empty)
    val cx = c.run()
    assert(cx.regs.a == 0)
    assert(cx.output.reverse == List(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0))

    // println(s" $initalState => ${initalState.run()}")
    println(initalState.copy(regs = initalState.regs.copy(a = 117440)).run().output.reverse.mkString(","))
  }

  def printState(state: State): Unit = {
    println(s"IP: ${state.ip}; Output: ${state.output}")
    println(s"A: ${state.regs.a.toBinaryString.reverse.padTo(32, '0').reverse} - ${state.regs.a}")
    println(s"B: ${state.regs.b.toBinaryString.reverse.padTo(32, '0').reverse} - ${state.regs.b}")
    println(s"C: ${state.regs.c.toBinaryString.reverse.padTo(32, '0').reverse} - ${state.regs.c}")
    for (i <- 0 until state.ip) {
      print("  ")
    }
    println("+")
    println(state.program.mkString(" "))
  }

  def crack(pos: Int, state: State): Unit = {
    for (i <- 0 until 8) {
      var sofar   = state.regs.a
      val attempt = sofar | i
      val start   = State(state.program, Registers(attempt, 0, 0), 0, List.empty)
      val res     = start.run()
      if (res.output.size == res.program.size) {
        if (res.program.reverse == res.output) {
          println(s"Cracked!!! $attempt")
        }
      }
      if (res.output.size > res.program.size) {} else {
        if (res.output.size > pos) {
          if (res.output(pos) == state.program.reverse(pos)) {
            //println(res.output)
            //println(state.program.reverse.toList)
            //println(s"$attempt ${attempt << 3} ${attempt.toBinaryString.length()+3}")
            crack(pos + 1, state.copy(regs = Registers(a = attempt << 3, b = 0, c = 0)))

          }
        }
      }
    }
  }

  @main
  def day17_02(): Unit = {
    val initalState = readInput(inputStrings)
   //  initalState.runStepByStep()

    crack(0, initalState.copy(regs = Registers(0, 0, 0)))
    //println( Integer.MAX_VALUE.toBinaryString.length())
    /*
    for(i <- 0 until 8){
        val start =State(initalState.program,Registers(i,0,0),0,List.empty)
        val res = start.run()
//        printState(res)
      //  println(s"$i -> ${res.output.head}")

    //    val second = start.copy(regs = start.regs.copy(a = i << 3 & i))
//        printState(second.run())

//        println(s"===================$i")
    } */
  }

}
