import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import mu.PriorityQueue
import scala.annotation.tailrec

object day21 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day21_sample.txt"
    } else {
      os.resource / "day21.txt"
    }
  }

  val inputStrings: Vector[String]        = os.read.lines(filePath).toVector
  val inputCharVecs: Vector[Vector[Char]] = inputStrings.map(_.toCharArray.toVector)
  val inputNums                           = inputStrings.map(_.dropRight(1).toInt)

  case class Pos(y: Int, x: Int)

  trait Pad {
    def pos: Pos
    override def toString(): String
  }

  sealed trait NumPad extends Pad {
    override def toString: String = s"${this.getClass.getSimpleName}(${pos.x}, ${pos.y})"
  }

  object Nr7 extends NumPad {
    val pos = Pos(0, 0)
  }

  object Nr8 extends NumPad {
    val pos = Pos(0, 1)
  }

  object Nr9 extends NumPad {
    val pos = Pos(0, 2)
  }

  object Nr4 extends NumPad {
    val pos = Pos(1, 0)
  }

  object Nr5 extends NumPad {
    val pos = Pos(1, 1)
  }

  object Nr6 extends NumPad {
    val pos = Pos(1, 2)
  }

  object Nr1 extends NumPad {
    val pos = Pos(2, 0)
  }

  object Nr2 extends NumPad {
    val pos = Pos(2, 1)
  }

  object Nr3 extends NumPad {
    val pos = Pos(2, 2)
  }

  object Nr0 extends NumPad {
    val pos = Pos(3, 1)
  }

  object NrA extends NumPad {
    val pos = Pos(3, 2)
  }

  sealed trait DPad extends Pad

  object DUp extends DPad {
    val pos               = Pos(0, 1)
    override val toString = "^"
  }

  object DDown extends DPad {
    val pos               = Pos(1, 1)
    override val toString = "v"
  }

  object DLeft extends DPad {
    val pos               = Pos(1, 0)
    override val toString = "<"
  }

  object DRight extends DPad {
    val pos               = Pos(1, 2)
    override val toString = ">"
  }

  object DA extends DPad {
    val pos                         = Pos(0, 2)
    override def toString(): String = "A"
  }

  object NumPad {
    def gotoVariants(from: NumPad, to: NumPad): Vector[Vector[DPad]] = {
      def rec(cur: Pos, to: Pos, path: Vector[DPad]): Vector[Vector[DPad]] = {
        if (cur == to) {
          return Vector(path :+ DA)
        }
        if (cur == Pos(3, 0)) {
          return Vector.empty
        }

        val vert = {
          if (cur.y != to.y) {
            if (cur.y < to.y) {
              val newpos = cur.copy(y = cur.y + 1)
              rec(newpos, to, path :+ DDown)
            } else {
              val newpos = cur.copy(y = cur.y - 1)
              rec(newpos, to, path :+ DUp)
            }
          } else {
            Vector.empty
          }
        }

        val horiz = {
          if (cur.x != to.x) {
            if (cur.x < to.x) {
              val newpos = cur.copy(x = cur.x + 1)
              rec(newpos, to, path :+ DRight)
            } else {
              val newpos = cur.copy(x = cur.x - 1)
              rec(newpos, to, path :+ DLeft)
            }
          } else {
            Vector.empty
          }
        }

        vert ++ horiz
      }

      rec(from.pos, to.pos, Vector.empty)
    }
    def goto(from: NumPad, to: NumPad): List[DPad] = {
      var l   = List.empty[DPad]
      var cur = from.pos

      while (cur.y > to.pos.y) {
        l = DUp +: l
        cur = cur.copy(y = cur.y - 1)
      }
      while (cur.x < to.pos.x) {
        l = DRight +: l
        cur = cur.copy(x = cur.x + 1)
      }
      while (cur.y < to.pos.y) {
        l = DDown +: l
        cur = cur.copy(y = cur.y + 1)
      }
      while (cur.x > to.pos.x) {
        l = DLeft +: l
        cur = cur.copy(x = cur.x - 1)
      }

      if (cur == to.pos) {
        return DA +: l
      } else {
        assert(false)
        return l
      }
    }

    val posMap: Map[Pos, NumPad] = Map(
      Pos(0, 0) -> Nr7,
      Pos(0, 1) -> Nr8,
      Pos(0, 2) -> Nr9,
      Pos(1, 0) -> Nr4,
      Pos(1, 1) -> Nr5,
      Pos(1, 2) -> Nr6,
      Pos(2, 0) -> Nr1,
      Pos(2, 1) -> Nr2,
      Pos(2, 2) -> Nr3,
      Pos(3, 1) -> Nr0,
      Pos(3, 2) -> NrA,
    )

  }
  object DPad {

    val gotoVCache: mu.Map[(DPad, DPad), Vector[Vector[DPad]]] = mu.Map.empty
    def gotoVariants(from: DPad, to: DPad): Vector[Vector[DPad]] = {
      if(gotoVCache.contains((from, to))) {
        return gotoVCache((from, to))
      }
      def rec(cur: Pos, to: Pos, path: Vector[DPad]): Vector[Vector[DPad]] = {
        if (cur == to) {
          return Vector(path :+ DA)
        }
        if (cur == Pos(0, 0)) {
          return Vector.empty
        }

        val vert = {
          if (cur.y != to.y) {
            if (cur.y < to.y) {
              val newpos = cur.copy(y = cur.y + 1)
              rec(newpos, to, path :+ DDown)
            } else {
              val newpos = cur.copy(y = cur.y - 1)
              rec(newpos, to, path :+ DUp)
            }
          } else {
            Vector.empty
          }
        }

        val horiz = {
          if (cur.x != to.x) {
            if (cur.x < to.x) {
              val newpos = cur.copy(x = cur.x + 1)
              rec(newpos, to, path :+ DRight)
            } else {
              val newpos = cur.copy(x = cur.x - 1)
              rec(newpos, to, path :+ DLeft)
            }
          } else {
            Vector.empty
          }
        }

        vert ++ horiz
      }
      val res = rec(from.pos, to.pos, Vector.empty)
      gotoVCache((from, to)) = res
      res
    }



    val gotoCache: mu.Map[(DPad, DPad), Vector[DPad]] = mu.Map.empty
    def goto(from: DPad, to: DPad): Vector[DPad] = {
      if (gotoCache.contains((from, to))) {
        return gotoCache((from, to))
      }
      var l   = List.empty[DPad]
      var cur = from.pos

      while (cur.y > to.pos.y) {
        l = DUp +: l
        cur = cur.copy(y = cur.y - 1)
      }

      while (cur.y < to.pos.y) {
        l = DDown +: l
        cur = cur.copy(y = cur.y + 1)
      }

      while (cur.x < to.pos.x) {
        l = DRight +: l
        cur = cur.copy(x = cur.x + 1)
      }
      while (cur.x > to.pos.x) {
        l = DLeft +: l
        cur = cur.copy(x = cur.x - 1)
      }

      if (cur == to.pos) {
        val res = (DA +: l).toVector
        gotoCache((from, to)) = res
        return res
      } else {
        assert(false)
        return l.toVector
      }
    }

    val posMap: Map[Pos, DPad] = Map(
      Pos(0, 1) -> DUp,
      Pos(1, 1) -> DDown,
      Pos(1, 0) -> DLeft,
      Pos(1, 2) -> DRight,
      Pos(0, 2) -> DA,
    )
  }
  
  def num2DpadVariants(input: Vector[NumPad]): Vector[Vector[DPad]] = {
    val ini: (Vector[Vector[DPad]], NumPad) = (Vector.empty, NrA)
    val (translation, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, prev)                 = acc
        val oneMove: Vector[Vector[DPad]] = NumPad.gotoVariants(prev, ele)
        val newvariants = oneMove.flatMap(newele => {
          val newvariant = {
            if (sofar.isEmpty) {
              Vector(newele)
            } else {
              sofar.map(sofarvariant => sofarvariant ++ newele)
            }
          }
          newvariant
        })
        (newvariants, ele)
      })
    }
    translation
  }

  def pad2padVariants(input: Vector[DPad]): Vector[Vector[DPad]] = {
    val ini: (Vector[Vector[DPad]], DPad) = (Vector.empty, DA)
    val (translation, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, prev)                 = acc
        val oneMove: Vector[Vector[DPad]] = DPad.gotoVariants(prev, ele)
        val newvariants = oneMove.flatMap(newele => {
          val newvariant = {
            if (sofar.isEmpty) {
              Vector(newele)
            } else {
              sofar.map(sofarvariant => sofarvariant ++ newele)
            }
          }
          newvariant
        })
        (newvariants, ele)
      })
    }
    translation
  }

  def num2Dpad(input: Vector[NumPad]): Vector[DPad] = {
    val ini: (Vector[DPad], NumPad) = (Vector.empty, NrA)
    val (translation, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, prev)         = acc
        val oneMove: Vector[DPad] = NumPad.goto(prev, ele).toVector.reverse
        val nlist: Vector[DPad]   = sofar ++ oneMove
        (nlist, ele)
      })
    }
    translation
  }

  def dpad2Num(input: Vector[DPad]): Vector[NumPad] = {
    val apos                       = NrA.pos
    val ini: (Vector[NumPad], Pos) = (Vector.empty, apos)
    val (res, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, cur) = acc
        ele match
          case DUp =>
            (sofar, cur.copy(y = cur.y - 1))
          case DDown =>
            (sofar, cur.copy(y = cur.y + 1))
          case DLeft =>
            (sofar, cur.copy(x = cur.x - 1))
          case DRight =>
            (sofar, cur.copy(x = cur.x + 1))
          case DA =>
            (sofar :+ NumPad.posMap(cur), cur)
      })
    }
    res
  }

  def dpadEncode(input: Vector[DPad]): Vector[DPad] = {
    val ini: (Vector[DPad], DPad) = (Vector.empty, DA)
    val (translation, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, prev)         = acc
        val oneMove: Vector[DPad] = DPad.goto(prev, ele).toVector.reverse
        val nlist: Vector[DPad]   = sofar ++ oneMove
        (nlist, ele)
      })
    }
    translation
  }

  def dpadDecode(input: Vector[DPad]): Vector[DPad] = {
    val apos                     = DA.pos
    val ini: (Vector[DPad], Pos) = (Vector.empty, apos)
    val (res, _) = {
      input.foldLeft(ini)((acc, ele) => {
        val (sofar, cur) = acc
        ele match
          case DUp =>
            (sofar, cur.copy(y = cur.y - 1))
          case DDown =>
            (sofar, cur.copy(y = cur.y + 1))
          case DLeft =>
            (sofar, cur.copy(x = cur.x - 1))
          case DRight =>
            (sofar, cur.copy(x = cur.x + 1))
          case DA =>
            (sofar :+ DPad.posMap(cur), cur)
      })
    }
    res
  }

  val costCache2: mu.Map[(NumPad, NumPad, Long), Long] = mu.Map.empty
  def get_cost(a: NumPad, b: NumPad, depth: Long): Long = {
    if(costCache2.contains((a,b,depth))) {
      return costCache2((a,b,depth))
    }
    if(depth == 0) {
      NumPad.gotoVariants(a,b).map(x => x.size).min
    } else{
      val variants = NumPad.gotoVariants(a, b)
      val min_cost = variants.map(seq2 => {
        val seq = DA +: seq2 
        var cost = 0L
        for (i <- 0 until seq.size - 1) {
          val a = seq(i)
          val b = seq(i + 1)
          cost = cost + get_cost(a, b, depth - 1)
        }
        cost
      }).min
      min_cost
    }
  }


  val costCache: mu.Map[(DPad, DPad, Long), Long] = mu.Map.empty
  def get_cost(a: DPad, b: DPad, depth: Long): Long = {
    if(costCache.contains((a,b,depth))) {
      return costCache((a,b,depth))
    }
    if(depth == 0) {
      val mins = DPad.gotoVariants(a,b).map(x => x.size).min
      mins
    } else{
      val variants = DPad.gotoVariants(a, b)
      val min_cost = variants.map(seq2 => {
        val seq = DA +: seq2
        var cost = 0L
        for (i <- 0 until seq.size - 1) {
          val a = seq(i)
          val b = seq(i + 1)
          cost = cost + get_cost(a, b, depth - 1)
        }
        cost
      }).min
      costCache((a,b,depth)) = min_cost
      min_cost
    }
  }


  def char2Numpad(c: Char): NumPad = {
    c match {
      case '7' =>
        Nr7
      case '8' =>
        Nr8
      case '9' =>
        Nr9
      case '4' =>
        Nr4
      case '5' =>
        Nr5
      case '6' =>
        Nr6
      case '1' =>
        Nr1
      case '2' =>
        Nr2
      case '3' =>
        Nr3
      case '0' =>
        Nr0
      case 'A' =>
        NrA
    }
  }
  def dir2Dpad(c: Char): DPad = {
    c match {
      case '^' =>
        DUp
      case 'v' =>
        DDown
      case '<' =>
        DLeft
      case '>' =>
        DRight
      case 'A' =>
        DA
    }
  }

  def get_code_cost(code: Vector[NumPad], depth: Int): Long = {
    val res = code.sliding(2).map { case Vector(a, b) => get_cost(a, b, depth).toLong }.sum
    res
  }

  @main
  def day21_02(): Unit = {

    val numpadInputs = inputCharVecs.map(row => row.map(x => char2Numpad(x)))
    val prepend = numpadInputs.map(x => NrA +: x)
    prepend.foreach(x => println(x))

    //get_code_cost(prepend.head,1)
    val sizes =prepend.map(numpad => get_code_cost(numpad,25))
    
    val complexity2 = sizes.zip(inputNums).map(x => x._1 * x._2.toLong).sum
    println(complexity2)
  }

  @main
  def day21_01(): Unit = {
    inputStrings.foreach(println(_))
    val numpadInputs = inputCharVecs.map(row => row.map(x => char2Numpad(x)))

    val sizes = numpadInputs.map(x => {
      val res = num2DpadVariants(x).flatMap { num2Dpad =>
        pad2padVariants(num2Dpad).flatMap { pad2pad =>
          pad2padVariants(pad2pad).map { pad2pad2 =>
            // val x = dpad2Num(dpadDecode(dpadDecode(pad2pad2)))
            // println(pad2pad2.size)
            pad2pad2
          }
        }
      }
      res.map(x => x.size).min
    })

    val complexity2 = sizes.zip(inputNums).map(x => x._1 * x._2).sum
    println(complexity2)
  }

  case class TreeNode(id: String, children: List[TreeNode])

  def generateDotFile(tree: TreeNode, filename: String): Unit = {
    val writer = new java.io.PrintWriter(filename)
    writer.println("digraph G {")
    def writeNode(node: TreeNode): Unit = {
      node
        .children
        .foreach { child =>
          writer.println(s""""${node.id}" -> "${child.id}";""")
          writeNode(child)
        }
    }
    writeNode(tree)
    writer.println("}")
    writer.close()
  }

  @main
  def day21_exploreDPadMut(): Unit = {
    val allDaps = Vector(DDown, DUp, DLeft, DRight)
    val seq     = allDaps.map(nr => Vector(nr, DA))

    val sizes = seq.map(x => {
      val res = pad2padVariants(x).flatMap { num2Dpad =>
        pad2padVariants(num2Dpad).flatMap { pad2pad =>
          pad2padVariants(pad2pad).map { pad2pad2 =>
            // val x = dpad2Num(dpadDecode(dpadDecode(pad2pad2)))
            // println(pad2pad2.size)
            pad2pad2
          }
        }
      }
      res.map(x => x.size).toSet
    })
    sizes.map(println(_))
  }

  def createLeafs(pad: Vector[DPad], idsofar: String, depth: Int): List[TreeNode] = {
    val vars = pad2padVariants(pad)
    vars
      .zipWithIndex
      .map { (pad2pad2, id2) =>
        {
          if (depth == 0) {
            // TreeNode(s"${id0}_${id1}_${id2}", List(TreeNode(s"${id0}${id1}${id2}size_{${pad2pad2.size}}", List.empty)))
            TreeNode(s"${idsofar}_${id2}", List(TreeNode(s"size_{${pad2pad2.size}}", List.empty)))
            // TreeNode(s"${id0}_${id1}_${id2}", List(TreeNode(s"size_{${pad2pad2.size}}", List.empty)))
          } else {
            TreeNode(s"${idsofar}_${id2}", createLeafs(pad2pad2, s"${idsofar}_${id2}", depth - 1))
          }
        }
      }
      .toList

  }

  @main
  def day21_exploreDPad(): Unit = {
    // val allDaps = Vector(DDown, DUp, DLeft, DRight)
    val allDaps = Vector(DLeft)
    val seq     = allDaps.map(nr => Vector(nr, DA))
    seq.map(x => {
      val children = createLeafs(x, "", 1)

      // Your existing code to generate the tree
      val root = TreeNode("root", children.toList)
      // Generate DOT file
      val filename = {
        x.head match
          case DUp =>
            "up"
          case DDown =>
            "down"
          case DLeft =>
            "left"
          case DRight =>
            "right"
          case DA =>
            "a"
      }

      generateDotFile(root, s"${filename}.dot")
    })
  }
  @main
  def day21_explore(): Unit = {
    val numPadNums = Vector(Nr0, Nr1, Nr2, Nr3, Nr4, Nr5, Nr6, Nr7, Nr8, Nr9)
    val seq        = numPadNums.map(nr => Vector(nr, NrA))
    seq.map(x => {
      val children = num2DpadVariants(x)
        .zipWithIndex
        .map { (num2Dpad, id0) =>
          val children = pad2padVariants(num2Dpad)
            .zipWithIndex
            .map { (pad2pad, id1) =>
              {
                val children = pad2padVariants(pad2pad)
                  .zipWithIndex
                  .map { (pad2pad2, id2) =>
                    {
                      // TreeNode(s"${id0}_${id1}_${id2}", List(TreeNode(s"${id0}${id1}${id2}size_{${pad2pad2.size}}", List.empty)))
                      TreeNode(
                        s"${id0}_${id1}_${id2}",
                        List(TreeNode(s"${id0}${id1}size_{${pad2pad2.size}}", List.empty)),
                      )
                      // TreeNode(s"${id0}_${id1}_${id2}", List(TreeNode(s"size_{${pad2pad2.size}}", List.empty)))
                    }
                  }
                TreeNode(s"${id0}_${id1}", children.toList)
              }
            }
          TreeNode(s"${id0}", children.toList)
        }

      // Your existing code to generate the tree
      val root = TreeNode("root", children.toList)
      // Generate DOT file
      generateDotFile(root, s"${x.head.toString.take(3)}.dot")
    })
  }
}
