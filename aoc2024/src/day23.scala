import os._
import scala.collection.{mutable => mu}
import scala.util.matching.Regex
import mu.PriorityQueue
import scala.annotation.tailrec

object day23 {

  val test       = false
  val printstuff = test

  val filePath = {
    if (test) {
      os.resource / "day23_sample.txt"
    } else {
      os.resource / "day23.txt"
    }
  }

  type NodeID = String
  val inputStrings: Vector[String] = os.read.lines(filePath).toVector
  val node2node                    = inputStrings.map(_.split("-").map(_.trim).toVector)

  case class GraphInfo(
      val graph: Map[NodeID, Set[NodeID]],
      val tset: Set[NodeID],
      val nrNeighbors: Map[Int, Set[NodeID]],
  )

  val graphinfo = {
    val withoutNrNeigh = {
      node2node
        .map(x => (x(0), x(1)))
        .foldLeft(GraphInfo(Map.empty[NodeID, Set[NodeID]], Set.empty[NodeID], Map.empty[Int, Set[NodeID]])) {
          case (info, (from, to)) =>
            val acc         = info.graph
            val tset        = info.tset
            val nrNeighbors = info.nrNeighbors
            val fromto      = acc.updated(from, acc.getOrElse(from, Set.empty) + to)
            val tofrom      = fromto.updated(to, fromto.getOrElse(to, Set.empty) + from)
            val nteset1 = {
              if (from.head == 't') {
                tset + from
              } else {
                tset
              }
            }
            val nteset2 = {
              if (to.head == 't') {
                nteset1 + to
              } else {
                nteset1
              }
            }
            val ninfo = info.copy(graph = tofrom, tset = nteset2)
            ninfo
        }
    }
    withoutNrNeigh
      .graph
      .foldLeft(withoutNrNeigh) {
        case (info, (node, neighbors)) => {
          val nrNeighbors = neighbors.size
          val ninfo = info.copy(nrNeighbors = {
            info.nrNeighbors + (nrNeighbors -> (info.nrNeighbors.getOrElse(nrNeighbors, Set.empty) + node))
          },
          )
          ninfo
        }
      }
  }

  def generateDotFile(graph: Map[NodeID, Set[NodeID]], filename: String): Unit = {
    val writer = new java.io.PrintWriter(filename)
    writer.println("digraph G {")
    graph.foreach { case (from, tos) =>
      tos.foreach { to =>
        writer.println(s""""$from" -> "$to";""")
      }
    }
    writer.println("}")
    writer.close()
  }

  def commonNeighborsSets(
      graph: Map[NodeID, Set[NodeID]],
      thisNodeID: NodeID,
      otherNodeId: NodeID,
  ): Set[Set[NodeID]] = {
    val otherNeighbors    = graph(otherNodeId)
    val thisNodeNeighbors = graph(thisNodeID)
    val commonNodes       = thisNodeNeighbors.intersect(otherNeighbors)
    val res               = commonNodes.map(common => Set(thisNodeID, otherNodeId, common))
    res.foreach(x => assert(x.size == 3))
    res
  }

  def create3Sets(graph: Map[NodeID, Set[NodeID]], tset: Set[NodeID]): Set[Set[NodeID]] = {
    val allt3sets = tset.map { tnodeid =>
      {
        val neighbors = graph(tnodeid)
        val tnodeid3sets = neighbors
          .toVector
          .map { neighbor =>
            commonNeighborsSets(graph, tnodeid, neighbor)
          }
        tnodeid3sets.reduce(_ ++ _)
      }
    }
    allt3sets.reduce(_ ++ _)
  }

  def findLargestClique(graph: Map[NodeID, Set[NodeID]]): Set[NodeID] = {
    def isClique(nodes: Set[NodeID]): Boolean = {
      nodes.forall(node => nodes.subsetOf(graph(node) + node))
    }

    def findClique(nodes: Set[NodeID], potential: mu.Set[NodeID], excluded: mu.Set[NodeID]): Set[NodeID] = {
      if (potential.isEmpty && excluded.isEmpty) {
        return nodes
      }

      var maxClique = nodes
      for (node <- potential) {
        val newNodes     = nodes + node
        val newPotential = potential.intersect(graph(node))
        val newExcluded  = excluded.intersect(graph(node))
        val clique       = findClique(newNodes, newPotential, newExcluded)
        if (clique.size > maxClique.size) {
          maxClique = clique
        }
        potential -= node
        excluded += node
      }
      maxClique
    }
    val kset: mu.Set[NodeID] = mu.Set(graph.keySet.toSeq: _*)
    findClique(Set.empty, kset, mu.Set.empty)
  }
  @main
  def day23_01(): Unit = {
    val graph     = graphinfo.graph
    val tset      = graphinfo.tset
    val threesets = create3Sets(graph, tset)
    // threesets.foreach(x => println(x))
    println(threesets.size)
    generateDotFile(graph, "graph.dot")
  }

  @main
  def day23_02(): Unit = {
    val graph         = graphinfo.graph
    val largestClique = findLargestClique(graph)
    val sortedClique  = largestClique.toVector.sorted
    println(s"Largest clique size: ${largestClique.size}")
    println(s"Largest clique nodes: ${sortedClique.mkString(",")}")
  }
}
