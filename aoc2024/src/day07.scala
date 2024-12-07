import os._
import scala.util.boundary, boundary.break

object day07 {
/*
  @main 
  def day07_03(): Unit = {
  }
*/

  @main
  def day07_01(): Unit = {
    val filePath = os.resource / "day7.txt"

    val lists = os.read.lines(filePath).map { line =>
      parseStringToLongList(line)
    }
    val checks = lists.map(checkLine(_))
    println(checks.sum)
  }

  def parseStringToLongList(input: String): List[Long] = {
    // Split the string by ":"
    val parts = input.split(":")

    // Check if the input format is correct
    if (parts.length != 2) {
      throw new IllegalArgumentException("Input string format is incorrect")
    }

    // Get the part after the colon and trim any whitespace
    val numbersPart = parts(1).trim

    // Split the numbers part by spaces and convert each to Long
    val numbersList = numbersPart.split(" ").map(_.toLong).toList

    parts(0).toLong :: numbersList
  }


  def checkLine(line: List[Long]): Long = {
    val result = line.head

    val rest = line.tail

    var l = List.empty[Long]
    /* 
    val muls = rest.foldLeft(List(1L)){
        (acc,ele) => {
            (acc.head * ele) +: acc
        }
    }
    
    val adds = rest.foldLeft(List(0L)){
        (acc,ele) => {
            (acc.head + ele) +: acc
        }
    }
    */
    if( bruteForce(result, rest))
        result
    else 
        0

    //if(recurse(result, rest.reverse, muls, adds))
    //    result
    //else 
    //    0
  }


  def bruteForce(sum: Long, nums: List[Long]): Boolean = {
    val head = nums.head 
    return rec(nums.tail,head, sum) 
}

  def rec(nums: List[Long], count: Long, sum: Long): Boolean = {
    if (count > sum) return false
    if (nums.isEmpty)
        if(count == sum) return true
        else 
            return false
    else{
        val cur = nums.head
        return rec(nums.tail, count + cur, sum) || rec(nums.tail, count*cur, sum)
    }
  }



  def recurse(sum: Long, nums: List[Long], muls: List[Long], adds: List[Long]): Boolean = {
    //if (muls.head == sum) return true
    //if (adds.head == sum) return true

    if(nums.size == 2) return false

    //if(sum > muls.head && sum > adds.head) return false
    //if(sum < adds.head && sum < muls.head) return false

    recurse(sum - nums.head, nums.tail, muls.tail, adds.tail) || recurse(sum / nums.head, nums.tail, muls.tail, adds.tail)

  }


  def checkLine2(line: List[Long]): Long = {
    val result = line.head

    val rest = line.tail

    var l = List.empty[Long]
    if( rec2(rest.tail, rest.head,result))
        result
    else 
        0
  }



  def rec2(nums: List[Long], count: Long, sum: Long): Boolean = {
    if (count > sum) return false
    if (nums.isEmpty)
        if(count == sum) return true
        else 
            return false
    else{
        val cur = nums.head
        return rec2(nums.tail, count + cur, sum) || rec2(nums.tail, count*cur, sum) || rec2(nums.tail, s"${count.toString()}${cur.toString()}".toLong, sum)
    }
  }

  @main
  def day07_02(): Unit = {
    val filePath = os.resource / "day7.txt"

    val lists = os.read.lines(filePath).map { line =>
      parseStringToLongList(line)
    }
    val checks = lists.map(checkLine2(_))
    println(checks.sum)
  }
}
