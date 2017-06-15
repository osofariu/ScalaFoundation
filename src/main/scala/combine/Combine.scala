package combine

object Combine {

  def combineEach(itemList: List[Int], restList: List[List[Int]]): List[List[Int]] = {
    if (itemList.isEmpty)
      restList
    else for {
      item: Int <- itemList
      rest: List[Int] <- restList
    } yield item :: rest
  }

  def variableZip(itemLists: List[List[Int]]): List[List[Int]] = {
    if (itemLists.isEmpty)
      List(Nil)
    else {
      combineEach(itemLists.head, variableZip(itemLists.tail))
    }
  }


  def countPermutations(items: Set[Int]): Int =
    factorial(items.size)

  def factorial(i: Int): Int = {
    if (i <= 1) 1
    else i * factorial(i - 1)
  }

  def permutations[T](items: Set[T]) : Set[List[T]]= {
    if (items.isEmpty) Set(Nil)
    else for {
      item: T <- items
      rest: List[T] <- permutations[T](items - item)
    } yield item :: rest
  }

  /* TODO: Fix this warning:
  Warning:(34, 13) abstract type pattern T is unchecked since it is eliminated by erasure
      item: T <- items
   */

  def combinationsCk(items: Set[Int], i: Int) : Set[Set[Int]] = {
    def helper(items: Set[Int], i: Int): Set[Set[Int]] = {
      if (items.isEmpty || i <= 0) Set(Set())
      else for {
        item <- items
        rest <- helper(items - item, i - 1)
      } yield rest + item
    }
    helper(items,i)
  }

  def combinations(items: Set[Int]) : Set[Set[Int]] = {
    def helper(size: Int): Set[Set[Int]] = {
      if (size == 0)
        Set(Set())
      else
        combinationsCk(items, size) ++ helper(size - 1)
    }
    helper(items.size)
  }
}

