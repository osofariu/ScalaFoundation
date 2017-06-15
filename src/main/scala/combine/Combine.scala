package combine

object Combine {

  def variableZip(itemLists: List[List[Int]]): List[List[Int]] = {
    def combineEach(itemList: List[Int], restList: List[List[Int]]): List[List[Int]] = {
      if (itemList.isEmpty)
        restList
      else for {
        item: Int <- itemList
        rest: List[Int] <- restList
      } yield item :: rest
    }

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

  def permutations[T](items: Set[T]): Set[List[T]] = {
    if (items.isEmpty) Set(Nil)
    else for {
      item <- items
      rest <- permutations[T](items - item)
    } yield item :: rest
  }

  def combinationsCk[T](items: Set[T], i: Int): Set[Set[T]] = {
    def helper(items: Set[T], i: Int): Set[Set[T]] = {
      if (items.isEmpty || i <= 0) Set(Set())
      else for {
        item <- items
        rest <- helper(items - item, i - 1)
      } yield rest + item
    }
    helper(items, i)
  }

  def combinations[T](items: Set[T]): Set[Set[T]] = {
    def helper(size: Int): Set[Set[T]] = {
      if (size == 0)
        Set(Set())
      else
        combinationsCk(items, size) ++ helper(size - 1)
    }
    helper(items.size)
  }

  def combinationsAlt[T](items: Set[T]): Set[Set[T]] = {
    if (items.isEmpty)
      Set(Set())
    else
      (for {
        i <- 1 to items.size
        itemsGroup <- items take i
        itemsRest <- combinationsAlt(items drop i)
      } yield itemsRest + itemsGroup).toSet + Set()
  }

  def combinationsCount(n: Int, k: Int): Int = {
    factorial(n) / (factorial(k) * factorial(n - k))
  }
}

