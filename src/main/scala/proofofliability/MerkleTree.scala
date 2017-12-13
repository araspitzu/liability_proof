package proofofliability

import Util._
import proofofliability.Proof.ProofOfLiability

object MerkleTree {

  case class Tree(
      private[proofofliability] val root: Node,
      private[proofofliability] val accountToLeaves: Map[Account, Seq[Node]]
  ) {

    def rootDigest = root.id

    def totalBalance = root.totalValue

    def hasProofFor(account: Account): Boolean = findProofByAccount(account).isDefined

    def findProofByAccount(account: Account): Option[ProofOfLiability] = {
      accountToLeaves.get(account).map { nodes =>
        ProofOfLiability(nodes.map(_.index.get).map(mkProofPath(root, _)).flatten)
      }
    }

    private def mkProofPath(node: Node, index: Double): Option[Node] = {
      if (node.isLeaf && node.index == Some(index)) {
        return Some(node.copy())
      }

      if (!node.isLeaf) {
        val leftBranch = mkProofPath(node.left.get, index)

        if (leftBranch.isDefined)
          return Some(node.copy(left = leftBranch, right = None))

        val rightBranch = mkProofPath(node.right.get, index)

        if (rightBranch.isDefined)
          return Some(node.copy(left = None, right = rightBranch))

      }

      None
    }

    def numNodes: Int = nodesCountNode(root)

    private def nodesCountNode(node: Node): Int = node.isLeaf match {
      case true => 1
      case false =>
        val leftNodes = node.left.map(nodesCountNode).getOrElse(0)
        val rightNodes = node.right.map(nodesCountNode).getOrElse(0)
        leftNodes + rightNodes + 1
    }

    def maxDepth: Int = maxDepthNode(Some(root))

    private def maxDepthNode(optNode: Option[Node]): Int = optNode match {
      case None                       => 0
      case Some(node) if node.isLeaf  => 1
      case Some(node) if !node.isLeaf => math.max(maxDepthNode(node.left), maxDepthNode(node.right)) + 1
    }

    def numLeaves: Int = leavesCountNode(Some(root))

    private def leavesCountNode(node: Option[Node]): Int = node match {
      case None                 => 0
      case Some(n) if n.isLeaf  => 1
      case Some(n) if !n.isLeaf => leavesCountNode(n.left) + leavesCountNode(n.right)
    }

    def leavesBalances: Double = leavesBalancesNode(Some(root))

    private def leavesBalancesNode(someNode: Option[Node]): Double = someNode match {
      case None                       => 0
      case Some(node) if node.isLeaf  => node.totalValue
      case Some(node) if !node.isLeaf => leavesBalancesNode(node.left) + leavesBalancesNode(node.right)
    }

  }

  object Tree {

    val BALANCE_THRESHOLD = 20
    val BALANCE_SPLIT_SIZE = 3

    def apply(accounts: Seq[Account]): Tree = {
      val accountToLeaves = scrambleAccounts(accounts)
      val randomizedLeaves = accountToLeaves.values.flatten.toSeq.sortBy(_.index)
      
//      for( acc <- accountToLeaves.keys ) {
//        val accLeaves = accountToLeaves.getOrElse(acc, Seq.empty)
//        println(s"Account: ${acc.user}, ${acc.balance}  have leaves total value of :${accLeaves.map(_.totalValue).sum}")
//      }
      
      val rootNode = mkTree(randomizedLeaves)

      Tree(rootNode, accountToLeaves)
    }

    private def scrambleAccounts(input: Seq[Account]): Map[Account, Seq[Node]] = {
      input.map( acc => acc -> splitBySize(acc, BALANCE_SPLIT_SIZE) ).toMap
    }

    private def splitUserHashByByte(sha256Hash: String, size: Int): Seq[String] = {
        hexToBytes(sha256Hash).grouped( (32 / size) + (32 % size) ).map(bytesToHex).toSeq
    }

    private[proofofliability] def splitBySize(account: Account, size: Int): Seq[Node] = {
      splitUserHashByByte(Node.mkLeafId(account), size).map { partialHash =>
        Node.mkLeaf(Account(partialHash, account.balance / size))
      }
    }

    private def mkTree(inputLeaves: Seq[Node]): Node = inputLeaves match {
      //Leaf
      case singleton :: Nil => singleton
      //Node
      case moreThanOne =>
        val leftChild = mkTree(inputLeaves.take(inputLeaves.length / 2))
        val rightChild = mkTree(inputLeaves.drop(inputLeaves.length / 2))
        Node(
          id = Node.mkId(leftChild, rightChild),
          totalValue = leftChild.totalValue + rightChild.totalValue,
          leftValue = leftChild.totalValue,
          rightValue = rightChild.totalValue,
          leftHash = Some(leftChild.id),
          rightHash = Some(rightChild.id),
          left = Some(leftChild),
          right = Some(rightChild)
        )
    }

  }

  private[proofofliability] case class Node(
      // Hash of the concatenation of child hashes + total balance
      id: String,
      // The combined value of the subtrees, or account value if this node is a leaf
      totalValue: Double,
      // The value of the subtree on the left
      leftValue: Double = 0,
      // The value of the subtree on the right
      rightValue: Double = 0,
      // Hash pointer to left child ID's
      leftHash: Option[String] = None,
      // Hash pointer to left child ID's
      rightHash: Option[String] = None,
      // left child
      left: Option[Node] = None,
      // right child
      right: Option[Node] = None,
      //
      index: Option[Double] = None
  ) {

    def isLeaf = left.isEmpty && right.isEmpty

    override def toString: String = {
      isLeaf match {
        case true  => s"ID: $id  val:$totalValue"
        case false => s"\nID: $id  val:$totalValue \n  -- L:${left.map(_.toString)} \n  -- R:${right.map(_.toString)}"
      }
    }
  }

  object Node {

    def mkId(left: Node, right: Node): String =
      mkIdHash(left.id, right.id, left.totalValue + right.totalValue)

    //TODO add random nonce
    def mkIdHash(leftHash: String, rightHash: String, totalValue: Double): String =
      sha256(s"$leftHash | $rightHash | $totalValue")

    def mkLeafId(account: Account): String =
      sha256(s"${account.user} | ${account.balance}")

    def mkLeaf(account: Account) = Node(
      id = mkLeafId(account),
      totalValue = account.balance,
      index = Some(math.random)
    )

  }

}
