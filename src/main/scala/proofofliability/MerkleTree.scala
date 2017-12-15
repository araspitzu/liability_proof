package proofofliability

import Util._
import proofofliability.Proof.ProofOfLiability

object MerkleTree {

  case class Tree(
      private[proofofliability] val root: Node,
      private[proofofliability] val accountToLeaves: Map[Account, Seq[Node]],
      private val splitStrategy: SplitStrategy
  ) {

    def rootDigest = root.id

    def totalBalance = root.totalValue

    def hasProofFor(account: Account): Boolean = findProofByAccount(account).isDefined

    def findProofByAccount(account: Account): Option[ProofOfLiability] = {
      accountToLeaves.get(account).map { nodes =>
        val partialProofs = nodes.map(_.rndIndex.get).map(mkProofPath(root, _)).flatten
        ProofOfLiability(partialProofs,splitStrategy)
      }
    }

    private def mkProofPath(node: Node, index: Double): Option[Node] = {
      if (node.isLeaf && node.rndIndex == Some(index)) {
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
    
    def fromStrategy[T <: SplitStrategy](accounts: Seq[Account], strategy: T): Tree = {
      val accountToLeaves = scrambleAccounts(accounts, strategy)
      val randomizedLeaves = accountToLeaves.values.flatten.toSeq.sortBy(_.rndIndex)
      val rootNode = mkTree(randomizedLeaves)
  
      Tree(rootNode, accountToLeaves, strategy)
    }
  
    private def scrambleAccounts[T <: SplitStrategy](input: Seq[Account], strategy: T): Map[Account, Seq[MerkleTree.Node]] = {
      input.map(acc => acc -> strategy.split(acc)).toMap
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
      rndIndex: Option[Double] = None
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
      sha256Hex(s"$leftHash | $rightHash | $totalValue")

    private[proofofliability] def mkLeafId(account: Account): String =
      sha256Hex(s"${account.user} | ${account.balance}")

    def mkLeaf(account: Account) = Node(
      id = mkLeafId(account),
      totalValue = account.balance,
      rndIndex = Some(math.random)
    )

  }

}
