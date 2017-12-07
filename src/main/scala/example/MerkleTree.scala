package example

import java.security.MessageDigest
import scala.math._

object MerkleTree {

  case class Account(
      name: String,
      balance: Int
  ) extends Ordered[Account] {
    //lexicographical ordering
    def compare(that: Account): Int = this.name.compareTo(that.name)

  }

  case class Tree(
      root: Node
  ) {

    def totalBalance = root.totalValue

    def findProofByAccount(account: Account): Option[Tree] = {
      mkProofPath(root, account) map (Tree(_))
    }

    private def mkProofPath(node: Node, account: Account): Option[Node] = {
      if (node.isLeaf && node.id == Node.mkLeafId(account)) {
        return Some(node.copy())
      }

      if (!node.isLeaf) {
        val leftBranch = mkProofPath(node.left.get, account)

        if (leftBranch.isDefined)
          return Some(node.copy(left = leftBranch, right = None))

        val rightBranch = mkProofPath(node.right.get, account)

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

  }

  object Tree {
    //TODO scramble account ordering?
    def apply(accounts: Seq[Account]): Tree = Tree(mkTree(accounts.sorted))
  }

  case class Node(
      // Hash of the concatenation of child hashes + total balance
      id: String,
      // The combined value of the subtrees, or account value if this node is a leaf
      totalValue: Int,
      // The value of the subtree on the left
      leftValue: Int = 0,
      // The value of the subtree on the right
      rightValue: Int = 0,
      // Hash pointer to left child ID's
      leftHash: Option[String] = None,
      // Hash pointer to left child ID's
      rightHash: Option[String] = None,
      // left child
      left: Option[Node] = None,
      // right child
      right: Option[Node] = None
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

    def mkIdHash(leftHash: String, rightHash: String, totalValue: Int): String =
      sha256(leftHash ++ rightHash ++ s"$totalValue")

    def mkLeafId(account: Account): String =
      sha256(account.name ++ s"${account.balance}")

  }

  lazy val md = MessageDigest.getInstance("SHA-256")

  def sha256(msg: String): String = {
    md.update(msg.getBytes)
    md.digest().map("%02x".format(_)).mkString
  }

  private def mkTree(accounts: Seq[Account]): Node = {
    accounts match {
      //Leaf
      case singleton :: Nil => Node(
        id = Node.mkLeafId(singleton),
        totalValue = singleton.balance
      )
      //Node
      case moreThanOne =>
        val leftChild = mkTree(accounts.take(accounts.length / 2))
        val rightChild = mkTree(accounts.drop(accounts.length / 2))
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

}
