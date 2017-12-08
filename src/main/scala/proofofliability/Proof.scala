package proofofliability

import proofofliability.MerkleTree._

object Proof {

  case class ProofOfLiability(
      path: Tree
  ) {
    def isValid(rootDigest: String, account: Account): Boolean = {
      rootDigest == path.rootDigest && checkSubtreeProof(path.root, account)
    }
  }

  def checkNodeId(node: Node): Boolean = {
    !node.isLeaf && node.id == Node.mkIdHash(node.leftHash.get, node.rightHash.get, node.totalValue)
  }

  //TODO check for non decreasing node values ?
  private def checkSubtreeProof(node: Node, account: Account): Boolean = {

    if (node.isLeaf)
      return node.id == Node.mkLeafId(account)

    if (node.right.isDefined)
      return checkNodeId(node) && checkSubtreeProof(node.right.get, account)

    if (node.left.isDefined)
      return checkNodeId(node) && checkSubtreeProof(node.left.get, account)

    false
  }

}
