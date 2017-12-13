package proofofliability

import proofofliability.MerkleTree._

object Proof {

  case class ProofOfLiability(
      partialProofs: Seq[Node]
  ) {
    def isValid(rootDigest: String, account: Account): Boolean = {
      val partialAccounts = Tree.splitBySize(account, 1)
      partialProofs.map { proofRoot =>
        rootDigest == proofRoot.id && checkSubtreeProof(proofRoot, partialAccounts)
      }.reduce(_ && _)
    }
  }

  def checkNodeId(node: Node): Boolean = {
    !node.isLeaf && node.id == Node.mkIdHash(node.leftHash.get, node.rightHash.get, node.totalValue)
  }

  //TODO check for non decreasing node values ?
  private def checkSubtreeProof(node: Node, partialAccLeaf: Seq[Node]): Boolean = {

    if (node.isLeaf)
      return partialAccLeaf.exists(leaf => node.id == leaf.id)

    if (node.right.isDefined)
      return checkNodeId(node) && checkSubtreeProof(node.right.get, partialAccLeaf)

    if (node.left.isDefined)
      return checkNodeId(node) && checkSubtreeProof(node.left.get, partialAccLeaf)

    false
  }

}
