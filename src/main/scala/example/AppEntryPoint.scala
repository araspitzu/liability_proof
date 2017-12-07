package example

import MerkleTree._
import Proof._

object AppEntryPoint extends App {

  val users = Seq(
    Account("Satoshi", 100),
    Account("Clara", 30),
    Account("Erik", 20),
    Account("Luke", 65),
    Account("Lucy", 30),
    Account("Andrea", 80),
    Account("Mike", 25),
    Account("Dave", 5),
    Account("Mallory", 11),
    Account("Alice", 38),
    Account("Bob", 108),
    Account("Guido Van Rossum", 5),
    Account("Hodler", 190),
    Account("Scammer", 11),
    Account("Trader", 200),
    Account("Shiller", 1),
    Account("Frida", 99)
  )

  println(s"Using users DB of ${users.size} users")
  println("Creating merkle tree...")
  val tree = Tree(users)
  val rootDigest = tree.root.id
  println(s"Root digest: $rootDigest")
  println(s"Number of nodes: ${tree.numNodes}")
  println(s"Max depth: ${tree.maxDepth}")

  val accountToCheck = Account("Andrea", 80)
  println(s"Searching proof path for '$accountToCheck'")
  val accountProof = tree.findProofByAccount(accountToCheck)
  println(s"Proof found: ${accountProof.isDefined}, got ${accountProof.map(_.numNodes)} nodes in it")
  if (accountProof.isDefined) {
    println(accountProof.get.toString)
    val proof = ProofOfLiability(accountProof.get)
    println(s"Is proof valid? ${ProofOfLiability.isValid(rootDigest, proof, accountToCheck)}")
  }

}
