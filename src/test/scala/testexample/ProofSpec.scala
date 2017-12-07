package testexample

import scala.io.Source
import example.MerkleTree.{Account, Tree}
import example.Proof.{ProofOfLiability}
import org.scalatest._
import org.json4s._
import org.json4s.jackson.JsonMethods._

class ProofSpec extends FlatSpec with Matchers {
  
  implicit val formats = DefaultFormats
  
  val passingTestMock = Source.fromURL(getClass.getResource("/passing_test.json")).mkString
  
  it should "Construct a tree and a valid proof" in {
    
    val users = parse(passingTestMock).extract[Seq[Account]]
    
    val expectedNumNodes = 15//math.pow(2D, users.length).toInt - 1
    val expectedMaxDepth = 4//math.log(expectedNumNodes + 1) / math.log(2)
    
    val tree = Tree(users)
    val rootDigest = tree.root.id
  
    
    tree.numNodes shouldBe expectedNumNodes
    tree.maxDepth shouldBe expectedMaxDepth
    
    val existingAccountToCheck = Account("Alice", 38)
    val nonExistingAccountToCheck = Account("Mallory", 31)
    val accountProof = tree.findProofByAccount(existingAccountToCheck)
    
    accountProof.isDefined shouldBe true
    tree.findProofByAccount(nonExistingAccountToCheck) shouldBe None
    
    val proof = ProofOfLiability(accountProof.get)
  
    ProofOfLiability.isValid(rootDigest, proof, existingAccountToCheck) shouldBe true
      
  }
    
  

}
