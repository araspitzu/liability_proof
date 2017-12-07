package testexample

import scala.io.Source
import example.MerkleTree.{Account, Tree}
import example.Proof.{ProofOfLiability}
import org.scalatest._
import org.json4s._
import org.json4s.jackson.JsonMethods._

class ProofSpec extends FlatSpec with Matchers {
  
  def resourceAsString(fileName: String) = Source.fromURL(getClass.getResource(fileName)).mkString
  
  implicit val formats = DefaultFormats
  
  val passingTestMock = resourceAsString("/passing_test.json")
  val accountsTestMock = resourceAsString("/accounts.json")
  
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
  
  it should "validate the proof from external mock data account.json" in {
    val users = parse(accountsTestMock).extract[List[Account]]
    
    val expectedNumNodes = 33
    
    val tree = Tree(users)
    val rootDigest = tree.root.id
    
    val accountToCheck = Account("mark", 462)
    val proofRoot = tree.findProofByAccount(accountToCheck)
    
    
    tree.numNodes shouldBe 33
    tree.root.totalValue shouldBe 37618
    
    proofRoot.isDefined shouldBe true
    
    val proof = ProofOfLiability(proofRoot.get)
    ProofOfLiability.isValid(rootDigest, proof, accountToCheck) shouldBe true
    
    //Also othe validation should fail if the account name  or balance is incorrect
    ProofOfLiability.isValid(rootDigest, proof, Account("mark", 666)) shouldBe false
    ProofOfLiability.isValid(rootDigest, proof, Account("markzz", 462)) shouldBe false
    
  }
  

}
