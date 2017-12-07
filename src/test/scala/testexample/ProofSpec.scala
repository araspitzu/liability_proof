package testexample

import scala.io.Source
import example.MerkleTree.{ Account, Tree }
import example.MerkleTree._
import example.Proof.{ ProofOfLiability }
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

    val expectedNumNodes = 15 //math.pow(2D, users.length).toInt - 1
    val expectedMaxDepth = 4 //math.log(expectedNumNodes + 1) / math.log(2)

    val tree = Tree(users)
    val rootDigest = tree.rootDigest

    tree.numNodes shouldBe expectedNumNodes
    tree.maxDepth shouldBe expectedMaxDepth

    val existingAccountToCheck = Account("Alice", 38)
    val nonExistingAccountToCheck = Account("Mallory", 31)
    val Some(accountProof) = tree.findProofByAccount(existingAccountToCheck)

    tree.findProofByAccount(nonExistingAccountToCheck) shouldBe None
    
    accountProof.isValid(rootDigest, existingAccountToCheck) shouldBe true

  }

  it should "validate the proof from external mock data account.json" in {
    val users = parse(accountsTestMock).extract[List[Account]]

    val expectedNumNodes = 33

    val tree = Tree(users)
    val rootDigest = tree.rootDigest

    val accountToCheck = Account("mark", 462)
    val Some(proof) = tree.findProofByAccount(accountToCheck)

    tree.numNodes shouldBe expectedNumNodes
    tree.root.totalValue shouldBe 37618
    
    proof.isValid(rootDigest, accountToCheck) shouldBe true

    //Also the validation should fail if the account name  or balance is incorrect
    proof.isValid(rootDigest, Account("mark", 666)) shouldBe false
    proof.isValid(rootDigest, Account("markzz", 462)) shouldBe false

  }
  
  it should "not find a proof if the tree does not contain a certain user" in {
    val users = parse(passingTestMock).extract[Seq[Account]]
    val tree = Tree(users)
    tree.hasProofFor(Account("nope", 12)) shouldBe false
  }
  
  it should "validate a proof correctly (failing) given a wrong root digest" in {
    val users = parse(passingTestMock).extract[Seq[Account]]
    val tree = Tree(users)
    val Some(proof) = tree.findProofByAccount(Account("Bob", 108))
    
    val correctRootDigest = tree.rootDigest
    val wrongDigest = sha256("Yo")
    
    proof.isValid(correctRootDigest, Account("Bob", 108)) shouldBe true
    proof.isValid(wrongDigest, Account("Bob", 108)) shouldBe false
    
  }

}
