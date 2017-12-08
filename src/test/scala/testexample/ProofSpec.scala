package testexample

import scala.io.Source
import proofofliability.MerkleTree.{Account, Tree}
import proofofliability.MerkleTree._
import org.scalatest._
import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.math.pow
import scala.util.Random

class ProofSpec extends FlatSpec with Matchers {

  def resourceAsString(fileName: String) = Source.fromURL(getClass.getResource(fileName)).mkString

  implicit val formats = DefaultFormats

  lazy val passingTestMock = resourceAsString("/mock_data.json")
  lazy val accountsTestMock = resourceAsString("/accounts.json")
  
  lazy val randomAccounts:Stream[Account] = Account(
    user = Random.alphanumeric.take(6).mkString,
    balance = Random.nextInt
  ) #:: randomAccounts
  
  /**
    *  The merkle tree we build should always be balanced and contain the account data on the leaves,
    *  we can compute the number of nodes by getting the biggest-closest power
    *  of 2 to the number of accounts, then if the number is not a perfect square we subtract the rest.
    *  If the tree is balanced its max depth should be at most log2(numNodes)
    */
  private def checkTreeMetrics(tree: Tree, users: Seq[Account]) = {
    
    val expectedTotalBalance = users.map(_.balance).sum
    val expectedNumNodes = {
      val closestBiggerPowOfTwo = (1 to 32 takeWhile (pow(2, _) < users.size)).last + 1
      val rest = pow(2, closestBiggerPowOfTwo) - users.size
      (0 to closestBiggerPowOfTwo).map(pow(2,_)).sum - (2 * rest)
    }
    val expectedMaxDepth = math.log(expectedNumNodes) / math.log(2)


    tree.totalBalance shouldBe expectedTotalBalance
    tree.maxDepth shouldBe (expectedMaxDepth.toInt +- 1)
    tree.numNodes shouldBe expectedNumNodes
  
  }

  it should "Construct a tree and a valid proof" in {

    val users = parse(passingTestMock).extract[Seq[Account]]
    
    val tree = Tree(users)
    val rootDigest = tree.rootDigest

    tree.numNodes shouldBe 15
    tree.maxDepth shouldBe 4
    tree.totalBalance shouldBe 387

    val existingAccount= Account("Alice", 38)
    val nonExistingAccount = Account("Mallory", 31)
    val Some(proof) = tree.findProofByAccount(existingAccount)

    tree.findProofByAccount(nonExistingAccount) shouldBe None
    proof.isValid(rootDigest, existingAccount) shouldBe true

  }

  it should "validate the proof from external mock data account.json" in {
    val users = parse(accountsTestMock).extract[List[Account]]

    val expectedNumNodes = 33

    val tree = Tree(users)
    val rootDigest = tree.rootDigest

    val accountToCheck = Account("mark", 462)
    val Some(proof) = tree.findProofByAccount(accountToCheck)

    tree.numNodes shouldBe expectedNumNodes
    tree.totalBalance shouldBe 37618
    
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
  
  it should "be a balanced tree" in {
    
    //with power of two
    val eightUsers = randomAccounts.take(8).toList
    checkTreeMetrics(Tree(eightUsers), eightUsers)
  
    //with power of two - 1
    val fourteen = randomAccounts.take(14).toList
    checkTreeMetrics(Tree(fourteen), fourteen)
    
    //with power of two + 1
    val seventeen = randomAccounts.take(17).toList
    checkTreeMetrics(Tree(seventeen), seventeen)
    
    //with a lot of users
    val manyUsers = randomAccounts.take(8712).toList
    checkTreeMetrics(Tree(manyUsers), manyUsers)
    
  }
  
}
