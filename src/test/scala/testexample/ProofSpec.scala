package testexample

import scala.io.Source
import proofofliability.MerkleTree.Tree
import proofofliability.Util._
import org.scalatest._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import proofofliability.Account
import scala.math.pow
import scala.util.Random

class ProofSpec extends FlatSpec with Matchers {

  def resourceAsString(fileName: String) = Source.fromURL(getClass.getResource(fileName)).mkString

  implicit val formats = DefaultFormats

  lazy val passingTestMock = resourceAsString("/mock_data.json")
  lazy val accountsTestMock = resourceAsString("/accounts.json")

  lazy val randomAccounts = Stream.continually(Account(
    user = Random.alphanumeric.take(6).mkString,
    balance = Random.nextInt.abs
  ))

  /**
   *  The merkle tree is balanced and contain the input data on the leaves,
   *  we can compute the number of nodes by getting the smaller power
   *  of 2 which is bigger than the input size; from that we subtract the rest due to
   *  the 'empty' space left on the right hand side of the tree (the tree levels fills left to right).
   *  If the tree is balanced its max depth should be at most log2(numNodes)
   */
  private def checkTreeMetrics(tree: Tree, users: Seq[Account]) = {

    val expectedTotalBalance = users.map(_.balance).sum
    val leavesBalance = tree.leavesBalances
    val treeTotalBalance = tree.totalBalance
    
    val expectedNumNodes = 1 - (2 * (1 - tree.numLeaves))
    val expectedMaxDepth = math.log(expectedNumNodes) / math.log(2)

    tree.numNodes shouldBe expectedNumNodes
    tree.maxDepth shouldBe expectedMaxDepth.toInt +- 1
    expectedTotalBalance shouldBe tree.leavesBalances
    tree.totalBalance shouldBe expectedTotalBalance
    

  }

  it should "construct a tree and a valid proof" in {

    val users = parse(passingTestMock).extract[Seq[Account]]

    val tree = Tree.fromStrategy(users, SplitBySize(3))
    val rootDigest = tree.rootDigest

 //   tree.numNodes shouldBe 15
 //   tree.maxDepth shouldBe 4
 //   tree.totalBalance shouldBe 387

    val existingAccount = Account("Alice", 38)
    val nonExistingAccount = Account("Mallory", 31)
    val Some(proof) = tree.findProofByAccount(existingAccount)

    tree.findProofByAccount(nonExistingAccount) shouldBe None
    proof.isValid(rootDigest, existingAccount) shouldBe true

  }

  it should "validate the proof from external mock data account.json" in {
    val users = parse(accountsTestMock).extract[List[Account]]

    val expectedNumNodes = 33

    val tree = Tree.fromStrategy(users, SplitBySize(3))
    val rootDigest = tree.rootDigest

    val accountToCheck = Account("mark", 462)
    val Some(proof) = tree.findProofByAccount(accountToCheck)

//    tree.numNodes shouldBe expectedNumNodes
//    tree.totalBalance shouldBe 37618

    proof.isValid(rootDigest, accountToCheck) shouldBe true

    //Also the validation should fail if the account name  or balance is incorrect
    proof.isValid(rootDigest, Account("mark", 666)) shouldBe false
    proof.isValid(rootDigest, Account("markzz", 462)) shouldBe false

  }

  it should "not find a proof if the tree does not contain a certain user" in {
    val users = parse(passingTestMock).extract[Seq[Account]]
    val tree = Tree.fromStrategy(users, SplitBySize(3))
    tree.hasProofFor(Account("nope", 12)) shouldBe false
  }

  it should "validate a proof correctly (failing) given a wrong root digest" in {
    val users = parse(passingTestMock).extract[Seq[Account]]
    val tree = Tree.fromStrategy(users, SplitBySize(3))
    val Some(proof) = tree.findProofByAccount(Account("Bob", 108))

    val correctRootDigest = tree.rootDigest
    val wrongDigest = sha256Hex("Yo")

    proof.isValid(correctRootDigest, Account("Bob", 108)) shouldBe true
    proof.isValid(wrongDigest, Account("Bob", 108)) shouldBe false

  }

  it should "be a balanced tree" in {

    //with an exact power of two
    val eightUsers = randomAccounts.take(8).toList
    checkTreeMetrics(Tree.fromStrategy(eightUsers, SplitBySize(3)), eightUsers)

    //with power of two - 1
    val fourteen = randomAccounts.take(14).toList
    checkTreeMetrics(Tree.fromStrategy(fourteen, SplitBySize(3)), fourteen)

    //with power of two + 1
    val seventeen = randomAccounts.take(17).toList
    checkTreeMetrics(Tree.fromStrategy(seventeen, SplitBySize(3)), seventeen)

    //with a lot of users
    val manyUsers = randomAccounts.take(8712).toList
    checkTreeMetrics(Tree.fromStrategy(manyUsers, SplitBySize(3)), manyUsers)

  }

}
