package proofofliability

import proofofliability.MerkleTree.Node
import scala.math.Ordered

case class Account(
    user: String,
    balance: Double
) extends Ordered[Account] {
  //lexicographical ordering
  def compare(that: Account): Int = this.user.compareTo(that.user)

}

object Util {

  def bytesToHex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString
  def bytesToHex(bytes: Seq[Byte]): String = bytesToHex(bytes.toArray)
  def hexToBytes(hex: String): Array[Byte] = hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  private lazy val md = java.security.MessageDigest.getInstance("SHA-256")

  def sha256Hex(msg: String): String = {
    bytesToHex(sha256(msg.getBytes))
  }
  
  def sha256(msg: Array[Byte]): Array[Byte] = {
    md.update(msg)
    md.digest
  }
  
  def splitUserHashByByte(sha256Hash: String, size: Int): Seq[String] = {
    val groupSize = 32 / size + 32 % size
    hexToBytes(sha256Hash)
      .iterator
      .sliding(groupSize, groupSize)
      .withPadding(0.toByte)
      .map(bytesToHex)
      .toSeq
  }
  
  sealed trait SplitStrategy {
    def split(acc:Account): Seq[MerkleTree.Node]
  }
  case class SplitBySize(size: Int) extends SplitStrategy {
    override def split(acc: Account) = {
      splitUserHashByByte(Node.mkLeafId(acc), size).map { partialHash =>
        Node.mkLeaf(Account(partialHash, acc.balance / size))
      }
    }
  }
  case class SplitByThreshold(balanceThreshold: Double) extends SplitStrategy {
    //Split the account according to it's balnce and the given threshold
    override def split(acc: Account) = ???
  }
}
