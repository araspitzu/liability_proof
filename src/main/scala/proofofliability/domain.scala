package proofofliability

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
  def hexToBytes(hex: String): Array[Byte] = hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)

  lazy val md = java.security.MessageDigest.getInstance("SHA-256")

  def sha256(msg: String): String = {
    md.update(msg.getBytes)
    bytesToHex(md.digest)
  }
}

//sealed trait SplitStrategy
//case object