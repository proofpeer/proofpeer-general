package proofpeer.general.algorithms

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object HexStringTests extends Properties("HexString") {

  property("Inverses") = forAll { (a: List[Byte]) =>
    val hex = HexString.fromBytes(a : _*)
    val bOpt = HexString.toBytes(hex)
    bOpt.isDefined && bOpt.get.toList == a
  }

}