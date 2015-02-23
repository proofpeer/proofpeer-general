package proofpeer.general

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringUtilsTests extends Properties("StringUtils") {

  property("UTF8Conversions") = forAll { (s: String) =>
    StringUtils.fromUtf8Bytes(StringUtils.utf8Bytes(s)) == s
  }

}