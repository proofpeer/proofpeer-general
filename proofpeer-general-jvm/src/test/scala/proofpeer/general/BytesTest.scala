package proofpeer.general

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen


object BytesTests extends Properties("Bytes") {

  property("Boolean") = forAll { (x : Boolean) =>
    Bytes.asBoolean(Bytes.fromBoolean(x)) == x
  }

  property("Byte") = forAll { (x : Byte) =>
    Bytes.asByte(Bytes(x)) == x
  }

  property("Short") = forAll { (x : Short) =>
    Bytes.asShort(Bytes.fromShort(x)) == x
  }

  property("Char") = forAll { (x : Char) =>
    Bytes.asChar(Bytes.fromChar(x)) == x
  }

  property("Int") = forAll { (x : Int) =>
    Bytes.asInt(Bytes.fromInt(x)) == x
  }

  property("Long") = forAll { (x : Long) =>
    Bytes.asLong(Bytes.fromLong(x)) == x
  }

  property("String") = forAll { (x : String) =>
    (Bytes.fromString(x)).asString == x
  }

  val EncodingNat = Gen.choose(0, Long.MaxValue)
  val SpecialEncodingNat1 = Gen.choose(0L, 247L)
  val SpecialEncodingNat2 = Gen.choose(0L, 1000L)

  val testNat : Long => Boolean = (x : Long) => {
    val bytes = Bytes.encodeNat(x)
    val (y, len) = Bytes.decodeNat(bytes, 0)
    y == x && len == bytes.length
  }

  property("EncodeDecodeNat1") = forAll(SpecialEncodingNat1) { x => testNat(x) }
  property("EncodeDecodeNat2") = forAll(SpecialEncodingNat2) { x => testNat(x) }
  property("EncodeDecodeNat3") = forAll(EncodingNat) { x => testNat(x) }

  val test : Any => Boolean = (x : Any) => {
    val bytes = Bytes.encode(x)
    val (y, len) = Bytes.decode(bytes, 0)
    y == x && len == bytes.length
  }

  property("EncodeDecodeByte") = forAll { (x:Byte) => test(x) }
  property("EncodeDecodeShort") = forAll { (x:Short) => test(x) }
  property("EncodeDecodeInt") = forAll { (x:Int) => test(x) }  
  property("EncodeDecodeLong") = forAll { (x:Long) => test(x) }
  property("EncodeDecodeString") = forAll { (x:String) => test(x) }
  property("EncodeDecodeBytes") = forAll { (x:List[Byte]) => test(Bytes(x : _*)) }
  property("EncodeDecodeVector") = forAll { (x:List[List[Int]]) => test(x) }

  property("EncodeCompact1") = Bytes.encode(Vector(1L, -5L, 11L)).length == 4
  property("EncodeCompact2") = Bytes.encode(Vector(Long.MaxValue, Long.MinValue, Long.MaxValue)).length == 28

  property("UCS2") = forAll { (x : List[Byte]) => Bytes.decodeFromUCS2(Bytes.encodeToUCS2(x)) == x }

}