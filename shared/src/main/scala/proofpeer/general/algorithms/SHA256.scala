package proofpeer.general.algorithms

import proofpeer.general.StringUtils

/** Computes SHA-256 hash values. 
  * Based on the pseudo code at http://en.wikipedia.org/wiki/SHA-2#Pseudocode
  */ 
object SHA256 {

  private val k : Array[Int] = Array(    
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)

  private class PaddedBitstream(bits : BitStream) extends BitStream {
    
    var bitsRead : Long = 0
    var pad : BitStream = null

    def read() : Int = {
      if (bits.done) padding().read() else {
        bitsRead += 1
        bits.read()
      }
    }

    def done : Boolean = {
      if (bits.done) padding().done else false
    }

    def padding() : BitStream = {
      import BitStream._
      if (pad != null) 
        pad
      else {
        val sizeInBits = BigEndian.fromLong(bitsRead)
        val len = bitsRead % 512
        val filler = 
          if (len + 65 <= 512)
            zeros(512 - 65 - len)
          else
            zeros (512 - (len + 1) + 512 - 64)
        pad = join(one, filler, sizeInBits)
        pad
      }
    }

  }

  private def rotate(x : Int, k : Int) : Int = (x >>> k) | (x << (32-k))

  def compute(bitsOfMessage : BitStream) : Array[Byte] = {
    val bits = new PaddedBitstream(bitsOfMessage)

    var h0 = 0x6a09e667
    var h1 = 0xbb67ae85
    var h2 = 0x3c6ef372
    var h3 = 0xa54ff53a
    var h4 = 0x510e527f
    var h5 = 0x9b05688c
    var h6 = 0x1f83d9ab
    var h7 = 0x5be0cd19

    var w : Array[Int] = new Array(64)

    do {

      // fill first 16 words of w with 512 bit chunk
      for (i <- 0 to 15) w(i) = BitStream.BigEndian.toInt(bits)
      
      // expand w to 64 words
      for (i <- 16 to 63) {
        val w15 = w(i - 15)
        val s0 = rotate(w15, 7) ^ rotate(w15, 18) ^ (w15 >>> 3)
        val w2 = w(i - 2)
        val s1 = rotate(w2, 17) ^ rotate(w2, 19) ^ (w2 >>> 10)
        w(i) = w(i - 16) + s0 + w(i - 7) + s1
      }
      
      // initialize working variables from current hash
      var a = h0
      var b = h1
      var c = h2
      var d = h3
      var e = h4
      var f = h5
      var g = h6
      var h = h7

      // compression main loop
      for (i <- 0 to 63) {
        val s1 = rotate(e, 6) ^ rotate(e, 11) ^ rotate(e, 25)
        val ch = (e & f) ^ ((~ e) & g)
        val temp1 = h + s1 + ch + k(i) + w(i)
        val s0 = rotate(a, 2) ^ rotate(a, 13) ^ rotate(a, 22)
        val maj = (a & b) ^ (a & c) ^ (b & c)
        val temp2 = s0 + maj

        h = g
        g = f
        f = e
        e = d + temp1
        d = c
        c = b
        b = a
        a = temp1 + temp2
      }

      // add compressed chunk to hash
      h0 += a
      h1 += b
      h2 += c
      h3 += d
      h4 += e
      h5 += f
      h6 += g
      h7 += h

    } while (!bits.done)

    import BitStream._
    import BigEndian._

    val digest = join(fromInt(h0), fromInt(h1), fromInt(h2), fromInt(h3), 
      fromInt(h4), fromInt(h5), fromInt(h6), fromInt(h7))

    toBytes(digest, 32)
  }

  def compute(bytes : Byte*) : Array[Byte] = 
    compute(BitStream.BigEndian.fromBytes(bytes : _*))

  def compute(s : String) : Array[Byte] = {
    val bytes = StringUtils.utf8Bytes(s)
    compute(bytes : _*)
  }

  def computeHex(bitsOfMessage : BitStream) : String = 
    HexString.fromBytes(compute(bitsOfMessage) : _*)

  def computeHex(bytes : Byte*) : String = 
    HexString.fromBytes(compute(bytes : _*) : _*)

  def computeHex(s : String) : String = 
    HexString.fromBytes(compute(s) : _*)

}