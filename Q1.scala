object Q1 {

  def caesarEncrypt(plaintext: String, shift: Int): String = {

    val encryptedChars = plaintext.map { char =>
      if (char.isLetter) {
        val shiftOffset = if (char.isUpper) 'A' else 'a'
        val encryptedChar = ((char - shiftOffset + shift) % 26 + shiftOffset).toChar
        encryptedChar

      } else
      {
        char
      }
    }

    encryptedChars.mkString
  }

  def caesarDecrypt(ciphertext: String, shift: Int): String = {

    caesarEncrypt(ciphertext, -shift)
  }

  def cipher(text: String, shift: Int, operation: String): String = {
    operation match {
      case "encrypt" => caesarEncrypt(text, shift)
      case "decrypt" => caesarDecrypt(text, shift)
      case _ => throw new IllegalArgumentException("Invalid operation. Use 'encrypt' or 'decrypt'.")
    }
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Julius Caesar"
    val shiftAmount = 2

    // Encrypt the plaintext
    val encryptedText = cipher(plaintext, shiftAmount, "encrypt")
    println("Encrypted:"+encryptedText)

    // Decrypt the ciphertext
    val decryptedText =cipher(encryptedText, shiftAmount, "decrypt")
    println("Decrypted:"+decryptedText)
  }


}
