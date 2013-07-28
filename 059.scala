/*
Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard
Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value,
taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
The user would keep the encrypted message and the encryption key in different locations, and without both "halves",
it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case characters. Using cipher1.txt,
a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words,
decrypt the message and find the sum of the ASCII values in the original text.
*/

def decipher(key: String, text: String): String = text.zipWithIndex.map{ case (c, i) => (c ^ key(i % key.size)).toChar }.mkString
def sumASCII(text: String): Long = text.toList.map(_.toInt).sum
val document = scala.io.Source.fromFile("cipher1.txt").mkString.replaceAll("\n", "").split(",").map(_.toInt.toChar).mkString
val possibleKeys = for (c1 <- (97 to 122); c2 <- (97 to 122); c3 <- (97 to 122)) yield List(c1.toChar, c2.toChar, c3.toChar).mkString
val decipherings = possibleKeys.map(key => key -> decipher(key, document))
val finalKey = decipherings.filter{ case (key, text) => text.toLowerCase.contains(" the ") }.head._1
val A59 = sumASCII(decipher(finalKey, document))
