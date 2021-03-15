public class Cipher {

    public static String alphabet = "abcdefghijklmnopqrstuvwxyz";

    public static int charToInt(char l) {
        return alphabet.indexOf(l);
    }

    public static char intToChar(int i) {
      return alphabet.charAt(i);
    }

    public static String encipher(String original, String onetimepad) {
      String plaintext = original.toLowerCase(); //AHOJDA
      String key = onetimepad.toLowerCase();
      int[] charSums = new int[plaintext.length()];
      String processed = "";

      for (int i = 0; i < plaintext.length(); i++) {
            if (Character.isLetter(plaintext.charAt(i))) {
                charSums[i] = charToInt(plaintext.charAt(i)) + charToInt(key.charAt(i));
            } else {
                charSums[i] = 51;
            }
      }
      for (int j = 0; j < plaintext.length(); j++) {
            if (charSums[j] == 51) {
                processed += " ";
            } else {
                processed += intToChar(charSums[j] % 26);
            }
      }
      return processed;
    }

    public static String decipher(String encipheredText, String onetimepad) {
        String ciphertext = encipheredText.toLowerCase(); //AHOJDA
        String key = onetimepad.toLowerCase();
        int[] charSums = new int[ciphertext.length()];
        String processed = "";

        for (int i = 0; i < ciphertext.length(); i++) {
            if (Character.isLetter(ciphertext.charAt(i))) {
                charSums[i] = charToInt(ciphertext.charAt(i)) - charToInt(key.charAt(i));
            } else {
                charSums[i] = 51;
            }
        }
        for (int j = 0; j < ciphertext.length(); j++) {
            if (charSums[j] == 51) {
                processed += " ";
            } else if (charSums[j] < 0) {
                processed += intToChar((charSums[j]+26) % 26);
            } else {
                processed += intToChar(charSums[j] % 26);
            }
        }
        return processed;
    }

    public static void main(String[] args) {
      String ciphertext = encipher("IS THIS SECURE", "KEEPMEVERYVERYSAFE");
      String deciphertext = decipher(ciphertext, "KEEPMEVERYVERYSAFE").toUpperCase();
      System.out.println(ciphertext);
      System.out.println(deciphertext);
    }

}