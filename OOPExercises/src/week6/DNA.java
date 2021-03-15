import java.util.Arrays;

public class DNA {
    
    private String DNAStrand;

    public DNA(String dna) {
        DNAStrand = dna;
    }

    public boolean isValidDNA() {
        String[] dnaArray = {"A", "T", "C", "G"};
        if (DNAStrand.length() > 0) {
            for (int i = 0; i < DNAStrand.length(); i++) {
                if (!(Arrays.asList(dnaArray).contains("" + DNAStrand.charAt(i)))) {
                    System.out.println(Arrays.asList(dnaArray).contains(DNAStrand.charAt(i)));
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public String complementWC() {
        String swicthedDNA = "";
        for (int i = 0; i < DNAStrand.length(); i++) {
            switch (DNAStrand.charAt(i)) {
                case 'A':
                    swicthedDNA += "T";
                    break;
                case 'T':
                    swicthedDNA += "A";
                    break;
                case 'C':
                    swicthedDNA += "G";
                    break;
                case 'G':
                    swicthedDNA += "C";
                    break;
            }
        }
        return swicthedDNA;
    }

    public String palindromeWC() {
        String reverseDNA = complementWC();
        String finalString = "";

        for (int i = reverseDNA.length() - 1; i >= 0; i--) {
            finalString += reverseDNA.charAt(i);
        }

        return finalString;
    }

    public boolean containsSequence(String seq) {
        int seqLength = seq.length();

        for (int i = 0; i <= DNAStrand.length() - seqLength; i++) {
            if (DNAStrand.substring(i, i + seqLength).equals(seq)) {
                return true;
            }
        }
        return false;
    }

    public void printStrings() {
        System.out.println("Original DNA sequence: " + DNAStrand);
        System.out.println("Complement: " + complementWC());
        System.out.println("WC Palindrome: " + palindromeWC());
        System.out.println("Has sequence: AG: " + containsSequence("AG"));
    }

    public static void main(String[] args) {
        DNA myDNA = new DNA("AGTC");
        if (myDNA.isValidDNA()) {
            System.out.println("DNA is valid.");
            myDNA.printStrings();
        } else {
            System.out.println("DNA is not valid.");
        }
    }
}
