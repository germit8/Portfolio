public class Tokenizer {

    private String[] myTokens;

    public Tokenizer() {

    }

    public Tokenizer(String fname) {
        tokensFromFile(fname);
    }

    public void tokensFromFile(String fname) {
        In reader = new In(fname);
        String readStr = reader.readAll();
        tokenize(readStr);
    }

    public void tokenize(String str) {
        myTokens = str.split("\\W+");
    }

    public String[] getTokens() {
        return myTokens;
    }

    public int getNumberTokens() {
        return myTokens.length;
    }

    public static void main(String[] args) {
        Tokenizer test = new Tokenizer();
        test.tokensFromFile("test.txt");
        String[] myTokens = test.getTokens();
        for (int i = 0; i < test.getNumberTokens(); i++) {
            System.out.println(myTokens[i]);
        }
    }
}
