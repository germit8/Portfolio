import java.util.Arrays;
import java.util.HashMap;

public class WordCounter {
    
    private HashMap<Integer, Integer> myKeySet = new HashMap<Integer, Integer>();

    public WordCounter(String[] tokens) {
        wordFreqLength(tokens);
    }

    public void wordFreqLength(String[] tokens) {
        int key;

        for (String token : tokens) {
            key = token.length();
            // System.out.println(key);

            if (myKeySet.get(key) == null) {
                myKeySet.put(key, 1);
            } else {
                int newValue = myKeySet.get(key);
                myKeySet.put(key, newValue+1);
            } 
                // System.out.println(myKeySet);
        }
    }

    public HashMap<Integer, Integer> getFreqDist() {
        return myKeySet;
    }

    public int maxVal() {
        int temp = 0;
        for (Integer i : myKeySet.keySet()) {
            if (i > temp) {
                temp = i;
            }
        }
        return temp;
    }

    public double[] map2Array() {
        double[] freqArray = new double[maxVal()+1];
        double total = 0;
        System.out.println(maxVal());

        for (Integer i : myKeySet.keySet()) {
            total += myKeySet.get(i);
        }

        for (int i = 0; i < freqArray.length; i++) { // {2=2, 4=2, 5=5, 8=1}
            if (myKeySet.get(i) == null) {
                freqArray[i] = 0;
            } else {
                freqArray[i] = (myKeySet.get(i) / total) * 100;
            }
        }

        System.out.println(Arrays.toString(freqArray));
        return freqArray;
    }

    public static void main(String[] args) {
        Tokenizer tokenizer = new Tokenizer("melville-moby_dick.txt");
        String[] tokens = tokenizer.getTokens();

        WordCounter wordCounter = new WordCounter(tokens);
        System.out.println(wordCounter.getFreqDist());
        double[] points = wordCounter.map2Array();

        int n = points.length;
        StdDraw.clear();
        StdDraw.setXscale(0, n - 1);
        StdDraw.setYscale(0, 100);
        StdDraw.setPenRadius(0.5 / n);
        for (int i = 0; i < n; i++) {
            StdDraw.line(i, 0, i, points[i]);
        }
    }
}
