package dataStructures;

import java.util.HashMap;
import java.util.Map;

public class CashCount implements ICashCount {
    
    private HashMap<Integer, Integer> cashSupply;
    private int cashSum;

    public CashCount() {
        this.cashSupply = new HashMap<>(Map.of(2000, 0, 
                                               1000, 0,
                                               500, 0,
                                               200, 0,
                                               100, 0,
                                               50, 0,
                                               20, 0,
                                               10, 0));
    }

    public CashCount(ICashCount cashCount) {
        this.cashSupply = new HashMap<>(Map.of(2000, cashCount.getNrNotes_20pounds(), 
                                               1000, cashCount.getNrNotes_10pounds(),
                                               500, cashCount.getNrNotes_5pounds(),
                                               200, cashCount.getNrCoins_2pounds(),
                                               100, cashCount.getNrCoins_1pound(),
                                               50, cashCount.getNrCoins_50p(),
                                               20, cashCount.getNrCoins_20p(),
                                               10, cashCount.getNrCoins_10p()));
    }

    public int calculateCashSum() {
        int sum = 0;
        for (Integer key : cashSupply.keySet()) {
            sum += key * cashSupply.get(key);
        }
        return sum;
    }

    public void setNrNotes_20pounds(int noteCount) {
        cashSupply.put(2000, noteCount);
    }

    public void setNrNotes_10pounds(int noteCount) {
        cashSupply.put(1000, noteCount);
    }

    public void setNrNotes_5pounds(int noteCount) {
        cashSupply.put(500, noteCount);
    }

    public void setNrCoins_2pounds(int coinCount) {
        cashSupply.put(200, coinCount);
    }

    public void setNrCoins_1pound(int coinCount) {
        cashSupply.put(100, coinCount);
    }

    public void setNrCoins_50p(int coinCount) {
        cashSupply.put(50, coinCount);
    }

    public void setNrCoins_20p(int coinCount) {
        cashSupply.put(20, coinCount);
    }

    public void setNrCoins_10p(int coinCount) {
        cashSupply.put(10, coinCount);
    }

    // --------------------------------------------------------------

    public int getNrNotes_20pounds() {
        return cashSupply.get(2000);
    }

    public int getNrNotes_10pounds() {
        return cashSupply.get(1000);
    }

    public int getNrNotes_5pounds() {
        return cashSupply.get(500);
    }

    public int getNrCoins_2pounds() {
        return cashSupply.get(200);
    }

    public int getNrCoins_1pound() {
        return cashSupply.get(100);
    }

    public int getNrCoins_50p() {
        return cashSupply.get(50);
    }

    public int getNrCoins_20p() {
        return cashSupply.get(20);
    }

    public int getNrCoins_10p() {
        return cashSupply.get(10);
    }

    public int getCashSum() {
        cashSum = calculateCashSum();
        return cashSum;
    }

    @Override
    public String toString() {
        return cashSupply.toString();
    }
}
