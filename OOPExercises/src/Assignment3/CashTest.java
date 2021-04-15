import dataStructures.*;
import zoo.*;

public class CashTest {
    public static void main(String[] args) {

        Zoo zoo = new Zoo();
        CashCount cash = new CashCount();
        CashCount payment = new CashCount();

        payment.setNrNotes_20pounds(1);
        payment.setNrNotes_10pounds(0);
        payment.setNrNotes_5pounds(0);
        payment.setNrCoins_2pounds(1);
        payment.setNrCoins_1pound(0);
        payment.setNrCoins_50p(0);
        payment.setNrCoins_20p(0);
        payment.setNrCoins_10p(0);
        
        cash.setNrNotes_20pounds(6);
        cash.setNrNotes_10pounds(5);
        cash.setNrNotes_5pounds(10);
        cash.setNrCoins_2pounds(10);
        cash.setNrCoins_1pound(10);
        cash.setNrCoins_50p(20);
        cash.setNrCoins_20p(1);
        cash.setNrCoins_10p(0);

        zoo.setCashSupply(cash);
        zoo.setEntranceFee(17, 80);

        System.out.println("Payment: " + payment.getCashSum());
        System.out.println("Entrance fee: " + zoo.getEntranceFee());
        System.out.println("Change: " + zoo.payEntranceFee(payment).toString());

        System.out.println("Final zoo cash supply: " + zoo.getCashSupply());
    }
}
