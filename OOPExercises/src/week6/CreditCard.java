import java.util.Calendar;

public class CreditCard {
    
    private int expiryMonth;
    private int expiryYear;
    private String firstName;
    private String lastName;
    private String ccNumber;

    public CreditCard(int month, int year, String name1, String name2, String ccNum) {
        expiryMonth = month;
        expiryYear = year;
        firstName = name1;
        lastName = name2;
        ccNumber = ccNum;
    }

    public String formatExpiryDate() {
        String cutYear = "" + expiryYear;
        char first = cutYear.charAt(2);
        char second = cutYear.charAt(3);
        return "" + expiryMonth + "/" + first + second;
    }

    public String formatFullName() {
        return firstName + " " + lastName;
    }

    public String formatCCNumber() {
        return ccNumber.substring(0, 4) + " " + ccNumber.substring(4, 8) + " " + ccNumber.substring(8, 12) + " " + ccNumber.substring(12);
    }

    public boolean isValid() {
        Calendar now = Calendar.getInstance();
        if (now.get(Calendar.YEAR) <= expiryYear) {
            if (now.get(Calendar.MONTH) <= expiryMonth - 1) {
                return true;
            }
        }
        return false;
    }

    public void printString() {
        System.out.println("Number: " + formatCCNumber());
        System.out.println("Expiration date: " + formatExpiryDate());
        System.out.println("Account holder: " + formatFullName());
        System.out.println("Is valid: " + isValid());
    }

    public static void main(String[] args) {
        CreditCard myCard = new CreditCard(10, 2023, "Kryštof", "Bezděk", "1234567890675436");
        myCard.printString();
    }
}
