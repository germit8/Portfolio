public class SafeDivider {
    public static void main(String[] args) {
        double numerater = Double.parseDouble(args[0]);
        double denominator = Double.parseDouble(args[1]);

        if (denominator == 0) {
            System.out.println("You can not divide by zero!");
        } else {
            System.out.println(numerater / denominator);
        }

    }
}
