public class LargestDouble {
    public static void main(String[] args) {
        double val1 = Double.parseDouble(args[0]);
        double val2 = Double.parseDouble(args[1]);
        double largeDouble = Math.max(val1, val2);
        System.out.println(largeDouble);
    }
}
