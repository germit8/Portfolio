public class IntegerDistance {
    public static void main(String[] args) {
        int value1 = Integer.parseInt(args[0]);
        int value2 = Integer.parseInt(args[1]);
        int distance = value1 - value2;
        if (distance < 0) {
            System.out.println(Math.abs(distance));
        } else {
            System.out.println(distance);
        }
    }
}
