public class Multiplier {
    public static void main (String[] args) {
        int a = Integer.parseInt(args[0]);
        int b = Integer.parseInt(args[1]);
        int c = Integer.parseInt(args[2]);
        int d = a * b * c;
        System.out.print(a + " * " + b + " * " + c + " = " + d);
    }
}
