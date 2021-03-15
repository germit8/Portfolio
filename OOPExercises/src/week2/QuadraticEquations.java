public class QuadraticEquations {
    public static double discriminant(double a, double b, double c) {
        return b*b - 4*a*c;
    }

    public static double plusRoot(double a, double b, double disc) {
        return (-b + Math.sqrt(disc)) / (2*a);
    }

    public static double minusRoot(double a, double b, double disc) {
        return (-b - Math.sqrt(disc)) / (2*a);
    }

    public static void main(String[] args) {
        double a = Double.parseDouble(args[0]);
        double b = Double.parseDouble(args[1]);
        double c = Double.parseDouble(args[2]);

        double disc = discriminant(a, b, c);
        if (disc >= 0) {
            System.out.println(plusRoot(a, b, disc));
            System.out.println(minusRoot(a, b, disc));
        } else {
            System.out.println("If discriminant is less than zero, then there are no solutions!");
        }
    }
}
