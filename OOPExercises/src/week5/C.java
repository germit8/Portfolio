public class C {
    public static void main(String y[]) {
        int x = Integer.parseInt(y[0]);
        int z = Integer.parseInt(y[1]);
        if (x <= z) {
            for (int nnnnnnnn = x; nnnnnnnn <= z; nnnnnnnn++) {
                if (nnnnnnnn % 2 == 0) {
                    System.out.print(nnnnnnnn + " ");
                }
            }
        } else {
            System.out.println(x + " cannot be greater than " + z);
        }
    }
}
