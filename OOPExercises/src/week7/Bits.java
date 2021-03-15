public class Bits {
    
    private int x, y;
    public static void main(String[] args) {
        Bits bit = new Bits();
        bit.m2();
        bit.m1();
    }

    public void m2() {
        int z = 1;
        x = y = z;
    }

    public void m1() {
        x >>>= 1;
        System.out.println(x);
        System.out.println(y);
    }
}
