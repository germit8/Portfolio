public class X {
    private int x, y;

    public void m1(boolean b) {
        x <<= 1;
        if (b)
            x++;
        y++;
    }

    public Boolean m2() {
        if (m6())
            return null;
        return x % 2 != 0;
    }

    public Boolean m3() {
        Boolean z = m2();
        if (z != null) {
            x >>>= 1;
            y--;
        }
        return z;
    }

    public void m4() {
        x = y = 0;
    }

    public int m5() {
        return y;
    }

    public boolean m6() {
        return y == 0;
    }
}