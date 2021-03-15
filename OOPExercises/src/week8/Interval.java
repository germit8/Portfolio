public class Interval {
    
    private double left;
    private double right;

    public Interval(double left, double right) {
        this.left = left;
        this.right = right;
    }

    public double getLeft() {
        return left;
    }

    public double getRight() {
        return right;
    }

    public boolean doesContain(double x) {
        if (x < right && x > left && !(isEmpty())) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isEmpty() {
        if (right < left) {
            return true;
        } else {
            return false;
        }
    }

    // [1..4] U [2..5] == [2..4]
    public boolean intersects(Interval b) {
        if (isEmpty() || b.isEmpty()) {
            return false;
        }
        
        return doesContain(b.left) || doesContain(b.right) || b.doesContain(left) || b.doesContain(right);
    }

    public String toString() {
        if (isEmpty()) {
            return "Interval: (EMPTY)";
        }
        return "Interval: [" + left + ", " + right + "]";
    }

    public static void main(String[] args) {
        Interval in1 = new Interval(1.0, 4.0);
        Interval in2 = new Interval(2.0, 5.0);
        Interval in3 = new Interval(5.0, 0.0);

        System.out.println(in1.doesContain(3.0));
        System.out.println(in1.doesContain(6.0));
        System.out.println(in1.isEmpty());
        System.out.println(in3.isEmpty());
        System.out.println(in1.intersects(in2));
        System.out.println(in1.intersects(in3));
        System.out.println(in1.toString());
        System.out.println(in2.toString());
        System.out.println(in3.toString());
    }

}
