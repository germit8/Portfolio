import java.util.Objects;

public class Cars implements Comparable<Cars> {
    
    private int speed;
    private int maxSpeed = 150;

    public Cars(int maxSpeed) {
        if (maxSpeed <= 0) throw new IllegalArgumentException("Max speed must be bigger than 0");
        else this.maxSpeed = maxSpeed;
    }

    public int getSpeed() {
        return speed;
    }

    public void gas(int delta) {
        int deltaSpeed = speed + delta;
        if (deltaSpeed > maxSpeed) speed = maxSpeed;
        else if (deltaSpeed < 0) speed = 0;
        else speed = deltaSpeed;
    }

    @Override
    public int compareTo(Cars that) {
        Objects.requireNonNull(that, "The given Car instance must not be null.");
        if (this.speed > that.speed) return 1;
        else if (this.speed < that.speed) return -1;
        else return 0;
    }

    public static void main(String[] args) {

        Cars A = new Cars(120);
        Cars B = new Cars(150);

        A.gas(50);
        B.gas(60);
        System.out.println(A.compareTo(B));
        System.out.println(B.compareTo(A));

    }
}
