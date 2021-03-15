public class Vector3D {
    
    private double x;
    private double y;
    private double z;

    public Vector3D(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public double getRadius() {
        return Math.sqrt(x*x + y*y + z*z);
    }

    public double getTheta() {
        return Math.acos(z / getRadius());
    }

    public double getPhi() {
        return Math.atan(y / x);
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public double getZ() {
        return z;
    }

    public static Vector3D add(Vector3D lhs, Vector3D rhs) {
        double newX = lhs.getX() + rhs.getX();
        double newY = lhs.getY() + rhs.getY();
        double newZ = lhs.getZ() + rhs.getZ();

        Vector3D newVector = new Vector3D(newX, newY, newZ);

        return newVector;
    }

    public static Vector3D subtract(Vector3D lhs, Vector3D rhs) {
        double newX = lhs.getX() - rhs.getX();
        double newY = lhs.getY() - rhs.getY();
        double newZ = lhs.getZ() - rhs.getZ();

        Vector3D newVector = new Vector3D(newX, newY, newZ);

        return newVector;
    }

    public static Vector3D scale(Vector3D vector, double scaleFactor) {
        Vector3D newVector = new Vector3D(vector.getX() * scaleFactor,
                                          vector.getY() * scaleFactor,
                                          vector.getZ() * scaleFactor);
        return newVector;
    }

    public static void main(String[] args) {
        Vector3D vector1 = new Vector3D(3, 5, 8);
        Vector3D vector2 = new Vector3D(-1, 2, 6);

        Vector3D addedVector = Vector3D.add(vector1, vector2);
        Vector3D subtractVector = Vector3D.subtract(vector1, vector2);
        Vector3D scaleVector = Vector3D.scale(vector1, 3);

        System.out.println(vector1.getX());
        System.out.println(vector2.getX());
        System.out.println(addedVector.getX());
        System.out.println(subtractVector.getX());
        System.out.println(scaleVector.getX());
    }
}
