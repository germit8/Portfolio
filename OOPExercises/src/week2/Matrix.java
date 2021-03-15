public class Matrix {
    public static void main(String[] args) {

        final double[][] MATRIXA = {
            {1.2, 2.1, 3.5},
            {9.2, 0.1, 3.8},
            {1.9, 4.1, 6.5}
        };

        final double[][] MATRIXB = {
            {1.2, 2.1, 3.5},
            {9.2, 0.1, 3.8},
            {1.9, 4.1, 6.5}
        };

        final int MATRIXAN = MATRIXA.length;
        final int MATRIXBN = MATRIXB.length;

        double[][] matrixC = new double[MATRIXAN][MATRIXBN];
        for (int i = 0; i < MATRIXAN; i++) {
            for (int j = 0; j < MATRIXBN; j++) {
                matrixC[i][j] = MATRIXA[i][j] + MATRIXB[i][j];
                System.out.println(matrixC[i][j]);
            }
        }
    }
}
