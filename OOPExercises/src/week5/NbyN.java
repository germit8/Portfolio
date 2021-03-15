public class NbyN {
    
    public static void main(String[] args) {
        int n = 10;
        int[][] matrix = nbyn(n);
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                System.out.print(matrix[r][c] + " ");
            }
            System.out.print("\n");
        }
    }

    public static int[][] nbyn(int n) {
        int[][] newMatrix = new int[n][n];
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; c++) {
                if (r == c) {
                    newMatrix[r][c] = r;
                } else {
                    newMatrix[r][c] = 0;
                }
            }
        }
        return newMatrix;
    }
}
