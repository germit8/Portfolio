import java.util.Scanner;

public class NMax {
    public static void main(String[] args) {
        Scanner stdIn = new Scanner(System.in);
        double a = stdIn.nextDouble();
        double b = stdIn.nextDouble();
        double c = stdIn.nextDouble();

        System.out.println("The highest number is: " + max(a, b, c));
    }

    public static int max(int a, int b, int c) {
        int[] nums = {a, b, c};
        int tmp = nums[0];
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] > tmp) {
                tmp = nums[i];
            }
        }
        return tmp;
    }

    public static double max(double a, double b, double c) {
        double[] nums = {a, b, c};
        double tmp = nums[0];
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] > tmp) {
                tmp = nums[i];
            }
        }
        return tmp;
    }
}
