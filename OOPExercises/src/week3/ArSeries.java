public class ArSeries {
    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);
        int k = 1;
        int sum = 0;
        while (k <= n) {
            sum += k;
            k++;
        }
        System.out.println(sum);
    }
}
