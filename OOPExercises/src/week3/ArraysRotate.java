import java.util.Arrays;

public class ArraysRotate {
    public static void main(String[] args) {
        int[] nums = createNumList(args);
        int[] copy = rotatedNumList(nums);
        
        for (int i = 0; i < copy.length; i++) {
            System.out.print(copy[i] + " ");
        }
    }

    public static int[] createNumList(String[] numbers) {
        int[] lst = new int[numbers.length];
        for (int i = 0; i < numbers.length; i++) {
            lst[i] = Integer.parseInt(numbers[i]);
        }
        return lst;
    }

    public static int[] rotatedNumList(int[] numz) {
        int[] copy = new int[numz.length];
        if (numz.length == 1) {
            return numz;
        } else {
            int tmp1 = numz[0];
            int tmp2 = numz[1];

            for (int i = 0; i < numz.length; i++) {
                copy[i] = tmp2;
                if (i < numz.length - 2) {
                    tmp2 = numz[i+2];
                } else {
                    copy[numz.length -1] = tmp1;
                }
            }
            return copy;
        }
    }
}
