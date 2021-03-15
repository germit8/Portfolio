import java.util.Arrays;

public class ArrayOps {

    public static void main(String[] args) {
        double[] data = {1, 2, 3, 4, 5};
        double[] data1 = {6, 7, 8, 9, 10};

        // System.out.println(findMax(data));
        // System.out.println(Arrays.toString(normalise(data)));
        // normaliseInPlace(data);
        // System.out.println(Arrays.toString(reverse(data)));
        swap(data, data1);
        reverseInPlace(data);

    }

    public static double findMax(double[] data) {
        double max = data[0];
        for (double num : data) {
            if (num > max) {
                max = num;
            }
        }
        return max;
    }

    public static double[] normalise(double[] data) {
        double[] normalisedArr = new double[data.length];
        double sum = 0;

        for (double num : data) {
            sum += num;
        }
        for (int i = 0; i < data.length; i++) {
            normalisedArr[i] = data[i] / sum;
        }
        return normalisedArr;
    }

    public static void normaliseInPlace(double data[]) {
        double sum = 0;

        for (double num : data) {
            sum += num;
        }
        for (int i = 0; i < data.length; i++) {
            data[i] /= sum;
        }
        System.out.println(Arrays.toString(data));
    }

    public static double[] reverse(double[] data) {
        double[] reversedArr = new double[data.length];
        int i = 0;
        int j = data.length-1;

        while (i < data.length && j >= 0) {
            reversedArr[i] = data[j];
            i++;
            j--;
        }
        return reversedArr;
    }

    public static void reverseInPlace(double[] data) {
        int i = 0;
        int j = data.length-1;
        double tmp;

        while (i < (data.length-1) / 2 && j >= (data.length-1) / 2) {
            tmp = data[i];
            data[i] = data[j];
            data[j] = tmp;
            i++;
            j--;
        }
        System.out.println(Arrays.toString(data));
    }

    public static void swap(double[] data1, double[] data2) {
        double tmp;

        for (int i = 0; i < data1.length; i++) {
            tmp = data1[i];
            data1[i] = data2[i];
            data2[i] = tmp;
        }
        System.out.println(Arrays.toString(data1));
        System.out.println(Arrays.toString(data2));
    }

}