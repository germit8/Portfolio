import java.util.Arrays;

public class MeanVariance {
    public static void main(String[] args) {
        double[] dataSet = createDataSetFromArgs(args);
        double lengthOfData = dataSet.length;

        double mean = sumOfData(dataSet) / lengthOfData;
        double variance = variance(dataSet, mean) / lengthOfData;

        System.out.println(mean);
        System.out.println(variance);
    }

    public static double[] createDataSetFromArgs(String[] doubles) {
        double[] tmp = new double[doubles.length];
        for (int i = 0; i < doubles.length; i++) {
            tmp[i] = Double.parseDouble(doubles[i]);
        }
        return tmp;
    }

    public static double sumOfData(double[] data) {
        double sum = 0.0;
        for (int i = 0; i < data.length; i++) {
            sum += data[i];
        }
        return sum;
    }

    public static double variance(double[] data, double mean) {
        double squaredSum = 0.0;
        for (int i = 0; i < data.length; i++) {
            squaredSum += Math.pow(data[i] - mean, 2.0);
        }
        return squaredSum;
    }
}
