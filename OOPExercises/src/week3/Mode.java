import java.util.Arrays;

public class Mode {
    public static void main(String[] args) {
        int[] dataset = createDataSetFromArgs(args);
        int[] count = new int[10];

        for (int i = 0; i < dataset.length; i++) {
            count[dataset[i]]++;
        }
        
        int num = 0;
        for (int i = 0; i < count.length; i++) {
            if (count[i] > num) {
                num = count[i];
            }
        }

        int mode = 0;
        for (int i = 0; i < count.length; i++) {
            if (num == count[i]) {
                mode = i;
            }
        }

        for (int i = 0; i < count.length; i++) {
            System.out.format("[%ds: %d] " + ".".repeat(count[i]) + "%n", i, count[i]);
        }
        System.out.println("Mode: " + mode);
        
        
        
        // System.out.println(num);
        // System.out.println(Arrays.toString(count)); 
    }

    public static int[] createDataSetFromArgs(String[] ints) {
        int[] tmp = new int[ints.length];
        for (int i = 0; i < ints.length; i++) {
            tmp[i] = Integer.parseInt(ints[i]);
        }
        return tmp;
    }
}
