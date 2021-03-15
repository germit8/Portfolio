public class ArrayFront {
    public static void main(String[] args) {
        int[] numbers = new int[args.length];
        for (int i = 0; i < args.length; i++) {
            numbers[i] = Integer.parseInt(args[i]);
        }

        System.out.println(arrayFront(numbers));
    }

    public static boolean arrayFront(int[] nums) {
        if (nums.length >= 4) {
            for (int i = 0; i < 4; i++) {
                if (nums[i] == 9) {
                    return true;
                }
            }
        }
        return false;
    }
}
