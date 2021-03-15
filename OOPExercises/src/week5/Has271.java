public class Has271 {
    
    public static void main(String[] args) {
        int[] nums = {4, 9, 3};
        boolean answer = has271(nums);
        System.out.println(answer);
    }

    public static boolean has271(int[] nums) {
        if (nums.length < 3) {
            return false;
        } else {
            for (int i = 0; i < nums.length-2; i++) {
                if (nums[i+1] == (nums[i] + 5) && nums[i+2] == (nums[i] - 1)) {
                    return true;
                }
            }
            return false;
        }
    }
}
