public class NoTriple {
    public static void main(String[] args) {
        int[] nums = {1, 1, 2, 2, 2, 1};
        boolean answer = noTriples(nums);
        System.out.println(answer);
    }

    public static boolean noTriples(int[] nums) {
        if (nums.length < 3) {
            return true;
        } else {
            for (int i = 0; i < nums.length-2; i++) {
                if (nums[i] == nums[i+1] && nums[i+1] == nums[i+2]) {
                    return false;
                }
            }
            return true;
        }
    }
}
