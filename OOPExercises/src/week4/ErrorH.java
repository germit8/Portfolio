public class ErrorH {

    public static void main(String[] args) {
        String[] names = {"Peter", "Sarah", "Ivan"};
        int[] ages = {23, -5, 35};

        spam(names, ages);
    }

    public static boolean birthdayGreetings(String name, int age) {
        boolean success;
      
        if (age > 0) {
          System.out.println("All the best to your " + age + ". birthday " + name);
          success = true;
        } else {
          System.err.println("ERROR: The given age must be larger zero but is: " + age);
          success = false;
        }
      
        return success;
      }

    public static void spam(String[] names, int[] ages) {
        int defaultAge = 20;

        for (int i = 0; i < names.length; i++) {
            if (birthdayGreetings(names[i], ages[i]) == false) {
                birthdayGreetings(names[i], 20);
            }
        }
    }
      
}
