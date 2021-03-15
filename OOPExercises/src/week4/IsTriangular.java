import java.util.Scanner;

public class IsTriangular {
  public static void main(String[] args) {
      Scanner stdIn = new Scanner(System.in);
      double a = stdIn.nextDouble();
      double b = stdIn.nextDouble();
      double c = stdIn.nextDouble();
      
      if (isTri(a, b, c)) {
          System.out.printf("%s, %s and %s could be the lengths of a triangle\n", a, b, c);
      }
      else {
          System.out.println("Not a triangle.");
      }

      stdIn.close();
  }

  public static boolean isTri(double a, double b, double c) {
      if (a+b <= c || a+c <= b || b+c <= a) {
          return false;
      } else {
          return true;
      }
  }

}
