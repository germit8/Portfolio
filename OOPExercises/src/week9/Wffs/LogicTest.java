public class LogicTest {
    
    public static void main(String[] args) {
        PropVar p = new PropVar("P");
        PropVar q = new PropVar("Q");
        AndWff andWff = new AndWff(p, q);
        System.out.println(andWff);
    }
}
