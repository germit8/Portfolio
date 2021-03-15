public class DateFashion {
    public static void main(String[] args) {
        int me = Integer.parseInt(args[0]);
        int date = Integer.parseInt(args[1]);
        System.out.println(dateFashion(me, date));
    }

    public static int dateFashion(int me, int date) {
        // if (me < 0 || me > 10 || date < 0 || date > 10) {
        //     System.out.println("Inputs should be between 0 and 10");
        // }
        if (me <= 2 || date <= 2) {
            return 0;
        } else if (me >= 8 || date >= 8) {
            return 2;
        } else {
            return 1;
        }
    }
}
