public class Dalek1 {

    private double batteryCharge = 5.0; // instance variable

    public void batteryReCharge(double c) {
        this.batteryCharge += c;
        System.out.println("Battery charge is: " + batteryCharge);
    }

    public void move(int distance) {
        String moveString = "Moving... ";
        int moveDist = 1;
        while (batteryCharge >= 0.5 && moveDist <= distance) {
            moveString += "[" + moveDist + "] ";
            moveDist++;
            batteryCharge -= 0.5;
        }
        if (batteryCharge == 0) {
            moveString += "Out of battery!";
        }
        System.out.println(moveString);
    }

    public static void main(String[] args) {
        Dalek1 d = new Dalek1(); // start off with a well-charged battery
        d.move(11);              // move around and use up the charge
        d.batteryReCharge(2.5);  // get a new charge
        d.batteryReCharge(0.5);  // add a bit more
        d.move(5);               // move some more
    }

}