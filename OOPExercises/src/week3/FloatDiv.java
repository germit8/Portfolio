public class FloatDiv {
    public static void main(String[] args) {
        int age = Integer.parseInt(args[0]);
        int exposure = Integer.parseInt(args[1]);

        double dose = ((exposure * 250) / age);

        System.out.format("Patient Age: %d%n", age);
        System.out.format("Days since bitten: %d%n", exposure);
        System.out.format("Dosage of antidote: %.3fmg%n", dose);
    }
}