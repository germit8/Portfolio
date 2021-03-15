import java.util.Random;

public class Dalek2 {
    
    private String[] sayingRepertoaire = {"No utterances installed!"};
    
    public void setSayings(String[] newSayings) {
        this.sayingRepertoaire = newSayings;
    }

    public String[] getSayings() {
        return sayingRepertoaire;
    }

    public void speak(int sayingIndex) {
        System.out.println(sayingRepertoaire[sayingIndex]);
    }
    
    public static void main(String[] args) {

        int randomIndex;
        Random generator = new Random();
        Dalek2 d1 = new Dalek2();
        Dalek2 d2 = new Dalek2();

        String[] u1 = { "Exterminate, Exterminate!", "I obey!",
                "Exterminate, annihilate, DESTROY!", "You cannot escape.",
                "Daleks do not feel fear.", "The Daleks must survive!" };
        String[] u2 = { "I obey!" };

        d1.setSayings(u1);
        d2.setSayings(u2);
    
        System.out.println("\nDalek d1 says: ");
        for (int i = 0; i < 10; i++) {
            randomIndex = generator.nextInt(d1.getSayings().length);
            d1.speak(randomIndex);
        }
    
        System.out.println("\nDalek d2 says: ");
        for (int i = 0; i < 10; i++) {
            randomIndex = generator.nextInt(d2.getSayings().length);
            d2.speak(randomIndex);
        }
    }
}
