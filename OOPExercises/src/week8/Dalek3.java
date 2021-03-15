import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;

public class Dalek3 {
    
    private ArrayList<String> sayingRepertoaire = new ArrayList<String>(Arrays.asList("Nothing in repertoaire"));
    
    public void setSayings(ArrayList<String> newSayings) {
        this.sayingRepertoaire = newSayings;
    }

    public ArrayList<String> getSayings() {
        return sayingRepertoaire;
    }

    public void speak(int sayingIndex) {
        System.out.println(sayingRepertoaire.get(sayingIndex));
    }

    public void addSaying(String newSaying) {
        sayingRepertoaire.add(newSaying);
    }
    
    public static void main(String[] args) {

        int randomIndex;
        Random generator = new Random();
        Dalek3 d1 = new Dalek3();
        Dalek3 d2 = new Dalek3();

        ArrayList<String> u1 = new ArrayList<String>(Arrays.asList("Exterminate, Exterminate!", "I obey!",
        "Exterminate, annihilate, DESTROY!", "You cannot escape.",
        "Daleks do not feel fear.", "The Daleks must survive!" ));
        ArrayList<String> u2 = new ArrayList<String>(Arrays.asList("I obey!"));

        d1.setSayings(u1);
        d2.setSayings(u2);

        d1.addSaying("KURVA DOPÍČI");
        d2.addSaying("ZASRANÁ MRDKA");
    
        System.out.println("\nDalek d1 says: ");
        for (int i = 0; i < 10; i++) {
            randomIndex = generator.nextInt(d1.getSayings().size());
            d1.speak(randomIndex);
        }
    
        System.out.println("\nDalek d2 says: ");
        for (int i = 0; i < 10; i++) {
            randomIndex = generator.nextInt(d2.getSayings().size());
            d2.speak(randomIndex);
        }
    }
}
