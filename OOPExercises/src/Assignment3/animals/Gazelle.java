package animals;

public class Gazelle extends Animal {
    
    public Gazelle(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal.toString().equals("Lion")) return false;
        return true;
    }

    @Override
    public String toString() {
        return "Gazelle";
    }
}
