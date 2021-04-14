package animals;

public class Seal extends Animal {
    
    public Seal(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal.toString().equals("Shark")) return false;
        return true;
    }

    @Override
    public String toString() {
        return "Seal";
    }
}
