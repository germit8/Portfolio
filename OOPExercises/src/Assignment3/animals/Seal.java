package animals;

public class Seal extends Animal {
    
    public Seal(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal == null) throw new NullPointerException();
        else if (animal.toString().equals("Shark")) return false;
        else return true;
    }

    @Override
    public String toString() {
        return "Seal";
    }
}
