package animals;

public class Shark extends Animal {
    
    public Shark(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal == null) throw new NullPointerException();
        else if (animal.toString().equals("Seal")) return false;
        else return true;
    }

    @Override
    public String toString() {
        return "Shark";
    }
}
