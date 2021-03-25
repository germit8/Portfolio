package animals;

public class Zebra extends Animal {
    
    public Zebra(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal == null) throw new NullPointerException();
        else if (animal.toString() == "Lion") return false;
        else return true;
    }

    @Override
    public String toString() {
        return "Zebra";
    }
}
