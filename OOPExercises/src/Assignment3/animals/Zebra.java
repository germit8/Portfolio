package animals;

public class Zebra extends Animal {
    
    public Zebra(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        if (animal.toString().equals("Lion")) return false;
        return true;
    }

    @Override
    public String toString() {
        return "Zebra";
    }
}
