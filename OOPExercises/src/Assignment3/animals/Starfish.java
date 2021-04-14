package animals;

public class Starfish extends Animal {
    
    public Starfish(String name) {
        super(name);
    }

    @Override
    public boolean isCompatibleWith(Animal animal) {
        return true;
    }

    @Override
    public String toString() {
        return "Starfish";
    }
}
