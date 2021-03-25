package animals;
import animals.Animal;

public class Parrot extends Animal {
    
    public Parrot(String name) {
        super(name);
    }

    @Override
    public String toString() {
        return "Parrot";
    }
}
