package areas;

public class Cage extends Habitat {
    
    public Cage(int maxCapacity) {
        super(maxCapacity, 1);
    }

    @Override
    public String toString() {
        return "Cage_" + this.getAreaID();
    }
}
