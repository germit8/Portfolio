package areas;

public class Aquarium extends Habitat {
    
    public Aquarium(int maxCapacity) {
        super(maxCapacity, 3);
    }

    @Override
    public String toString() {
        return "Aquarium_" + this.getAreaID();
    }
}
