package areas;

public class Enclosure extends Habitat {
    
    public Enclosure(int maxCapacity) {
        super(maxCapacity, 2);
    }

    @Override
    public String toString() {
        return "Enclosure_" + this.getAreaID();
    }
}
