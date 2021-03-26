package areas;

import java.util.ArrayList;

public class Enclosure extends Habitat {
    
    public Enclosure(int maxCapacity) {
        super(maxCapacity, 2);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }

    @Override
    public String toString() {
        return "Enclosure_" + this.getAreaID();
    }
}
