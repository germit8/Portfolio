package areas;

import java.util.ArrayList;

public class Aquarium extends Habitat {
    
    public Aquarium(int maxCapacity) {
        super(maxCapacity, 3);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }

    @Override
    public String toString() {
        return "Aquarium_" + this.getAreaID();
    }
}
