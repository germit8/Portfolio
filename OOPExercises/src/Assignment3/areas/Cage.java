package areas;

import java.util.ArrayList;

public class Cage extends Habitat {
    
    public Cage(int maxCapacity) {
        super(maxCapacity, 1);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }

    @Override
    public String toString() {
        return "Cage-" + this.getAreaID();
    }
}
