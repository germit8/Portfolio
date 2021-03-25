package areas;

import java.util.ArrayList;

public class Aquarium extends Habitat implements IArea {
    
    public Aquarium(int maxCapacity) {
        super(maxCapacity, 3);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }
}
