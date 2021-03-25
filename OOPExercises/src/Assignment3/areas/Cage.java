package areas;

import java.util.ArrayList;

public class Cage extends Habitat implements IArea {
    
    public Cage(int maxCapacity) {
        super(maxCapacity, 1);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }
}
