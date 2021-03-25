package areas;

import areas.IArea;
import java.util.ArrayList;

public class Enclosure extends Habitat implements IArea {
    
    public Enclosure(int maxCapacity) {
        super(maxCapacity, 2);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }
}
