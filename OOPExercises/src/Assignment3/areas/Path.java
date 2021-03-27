package areas;

import java.util.ArrayList;

public abstract class Path implements IArea {
    
    private ArrayList<Integer> adjacentAreas = new ArrayList<>();

    Path() {
        
    }

    public void addAdjacentAreas(int areaId) {
        adjacentAreas.add(areaId);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return adjacentAreas;
    }
}
