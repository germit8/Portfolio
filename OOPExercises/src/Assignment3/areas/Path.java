package areas;

import java.util.ArrayList;

public abstract class Path implements IArea {
    
    private ArrayList<Integer> adjacentAreas = new ArrayList<>();

    protected Path() {
        
    }

    @Override
    public void addAdjacentArea(int areaId) {
        adjacentAreas.add(areaId);
    }

    @Override
    public void removeAdjacentArea(int areaID) {
        adjacentAreas.remove((Integer) areaID);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return adjacentAreas;
    }
}
