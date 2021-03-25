package zoo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import animals.Animal;
import zoo.Codes;
import areas.IArea;

public class Zoo implements IZoo {
    
    private HashMap<Integer, IArea> areas;
    private final Entrance entrance = Entrance.getEntrance();

    public Zoo() {
        this.areas = new HashMap<>(Map.of(0, entrance));
    }

    public int addArea(IArea area) {
        areas.put(area.getAreaID(), area);
        return area.getAreaID();
    }

    public void removeArea(int areaID) {
        areas.remove(areaID);
    }

    public IArea getArea(int areaID) {
        return areas.get(areaID);
    }

    public byte addAnimal(int areaID, Animal animal) {
        int areaDistinguisher = areaID / 100;
        IArea area = getArea(areaID);
        
        if (areaDistinguisher == 4) return Codes.NOT_A_HABITAT;
        else if (!area.getAllowedAnimals().contains(animal.toString())) return Codes.WRONG_HABITAT;
        else if (area.getMaxCapacity() == area.getCurrentCapacity()) return Codes.HABITAT_FULL;
        
        for (Animal anim : area.getAnimals()) {
            if (!animal.isCompatibleWith(anim)) return Codes.INCOMPATIBLE_INHABITANTS;
        }

        area.addAnimal(animal);
        return Codes.ANIMAL_ADDED;
    }

    // -----------------------------------------------------------------------

    public void connectAreas(int fromAreaId, int toAreaId) {
        
    }

    public boolean isPathAllowed(ArrayList<Integer> areaIds) {
        return false;
    }

    public ArrayList<String> visit(ArrayList<Integer> areaIdsVisited) {
        return null;
    }

    public ArrayList<Integer> findUnreachableAreas() {
        return null;
    }

    // ------------------------------------------------------------------------

    public void setEntranceFee(int pounds, int pence) {
        
    }

    public void setCashSupply(ICashCount coins) {
        
    }

    public ICashCount getCashSupply() {
        return null;
    }

    public ICashCount payEntranceFee(ICashCount cashInserted) {
        return null;
    }

}
