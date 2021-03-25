package zoo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import animals.Animal;
import areas.IArea;
import areas.Entrance;
import areas.Habitat;
import areas.Area;
import dataStructures.ICashCount;

public class Zoo implements IZoo {
    
    private HashMap<Integer, IArea> areas;
    private final Entrance entrance = Entrance.getEntrance();

    public Zoo() {
        this.areas = new HashMap<>(Map.of(0, entrance));
    }

    public int addArea(IArea area) {
        Area newArea = (Area) area;
        areas.put(newArea.getAreaID(), newArea);
        return newArea.getAreaID();
    }

    public void removeArea(int areaID) {
        areas.remove(areaID);
    }

    public IArea getArea(int areaID) {
        if (areas.get(areaID) == null) System.out.println("Area with ID: " + areaID + " is not in this zoo.");
        return areas.get(areaID);
    }

    public byte addAnimal(int areaID, Animal animal) {
        int areaDistinguisher = areaID / 100;
        
        if (areaDistinguisher == 4) return Codes.NOT_A_HABITAT;

        Habitat area = (Habitat) getArea(areaID);
        
        if (!area.getAllowedAnimals().contains(animal.toString())) return Codes.WRONG_HABITAT;
        if (area.getMaxCapacity() == area.getCurrentCapacity()) return Codes.HABITAT_FULL;
        
        for (Animal anim : area.getAnimals()) {
            if (!animal.isCompatibleWith(anim)) return Codes.INCOMPATIBLE_INHABITANTS;
        }

        area.addAnimal(animal);
        return Codes.ANIMAL_ADDED;
    }

    public HashMap<Integer, IArea> getAreasWithCodes() {
        return areas;
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
