package zoo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import animals.Animal;
import areas.IArea;
import areas.Entrance;
import areas.Habitat;
import areas.Area;
import areas.Path;
import dataStructures.ICashCount;

public class Zoo implements IZoo {
    
    private int numOfAreas = 0;
    private HashMap<Integer, IArea> areas;
    private final Entrance entrance = Entrance.getEntrance();

    public Zoo() {
        this.areas = new HashMap<>(Map.of(0, entrance));
    }

    public int addArea(IArea area) {
        Area newArea = (Area) area;
        if (!newArea.getIsAPartOfZoo()) {
            int areaId = generateNewAreaID(newArea.getAreaSubID());
            newArea.setIsAPartOfZoo();
            newArea.setAreaID(areaId);
            areas.put(areaId, newArea);
            return areaId;
        }
        return newArea.getAreaID();
    }

    public int getNumOfAreas() {
        return numOfAreas;
    }

    public int generateNewAreaID(int areaSubId) {
        numOfAreas++;
        return 100 * areaSubId + numOfAreas;
    }

    public void removeArea(int areaID) {
        for (int key : areas.keySet()) {
            areas.get(key).getAdjacentAreas().remove((Integer) areaID);
        }
        areas.remove(areaID);
    }

    public IArea getArea(int areaID) {
        if (areas.get(areaID) == null) System.out.println("Area with ID: " + areaID + " is not in this zoo.");
        return areas.get(areaID);
    }

    public byte addAnimal(int areaID, Animal animal) {
        int areaDistinguisher = areaID / 100;
        
        if (areaDistinguisher == 4) return Codes.NOT_A_HABITAT;

        Habitat habitat = (Habitat) getArea(areaID);

        if (!habitat.getAllowedAnimals().contains(animal.toString())) return Codes.WRONG_HABITAT;
        if (habitat.getMaxCapacity() == habitat.getCurrentCapacity()) return Codes.HABITAT_FULL;
        
        for (Animal thatAnim : habitat.getAnimals()) {
            if (!animal.isCompatibleWith(thatAnim)) return Codes.INCOMPATIBLE_INHABITANTS;
        }

        habitat.addAnimal(animal);
        return Codes.ANIMAL_ADDED;
    }

    public HashMap<Integer, IArea> getAreasWithCodes() {
        return areas;
    }

    // -----------------------------------------------------------------------

    public void connectAreas(int fromAreaId, int toAreaId) {
        if (fromAreaId != toAreaId) {
            ((Path) getArea(fromAreaId)).addAdjacentAreas(toAreaId);
        } else {
            System.out.println("Cannot connect area to itself");
        }
    }

    public boolean isPathAllowed(ArrayList<Integer> areaIds) {
        boolean pathIsConnected = true;
        for (int i = 0; i < areaIds.size() - 1; i++) {
            if (!getArea(areaIds.get(i)).getAdjacentAreas().contains((Integer) areaIds.get(i + 1))) {
                pathIsConnected = false;
            }
        }
        return pathIsConnected;
    }

    public ArrayList<String> visit(ArrayList<Integer> areaIdsVisited) {
        ArrayList<String> visitedAnimals = new ArrayList<>();

        if (isPathAllowed(areaIdsVisited)) {
            for (Integer num : areaIdsVisited) {
                if (num != 0 && num / 100 != 4) {
                    visitedAnimals.addAll(((Habitat) getArea(num)).getAnimalsNames());
                }
            }
            return visitedAnimals;
        }
        return null;
    }

    public ArrayList<Integer> findUnreachableAreas() {
        ArrayList<Integer> allAdjacentAreas = new ArrayList<>();
        ArrayList<Integer> unreachableAreas = new ArrayList<>();

        for (int key : areas.keySet()) {
            allAdjacentAreas.addAll(areas.get(key).getAdjacentAreas());
        }
        Set<Integer> filteredAreas = new HashSet<>(allAdjacentAreas);
        allAdjacentAreas.clear();
        allAdjacentAreas.addAll(filteredAreas);

        unreachableAreas = getArrayIntersection(new ArrayList<>(areas.keySet()), allAdjacentAreas);
        unreachableAreas.remove((Integer) 0);
        return unreachableAreas;
    }

    public ArrayList<Integer> getArrayIntersection(ArrayList<Integer> list1, ArrayList<Integer> list2) {
        ArrayList<Integer> intersection = new ArrayList<>();

        for (Integer i : list1) {
            if (!list2.contains(i)) {
                intersection.add(i);
            }
        }

        return intersection;
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
