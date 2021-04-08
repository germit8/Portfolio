package zoo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.Collections;

import animals.Animal;
import areas.IArea;
import areas.Entrance;
import areas.Habitat;
import areas.Area;
import areas.Path;
import dataStructures.ICashCount;
import dataStructures.CashCount;

public class Zoo implements IZoo {
    
    private int entranceFeeTotalInPence;
    private int numOfAreas = 0;
    private HashMap<Integer, IArea> areas;
    private final Entrance entrance = Entrance.getEntrance();
    private CashCount cashSupply = new CashCount();

    public Zoo() {
        this.areas = new HashMap<>(Map.of(entrance.getEntranceID(), entrance));
    }

    // check for entrance
    public int addArea(IArea area) {
        if (area instanceof Entrance) return 0;
        Area newArea = (Area) area;

        if (!newArea.getIsAPartOfZoo()) {
            if (newArea.getAreaID() == 1) {
                int areaId = generateNewAreaID(newArea.getAreaSubID());
                newArea.setAreaID(areaId);
                areas.put(areaId, newArea);
            } else {
                areas.put(newArea.getAreaID(), newArea);
            }
            newArea.setIsAPartOfZoo(true);
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
        if (areaID != 0) {
            for (int key : areas.keySet()) {
                areas.get(key).getAdjacentAreas().remove((Integer) areaID);
            }
            Area theArea = (Area) getArea(areaID);
            if (theArea != null) theArea.setIsAPartOfZoo(false);
            areas.remove(areaID);
        }
        
    }

    public IArea getArea(int areaID) {
        if (areas.get(areaID) == null) System.out.println("Area with ID: " + areaID + " is not in this zoo.");
        return areas.get(areaID);
    }

    public byte addAnimal(int areaID, Animal animal) {
        
        if (areaID / 100 == 4 || areaID == 0) return Codes.NOT_A_HABITAT;

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
        if (areas.containsKey(fromAreaId) && areas.containsKey(toAreaId)) {
            if (fromAreaId != toAreaId) {
                ((Path) getArea(fromAreaId)).addAdjacentAreas(toAreaId);
            }
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
        removeDuplicates(allAdjacentAreas);
        unreachableAreas = getArrayIntersection(new ArrayList<>(areas.keySet()), allAdjacentAreas);
        unreachableAreas.remove((Integer) entrance.getEntranceID());

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

    public void removeDuplicates(ArrayList<Integer> arrayList) {
        Set<Integer> filteredAreas = new HashSet<>(arrayList);
        arrayList.clear();
        arrayList.addAll(filteredAreas);
    }
    // ------------------------------------------------------------------------

    public int getEntranceFee() {
        return entranceFeeTotalInPence;
    }

    public void setEntranceFee(int pounds, int pence) {
        entranceFeeTotalInPence = pence + pounds * 100;
    }

    public void setCashSupply(ICashCount coins) {
        cashSupply = (CashCount) coins;
    }

    public ICashCount getCashSupply() {
        return cashSupply;
    }

    public ICashCount payEntranceFee(ICashCount cashInserted) {
        CashCount cashIn = (CashCount) cashInserted;

        if (entranceFeeTotalInPence < cashIn.getCashSum()) {
            return calculateChange(cashIn);
        } else if (entranceFeeTotalInPence > cashIn.getCashSum()) {
            return cashInserted;
        }
        return new CashCount();
    }

    public ICashCount calculateChange(CashCount cashIn) {
        int desiredChange = cashIn.getCashSum() - entranceFeeTotalInPence;
        CashCount theChange = new CashCount();

        if (desiredChange % 10 != 0) return cashIn;

        for (int key : cashIn.getCoinsAndNotes()) {
            int numOfCoins = cashSupply.getCashSupply().get(key);
            while (desiredChange > 0 && numOfCoins > 0 && key <= desiredChange) {
                desiredChange -= key;
                numOfCoins--;
                theChange.getCashSupply().put(key, theChange.getCashSupply().get(key) + 1);
            }
        }

        if (desiredChange != 0) return cashIn;
        
        return theChange;
    }
}
