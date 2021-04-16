package zoo;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

import animals.Animal;
import areas.IArea;
import areas.Entrance;
import areas.Habitat;
import areas.Area;
import dataStructures.*;


public class Zoo implements IZoo {
    
    private int entranceFeeTotalInPence;
    private int numOfAreas = 0;
    private final Entrance entrance = new Entrance();
    private ICashCount cashSupply = new CashCount();
    private HashMap<Integer, IArea> areas = new HashMap<>(Map.of(entrance.getEntranceID(), entrance));
    private static final ArrayList<Integer> COINS_AND_NOTES = new ArrayList<>(List.of(2000, 1000, 500, 200, 100, 50, 20, 10));

    public Zoo() {

    }

    @Override
    public int addArea(IArea area) {
        if (area instanceof Entrance) return 0;
        Area newArea = (Area) area;

        // Checks if area is already assigned to a zoo
        if (!newArea.getIsAPartOfZoo()) {
            // Every unassigned area has areaID = 1 until it is assigned -> changed
            if (newArea.getAreaID() == 1) {
                int areaId = generateNewAreaID(newArea.getAreaSubID());
                newArea.setAreaID(areaId);
                areas.put(areaId, newArea);
            // If area has been assigned and then removed, it is only put back again
            } else {
                areas.put(newArea.getAreaID(), newArea);
            }
            newArea.setIsAPartOfZoo(true);
        }
        return newArea.getAreaID();
    }

    // ID is generated based on sub-ID (1,2,3,4)
    private int generateNewAreaID(int areaSubId) {
        numOfAreas++;
        return 100 * areaSubId + numOfAreas;
    }

    @Override
    public void removeArea(int areaID) {
        if (areaID != 0) {
            removeFromAllAdjacentAreas(areaID);
            Area theArea = (Area) getArea(areaID);
            if (theArea != null) {
                theArea.setIsAPartOfZoo(false);
                theArea.getAdjacentAreas().clear();
            }
            areas.remove(areaID);
        }
    }

    private void removeFromAllAdjacentAreas(int areaID) {
        for (int key : areas.keySet()) {
            areas.get(key).removeAdjacentArea(areaID);
        }
    }

    @Override
    public IArea getArea(int areaID) {
        return areas.get(areaID);
    }

    @Override
    public byte addAnimal(int areaID, Animal animal) {
        
        // 4 are all picnics and 0 is th entrance
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

    @Override
    public void connectAreas(int fromAreaId, int toAreaId) {

        // Checks if both ID's are in zoo, if they are not identical and if ID is not a duplicate
        if (areas.containsKey(fromAreaId) &&
            areas.containsKey(toAreaId) && 
            fromAreaId != toAreaId &&
            !areas.get(fromAreaId).getAdjacentAreas().contains((Integer) toAreaId)) {
                areas.get(fromAreaId).addAdjacentArea(toAreaId);
        }
    }

    @Override
    public boolean isPathAllowed(ArrayList<Integer> areaIds) {
        boolean pathIsConnected = true;
        for (int i = 0; i < areaIds.size() - 1; i++) {
            // Checks if the area to visit (i+1) is in the list of adjacentAreas of the current area (i)
            if (!getArea(areaIds.get(i)).getAdjacentAreas().contains((Integer) areaIds.get(i + 1))) {
                pathIsConnected = false;
            }
        }
        return pathIsConnected;
    }

    @Override
    public ArrayList<String> visit(ArrayList<Integer> areaIdsVisited) {
        ArrayList<String> visitedAnimals = new ArrayList<>();

        // First checks if such path is even allowed
        if (isPathAllowed(areaIdsVisited)) {
            for (Integer num : areaIdsVisited) {
                // From every habitat, all names of all animals are added to the list
                if (num != 0 && num / 100 != 4) {
                    visitedAnimals.addAll(((Habitat) getArea(num)).getAnimalsNames());
                }
            }
            return visitedAnimals;
        }
        return null;
    }

    @Override
    public ArrayList<Integer> findUnreachableAreas() {
        ArrayList<Integer> allAdjacentAreas = new ArrayList<>();
        ArrayList<Integer> unreachableAreas = new ArrayList<>();

        // Makes a list of all possible connections and removes duplicates
        allAdjacentAreas = getAllAdjacentAreas();
        removeDuplicates(allAdjacentAreas);

        // Finds the intersection of accessible areas and all areas in the zoo - the intersection is the unreachable
        unreachableAreas = getArrayIntersection(new ArrayList<>(areas.keySet()), allAdjacentAreas);
        unreachableAreas.remove((Integer) entrance.getEntranceID());

        return unreachableAreas;
    }

    private ArrayList<Integer> getAllAdjacentAreas() {
        ArrayList<Integer> allAdjacentAreas = new ArrayList<>();
        for (int key : areas.keySet()) {
            allAdjacentAreas.addAll(areas.get(key).getAdjacentAreas());
        }
        return allAdjacentAreas;
    }

    private ArrayList<Integer> getArrayIntersection(ArrayList<Integer> longerList, ArrayList<Integer> shorterList) {
        ArrayList<Integer> intersection = new ArrayList<>();

        for (Integer i : longerList) {
            if (!shorterList.contains(i)) {
                intersection.add(i);
            }
        }
        return intersection;
    }

    private void removeDuplicates(ArrayList<Integer> arrayList) {
        Set<Integer> filteredAreas = new HashSet<>(arrayList);
        arrayList.clear();
        arrayList.addAll(filteredAreas);
    }
    // ------------------------------------------------------------------------

    public int getEntranceFee() {
        return entranceFeeTotalInPence;
    }

    @Override
    public void setEntranceFee(int pounds, int pence) {
        entranceFeeTotalInPence = pence + pounds * 100;
    }

    @Override
    public void setCashSupply(ICashCount coins) {
        cashSupply = coins;
    }

    @Override
    public ICashCount getCashSupply() {
        return cashSupply;
    }

    @Override
    public ICashCount payEntranceFee(ICashCount cashInserted) {
        if (entranceFeeTotalInPence < getCashSum(cashInserted)) {
            return determineChange(cashInserted);
        } else if (entranceFeeTotalInPence > getCashSum(cashInserted)) {
            return cashInserted;
        } else {
            addMoneyForExactPayment(cashInserted);
            return new CashCount();
        }
    }

    private ICashCount determineChange(ICashCount cashIn) {
        int targetChange = getCashSum(cashIn) - entranceFeeTotalInPence;
        ICashCount theChange = new CashCount();
        ICashCount revertToOriginalSupply = new CashCount(cashSupply);

        for (int denomination : COINS_AND_NOTES) {

            // First the given cash is added to the cashSupply pool - this ensures that given money can be used for change
            setCoinOrNoteState(denomination, cashSupply, getCoinOrNoteState(denomination, cashIn));
            int numOfCoins = getCoinOrNoteState(denomination, cashSupply);
            
            // It keeps on giving the largest denomination as long as it is possible, then moves to smaller and repeats
            while (targetChange > 0 && numOfCoins > 0 && denomination <= targetChange) {
                targetChange -= denomination;
                numOfCoins--;
                setCoinOrNoteState(denomination, theChange, 1);
                setCoinOrNoteState(denomination, cashSupply, -1);
            }

            if (targetChange == 0) break;
        }

        // If it is not possible to give exact change, cashSupply is reverted to its original state and money is refunded
        if (targetChange != 0) {
            cashSupply = revertToOriginalSupply;
            return cashIn;
        }
        
        return theChange;
    }

    private void addMoneyForExactPayment(ICashCount cashIn) {
        for (int denomination : COINS_AND_NOTES) {
            // Current state of note/coin of inserted cash is added to cashSupply
            setCoinOrNoteState(denomination, cashSupply, getCoinOrNoteState(denomination, cashIn));
        }
    }

    private int getCashSum(ICashCount cashCount) {
        return cashCount.getNrNotes_20pounds() * 2000 +
               cashCount.getNrNotes_10pounds() * 1000 +
               cashCount.getNrNotes_5pounds() * 500 +
               cashCount.getNrCoins_2pounds() * 200 +
               cashCount.getNrCoins_1pound() * 100 +
               cashCount.getNrCoins_50p() * 50 +
               cashCount.getNrCoins_20p() * 20 +
               cashCount.getNrCoins_10p() * 10;
    }

    // Getter for current state of note/coin
    private int getCoinOrNoteState(int moneyValue, ICashCount cash) {
        switch (moneyValue) {
            case 2000:
                return cash.getNrNotes_20pounds();
            case 1000:
                return cash.getNrNotes_10pounds();
            case 500:
                return cash.getNrNotes_5pounds();
            case 200:
                return cash.getNrCoins_2pounds();
            case 100:
                return cash.getNrCoins_1pound();
            case 50:
                return cash.getNrCoins_50p();
            case 20:
                return cash.getNrCoins_20p();
            case 10:
                return cash.getNrCoins_10p();
        }
        return 0;
    }

    // Incrementer/Decrementer of current state of note/coin based on added amount
    private void setCoinOrNoteState(int moneyValue, ICashCount cash, int addAmount) {
        int currStateOfMoneyAndAdded = addAmount + getCoinOrNoteState(moneyValue, cash);
        switch (moneyValue) {
            case 2000:
                cash.setNrNotes_20pounds(currStateOfMoneyAndAdded);
                break;
            case 1000:
                cash.setNrNotes_10pounds(currStateOfMoneyAndAdded);
                break;
            case 500:
                cash.setNrNotes_5pounds(currStateOfMoneyAndAdded);
                break;
            case 200:
                cash.setNrCoins_2pounds(currStateOfMoneyAndAdded);
                break;
            case 100:
                cash.setNrCoins_1pound(currStateOfMoneyAndAdded);
                break;
            case 50:
                cash.setNrCoins_50p(currStateOfMoneyAndAdded);
                break;
            case 20:
                cash.setNrCoins_20p(currStateOfMoneyAndAdded);
                break;
            case 10:
                cash.setNrCoins_10p(currStateOfMoneyAndAdded);
                break;
        }
    }
}
