package areas;

public abstract class Area implements IArea {
    
    private static int numOfAreas = 0;
    private final int areaID;

    Area(int areaSubId) {
        this.areaID = generateNewAreaID(areaSubId);
    }

    public int getNumOfAreas() {
        return numOfAreas;
    }

    public int generateNewAreaID(int areaSubId) {
        numOfAreas++;
        return 100 * areaSubId + numOfAreas;
    }

    public int getAreaID() {
        return areaID;
    }
}
