package areas;

public abstract class Area extends Path {
    
    // areaID = 1 is a default that signals that it has not been added to any zoo yet
    private int areaID = 1;
    private final int areaSubID;
    private boolean isAPartOfZoo = false;
    
    protected Area(int areaSubId) {
        this.areaSubID = areaSubId;
    }

    // -------------------------------- SETTERS --------------------------------
    public void setAreaID(int newID) {
        areaID = newID;
    }

    public void setIsAPartOfZoo(boolean zooPart) {
        isAPartOfZoo = zooPart;
    }

    // -------------------------------- GETTERS --------------------------------

    public int getAreaSubID() {
        return areaSubID;
    }

    public int getAreaID() {
        return areaID;
    }

    public boolean getIsAPartOfZoo() {
        return isAPartOfZoo;
    }
}
