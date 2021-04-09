package areas;

public abstract class Area extends Path {
    
    private int areaID = 1;
    private final int areaSubID;
    private boolean isAPartOfZoo = false;
    
    Area(int areaSubId) {
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
