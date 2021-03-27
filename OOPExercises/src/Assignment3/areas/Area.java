package areas;

public abstract class Area extends Path {
    
    private int areaID;
    private final int areaSubID;
    private boolean isAPartOfZoo = false;
    
    Area(int areaSubId) {
        this.areaSubID = areaSubId;
    }

    public void setAreaID(int newID) {
        areaID = newID;
    }

    public void setIsAPartOfZoo(boolean zooPart) {
        isAPartOfZoo = zooPart;
    }

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
