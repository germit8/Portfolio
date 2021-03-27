package areas;

public class Entrance extends Path {
    
    private final static int ID = 0;
    private static Entrance entrance = new Entrance();

    private Entrance() {
        
    }

    public static Entrance getEntrance() {
        if (entrance == null) return new Entrance();
        else return entrance;
    }

    public int getEntranceID() {
        return ID;
    }

    @Override
    public String toString() {
        return "Entrance_0";
    }
}
