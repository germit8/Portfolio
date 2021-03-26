package areas;

import java.util.ArrayList;

public class PicnicArea extends Area {

    public PicnicArea() {
        super(4);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }

    @Override
    public String toString() {
        return "PicnicArea_" + this.getAreaID();
    }
}
