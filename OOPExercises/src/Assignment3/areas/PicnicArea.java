package areas;

import areas.IArea;
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
        return "PicnicArea-" + this.getAreaID();
    }
}
