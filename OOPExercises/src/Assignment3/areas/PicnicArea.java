package areas;

import areas.IArea;
import java.util.ArrayList;

public class PicnicArea extends Area implements IArea {

    public PicnicArea() {
        super(4);
    }

    @Override
    public ArrayList<Integer> getAdjacentAreas() {
        return null;
    }
}
