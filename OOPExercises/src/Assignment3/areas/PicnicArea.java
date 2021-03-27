package areas;

public class PicnicArea extends Area {

    public PicnicArea() {
        super(4);
    }

    @Override
    public String toString() {
        return "PicnicArea_" + this.getAreaID();
    }
}
