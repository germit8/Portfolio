import areas.*;
import zoo.*;

public class ConnectTest {
 
    public static void main(String[] args) {

        Zoo zoo = new Zoo();

        PicnicArea picnic = new PicnicArea();

        zoo.addArea(picnic);
        zoo.connectAreas(0, picnic.getAreaID());
        zoo.connectAreas(0, picnic.getAreaID());
        zoo.connectAreas(picnic.getAreaID(), 0);

        System.out.println(zoo.getArea(0).getAdjacentAreas().get(0));
        System.out.println(zoo.getArea(picnic.getAreaID()).getAdjacentAreas().get(0));
    }
}
