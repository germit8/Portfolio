import areas.*;
import zoo.*;

public class ConnectTest {
 
    public static void main(String[] args) {

        Zoo zoo = new Zoo();
        PicnicArea picnic = new PicnicArea();
        Enclosure enclosure = new Enclosure(20);

        zoo.addArea(picnic);
        zoo.connectAreas(picnic.getAreaID(), 0);
        zoo.connectAreas(0, picnic.getAreaID());

        // System.out.println(zoo.getAreasWithCodes());

        // System.out.println(picnic.getAdjacentAreas().size());
        // System.out.println(zoo.getArea(0).getAdjacentAreas().size());
    }
}
