import areas.*;
import zoo.*;

public class ConnectTest {
 
    public static void main(String[] args) {

        Zoo zoo = new Zoo();

        PicnicArea picnic = new PicnicArea();
        Aquarium aquarium = new Aquarium(20);
        Enclosure enclosure1 = new Enclosure(10);
        Enclosure enclosure2 = new Enclosure(20);
        Enclosure enclosure3 = new Enclosure(20);
        Cage cage = new Cage(5);

        zoo.addArea(picnic);
        zoo.addArea(aquarium);
        zoo.addArea(enclosure1);
        zoo.addArea(enclosure2);
        zoo.addArea(enclosure3);
        zoo.addArea(cage);
        zoo.connectAreas(0, picnic.getAreaID());
        zoo.connectAreas(picnic.getAreaID(), 0);

        System.out.println(zoo.getArea(0).getAdjacentAreas());
        System.out.println(zoo.getArea(picnic.getAreaID()).getAdjacentAreas());
    }
}
