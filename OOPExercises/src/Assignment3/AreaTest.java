import areas.*;
import animals.*;

public class AreaTest {
    
    public static void main(String[] args) {
        Enclosure enclosure1 = new Enclosure(10);
        Enclosure enclosure2 = new Enclosure(20);
        Gazelle gazelle = new Gazelle("Betty");
        Zebra zebra = new Zebra("Maria");
        Lion lion = new Lion("Frank");
        Shark shark = new Shark("Boris");
        Seal seal = new Seal("Navy");


        PicnicArea picnic1 = new PicnicArea();
        PicnicArea picnic2 = new PicnicArea();
        Entrance entrance = Entrance.getEntrance();

        System.out.println(picnic1.getNumOfAreas());
        System.out.println(picnic2.getNumOfAreas());
        System.out.println(enclosure1.getNumOfAreas());
        System.out.println(enclosure2.getNumOfAreas());

        System.out.println("Enclosure 1: ");
        System.out.println(enclosure1.getMaxCapacity());
        enclosure1.addAnimal(gazelle);
        enclosure1.addAnimal(zebra);
        System.out.println(enclosure1.getCurrentCapacity());
        System.out.println(enclosure1.getAllowedAnimals().toString());
        System.out.println(enclosure1.getAnimals().toString());


        System.out.println("Enclosure 2: ");
        System.out.println(enclosure2.getMaxCapacity());
        enclosure2.addAnimal(shark);
        enclosure2.addAnimal(seal);
        System.out.println(enclosure2.getCurrentCapacity());
        System.out.println(enclosure2.getAllowedAnimals().toString());
        System.out.println(enclosure2.getAnimals().toString());

        System.out.println(enclosure1.getAreaID());
        System.out.println(enclosure2.getAreaID());
        System.out.println(picnic1.getAreaID());
        System.out.println(picnic2.getAreaID());
    }
}
