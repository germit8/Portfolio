import areas.*;
import animals.*;
import zoo.*;

public class ZooTest {

    public static void main(String[] args) {

        Zoo zoo = new Zoo();
        
        Gazelle gazelle = new Gazelle("Betty");
        Zebra zebra = new Zebra("Maria");
        Lion lion = new Lion("Frank");
        Shark shark = new Shark("Boris");
        Seal seal = new Seal("Navy");

        PicnicArea picnic = new PicnicArea();
        Aquarium aquarium = new Aquarium(20);
        Enclosure enclosure1 = new Enclosure(10);
        Enclosure enclosure2 = new Enclosure(20);
        Cage cage = new Cage(5);

        zoo.addArea(picnic);
        zoo.addArea(aquarium);
        zoo.addArea(enclosure1);
        zoo.addArea(enclosure2);
        zoo.addArea(cage);

        zoo.removeArea(100);
        System.out.println(zoo.getArea(203).toString());
        System.out.println(zoo.getArea(0).toString());
        System.out.println(zoo.getAreasWithCodes().toString());

        byte gazelleAdd = zoo.addAnimal(203, gazelle);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(203, zebra);
        zoo.addAnimal(204, zebra);
        byte zebraAdd = zoo.addAnimal(203, zebra);
        byte lionAdd = zoo.addAnimal(204, lion);
        byte picnicAdd = zoo.addAnimal(401, gazelle);
        byte notAllowedAdd = zoo.addAnimal(203, shark);
        

        System.out.println("Codes: " + gazelleAdd + " " + picnicAdd + " " + notAllowedAdd + " " + zebraAdd + " " + lionAdd);
        System.out.println(enclosure1.getAnimals().toString());
        
    }
}