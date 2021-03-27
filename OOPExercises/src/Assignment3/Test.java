import areas.*;
import animals.*;
import zoo.*;
import java.util.ArrayList;
import java.util.List;

public class Test {

    public static void main(String[] args) {

        Zoo zoo = new Zoo();
        Zoo zoo2 = new Zoo();
        
        Gazelle gazelle = new Gazelle("Betty");
        Zebra zebra = new Zebra("Maria");
        Lion lion = new Lion("Frank");
        Shark shark = new Shark("Boris");
        Seal seal = new Seal("Navy");
        Buzzard buzzard = new Buzzard("Mitch");
        Buzzard buzzard2 = new Buzzard("Connel");
        Parrot parrot = new Parrot("Johnny");

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
        zoo.addArea(cage);

        zoo2.addArea(aquarium);
        zoo2.addArea(picnic);
        zoo2.addArea(enclosure1);
        zoo2.addArea(cage);
        zoo2.addArea(enclosure3);

        zoo.removeArea(100);
        System.out.println(zoo.getArea(203).toString());
        System.out.println(zoo.getArea(0).toString());
        System.out.println(zoo.getAreasWithCodes().toString());
        System.out.println(zoo2.getAreasWithCodes().toString());

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
        zoo.addAnimal(302, shark);
        zoo.addAnimal(302, shark);
        zoo.addAnimal(204, gazelle);
        zoo.addAnimal(105, buzzard);
        byte cageAdd = zoo.addAnimal(302, shark);

        zoo.connectAreas(0, 401);
        zoo.connectAreas(401, 203);
        zoo.connectAreas(302, 203);
        zoo.connectAreas(203, 204);
        zoo.connectAreas(204, 302);
        System.out.println(picnic.getAdjacentAreas().toString());
        System.out.println(aquarium.getAdjacentAreas().toString());
        System.out.println(enclosure1.getAdjacentAreas().toString());
        
        ArrayList<Integer> areaIds = new ArrayList<>(List.of(0, 401, 203, 204, 302));
        System.out.println("Codes: " + gazelleAdd + " " + picnicAdd + " " + notAllowedAdd + " " + zebraAdd + " " + lionAdd + " " + cageAdd);
        System.out.println(enclosure1.getAnimals().toString());
        System.out.println(zoo.visit(areaIds).toString());
        System.out.println(zoo.findUnreachableAreas().toString());
        System.out.println(zoo.getNumOfAreas());

        zoo.removeArea(203);
        System.out.println(zoo.getAreasWithCodes().toString());
        System.out.println(picnic.getAdjacentAreas().toString());
        System.out.println(aquarium.getAdjacentAreas().toString());
    }
}