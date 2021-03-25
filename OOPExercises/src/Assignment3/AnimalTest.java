import animals.*;

public class AnimalTest {
    
    public static void main(String[] args) {
        Zebra zebra = new Zebra("Polly");
        Gazelle gazelle = new Gazelle("Gertrude");
        Lion lion = new Lion("Franz");
        Shark shark = new Shark("Stanley");
        Starfish starfish = new Starfish("Margarita");
        Seal seal = new Seal("Boris");
        Buzzard buzzard = new Buzzard("Brock");
        Parrot parrot = new Parrot("Michael");

        System.out.println(zebra.getNickname());
        System.out.println(zebra.toString());

        System.out.println(zebra.isCompatibleWith(gazelle));
        System.out.println(gazelle.isCompatibleWith(zebra));
        System.out.println(gazelle.isCompatibleWith(lion));
        System.out.println(lion.isCompatibleWith(zebra));

        System.out.println(parrot.isCompatibleWith(buzzard));
        System.out.println(shark.isCompatibleWith(starfish));
        System.out.println(starfish.isCompatibleWith(shark));
    }
}
