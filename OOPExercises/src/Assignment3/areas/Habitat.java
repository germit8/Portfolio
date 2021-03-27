package areas;

import java.util.ArrayList;
import java.util.List;
import animals.Animal;

public abstract class Habitat extends Area {
    
    private final int maxCapacity;
    private int currentCapacity = 0;
    private ArrayList<Animal> animals = new ArrayList<>();
    private final ArrayList<String> allowedAnimals;

    Habitat(int maxCapacity, int areaSubID) {
        super(areaSubID);
        this.maxCapacity = maxCapacity;
        this.allowedAnimals = allowedAnimals(areaSubID);
    }

    public int getMaxCapacity() {
        return maxCapacity;
    }

    public int getCurrentCapacity() {
        return currentCapacity;
    }

    public ArrayList<Animal> getAnimals() {
        return animals;
    }

    public ArrayList<String> getAnimalsNames() {
        ArrayList<String> animalNames = new ArrayList<>();
        for (Animal animal : this.animals) {
            animalNames.add(animal.getNickname());
        }
        return animalNames;
    }

    public ArrayList<String> getAllowedAnimals() {
        return allowedAnimals;
    }

    public void addAnimal(Animal animal) {
        animals.add(animal);
        currentCapacity++;
    }

    public ArrayList<String> allowedAnimals(int areaSubID) {
        switch (areaSubID) {
            case 1:
                return new ArrayList<>(List.of("Buzzard", "Parrot"));
            case 2:
                return new ArrayList<>(List.of("Lion", "Gazelle", "Zebra"));
            case 3:
                return new ArrayList<>(List.of("Shark", "Starfish", "Seal"));
        }
        return null;
    }
}
