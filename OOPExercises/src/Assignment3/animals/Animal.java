package animals;

/**
 * You can modify the contents of this class, but you cannot:
 * - change the name, parameters or return types of provided methods
 * - remove it entirely
 */
public abstract class Animal {

	private final String nickname;

	public Animal(String nickname) {
		this.nickname = nickname;
	}

	/**
	 * @return Returns this animal's given name.
	 */
	public String getNickname() {
		return nickname;
	}
	
	/**
	 * Check whether two animals can live together.
	 * @param animal The animal for which to check compatibility with this animal.
	 * @return Returns true for compatible animals and false otherwise.
	 */
	public boolean isCompatibleWith(Animal animal) {
		if (animal == null) throw new NullPointerException();
		else if (this.toString().equals(animal.toString())) return true;
		return false;
	}
}
