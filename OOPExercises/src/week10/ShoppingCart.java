import java.util.List;
import java.util.ArrayList;
import java.util.Objects;

/**
 * Encapsulates the contents of a shopping cart in an online store.
 */
public class ShoppingCart {

    /** Name of the shopping cart's owner */
    private final String userName;
    /** Prices of items in the shopping cart */
    private final List<Double> itemPrices;

    /**
     * Create a new cart for the given user.
     * @param userName owner of the new cart
     * @throws NullPointerException if given userName is null
     */
    public ShoppingCart(String userName) {
        Objects.requireNonNull(userName);
        this.userName = userName;
        itemPrices = new ArrayList<>();
    }

    /**
     * Add price of item to shopping cart.
     * @param price the price of the item to be added
     * @throws IllegalArgumentException if the price of the given item is negative
     */
    public void addItem(double price) {
        if (price < 0) throw new IllegalArgumentException("Given price must be larger than zero but is: " + price);
        itemPrices.add(price);
    }

    /**
     * Returns the name of this cart's owner.
     * @return the name of this cart's owner.
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Returns a list with all item prices in the cart.
     * @return list with all item prices in the cart.
     */
    public List<Double> getItemPrices() {
       return new ArrayList<>(itemPrices); 
    }

   
    /**
     * Returns a String representation of this cart's state.
     * @return representation of this cart's state.
     */ 
    public String toString() {
        return new StringBuilder()
            .append(userName)
            .append(" :")
            .append(itemPrices)
            .toString();
    }
}