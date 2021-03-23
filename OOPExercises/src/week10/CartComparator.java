import java.util.Comparator;
import java.util.List;

public class CartComparator implements Comparator<ShoppingCart> {
    
    @Override
    public int compare(ShoppingCart a, ShoppingCart b) {
        double averageA = getAverageFromPrices(a);
        double averageB = getAverageFromPrices(b);

        if (averageA > averageB) return 1;
        else if (averageA < averageB) return -1;
        else return 0;
    }

    public double getAverageFromPrices(ShoppingCart cart) {
        List<Double> prices = cart.getItemPrices();
        double sum = 0;

        for (double price : prices) sum += price;

        return sum / prices.size();
    }
}
