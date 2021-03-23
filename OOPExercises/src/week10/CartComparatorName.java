public class CartComparatorName extends CartComparator {
    
    public int compare(ShoppingCart a, ShoppingCart b) {
        String nameA = a.getUserName();
        String nameB = b.getUserName();
        int result = super.compare(a, b);

        if (result == 0) {
            if (nameA.compareTo(nameB) == -1) return -1;
            else if (nameA.compareTo(nameB) == 1) return 1;
            else return result;
        }
        return result;        
    }
}
