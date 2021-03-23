public class CartTest {
    
    public static void main(String[] args) {
        ShoppingCart cartA = new ShoppingCart("Bert");
        ShoppingCart cartB = new ShoppingCart("Bert");
        CartComparatorName cc = new CartComparatorName();

        cartA.addItem(40.5);
        cartA.addItem(40.5);
        // cartA.addItem(20);

        cartB.addItem(40.5);
        cartB.addItem(30.5);
        // cartB.addItem(55.5);

        System.out.println(cc.compare(cartA, cartB));
    }
}
