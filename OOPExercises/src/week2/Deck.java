public class Deck {
    public static void main(String[] args) {
        final String[] RANK = {"2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"};
        final String[] SUIT = {"Hearts", "Spades", "Clubs", "Diamonds"};

        final int SUITS = SUIT.length;
        final int RANKS = RANK.length;
        final int CARDS = RANKS * SUITS;

        String[] deck = new String[CARDS];
        for (int i = 0; i < RANKS; i++) {
            for (int j = 0; j < SUITS; j++) {
                deck[SUITS * i + j] = RANK[i] + " of " + SUIT[j];
            }
        }

        for (int i = 0; i < CARDS; i++) {
            int randomCard = i + (int) (Math.random() * (CARDS - i));
            String temp = deck[randomCard];
            deck[randomCard] = deck[i];
            deck[i] = temp;
        }

        for (int k = 0; k < CARDS; k++) {
            System.out.println(deck[k]);
        }
    }
}