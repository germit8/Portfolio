// package music;

public class Favourites {
    
    private MusicArtist[] favs = new MusicArtist[5];
    private int songCounter = 0;

    public void addTrack(String artist, String song) {
        MusicArtist myArtist = new MusicArtist(artist, song);
        if (songCounter < 5) {
            favs[songCounter] = myArtist;
            songCounter++;
        } else {
            System.out.println("Sorry can't add: " + myArtist.toString());
        }     
    }

    public void showFavourites() {
        if (favs != null) {
            for (int i = 0; i < 5; i++) {
                System.out.println(favs[i].toString());
            }
        }
    }

    public static void main(String[] args) {
        Favourites favourites = new Favourites();
        favourites.addTrack("Fun", "Some Nights");
        favourites.addTrack("Oliver Tank", "Help You Breathe");
        favourites.addTrack("Horse Feathers", "Fit Against the Country");
        favourites.addTrack("Emile Sande", "Country House");
        favourites.addTrack("Fun", "Walking the Dog");
        favourites.addTrack("Porcelain Raft", "Put Me To Sleep");
        favourites.showFavourites();
    }
}
