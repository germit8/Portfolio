package music;

import java.util.ArrayList;

public class Favourites {
    
    private ArrayList<MusicArtist> favs = new ArrayList<MusicArtist>();

    public void addTrack(String artist, String song) {
        MusicArtist myArtist = new MusicArtist(artist, song);
        favs.add(myArtist); 
    }

    public void showFavourites() {
        if (favs != null) {
            for (int i = 0; i < 5; i++) {
                System.out.println(favs.get(i).toString());
            }
        }
    }
}
