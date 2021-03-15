package music;

public class MusicArtist {

    private String musicArtist;
    private String musicSong;

    public MusicArtist(String artist, String song) {
        this.musicArtist = artist;
        this.musicSong = song;
    }

    public String getArtist() {
        return musicArtist;
    }

    public String getSong() {
        return musicSong;
    }

    public String toString() {
        return musicSong + " | " + musicArtist;
    }
}
