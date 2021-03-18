import java.util.ArrayList;

public class Videostore {
    
    private ArrayList<Video> videos = new ArrayList<>();

    public boolean addVideo(Video video) {
        for (Video vid : videos) {
            if (vid.getTitle().equals(video.getTitle())) {
                System.out.println(video.getTitle() + " is already in stock.");
                return false;
            }
        }
        videos.add(video);
        return true;
    }

    public Video getVideo(String title) {
        for (Video video : videos) {
            if (video.getTitle().equals(title)) {
                return video;
            }
        }
        System.out.println("Such video does not exist");
        return null;
    }

    public boolean checkOutVideo(String title) {
        Video videoToRent = getVideo(title);
        if (videoToRent != null) {
            return videoToRent.checkOut();
        }
        return false;
    }

    public boolean returnVideo(Video video) {
        return video.returnToStore();
    }

    public void rateVideo(Video video, int rating) {
        video.addRating(rating);
    }

    public double getAverageRatingForVideo(Video video) {
        return video.getAverageRating();
    }

    public int numOfCheckedOut() {
        int numOfCheckOut = 0;
        for (Video video : videos) {
            if (video.isCheckedOut()) {
                numOfCheckOut++;
            }
        }
        return numOfCheckOut;
    }

    public Video[] getCheckedOut() {
        Video[] checkedVideos = new Video[numOfCheckedOut()];
        int i = 0;
        for (Video video : videos) {
            if (video.isCheckedOut()) {
                checkedVideos[i] = video;
                i++;
            }
        }
        return checkedVideos;
    }

    public Video mostPopular() {
        Video highestRankingVideo = videos.get(0);
        for (Video video : videos) {
            if (highestRankingVideo.getAverageRating() < video.getAverageRating()) {
                highestRankingVideo = video;
            }
        }
        return highestRankingVideo;
    }
}
