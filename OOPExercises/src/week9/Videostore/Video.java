import java.util.ArrayList;
import java.util.Arrays;

public class Video {
    
    private boolean isRented = false;
    private final String title;
    private int ratingsSum = 0;
    private int ratingsCount = 0;

    public Video(String title) {
        this.title = title;
    }

    public String getTitle() {
        return this.title;
    }

    public boolean addRating(int rating) {
        if (rating <= 5 && rating >= 1) {
            ratingsCount++;
            ratingsSum += rating;
            return true;
        } else {
            System.out.println("The rating should be between 1 and 5");
            return false;
        }
    }

    public double getAverageRating() {
        return Math.round(ratingsSum / ratingsCount);
    }

    public boolean checkOut() {
        if (isRented) {
            System.out.println("Video unavailable");
            return false;
        } else {
            isRented = true;
            System.out.println("Rental successful");
            return true;
        }
    }

    public boolean returnToStore() {
        if (isRented) {
            isRented = false;
            System.out.println("Return successful");
            return true;
        } else {
            System.out.println("Video is not rented");
            return false;
        }
    }

    public boolean isCheckedOut() {
        return isRented;
    }

    @Override
    public String toString() {
        return "Video[Title: " + title + ", Rent status: " + isRented + "].";
    }
}
