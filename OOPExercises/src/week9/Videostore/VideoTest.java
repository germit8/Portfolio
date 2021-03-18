public class VideoTest {
    
    public static void main(String[] args) {
        Video video1 = new Video("Django Unchained");
        Video video3 = new Video("Alice in Wonderland");
        Video video4 = new Video("Harry Potter");

        Videostore myStore = new Videostore();

        myStore.addVideo(video1);
        myStore.addVideo(video3);
        myStore.addVideo(video4);

        System.out.println("Add video checkpoint\n");

        myStore.rateVideo(video1, 3);
        myStore.rateVideo(video1, 4);
        myStore.rateVideo(video1, 5);
        myStore.rateVideo(video3, 2);
        myStore.rateVideo(video4, 6);
        myStore.rateVideo(video4, 1);
        myStore.rateVideo(video3, 3);

        System.out.println("Rate video checkpoint\n");

        myStore.checkOutVideo(video1.getTitle());
        myStore.checkOutVideo(video3.getTitle());
        myStore.checkOutVideo(video4.getTitle());

        video1.returnToStore();
        System.out.println(video1.isCheckedOut());

        System.out.println("Checked video checkpoint\n");

        Video[] checked = myStore.getCheckedOut();
        for (Video video : checked) {
            System.out.println(video.toString());
            System.out.println(video.getAverageRating());
        }
        System.out.println(myStore.mostPopular());
    }
}
