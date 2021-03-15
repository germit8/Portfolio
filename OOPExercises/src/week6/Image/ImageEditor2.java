import java.awt.Color;

public class ImageEditor2 {
    
    public static Picture threshold(Picture picture, int threshold) {
        Picture newPic = new Picture(picture.width(), picture.height());

        for (int i = 0; i < picture.height(); i++) {
            for (int j = 0; j < picture.width(); j++) {
                if (ImageEditor.luminence(picture.get(j, i)) < threshold) {
                    newPic.set(j, i, Color.BLACK);
                } else {
                    newPic.set(j, i, ImageEditor.toGrey(picture.get(j, i)));
                }
            }
        }

        return newPic;
    }

    public static void main(String[] args) {
        Picture p = new Picture("lion2.jpg");
        Picture t = threshold(p, 120);
        t.show();
    }
}
