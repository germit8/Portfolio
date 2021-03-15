import java.awt.Color;

public class ImageEditor {
    
    public static double luminence(Color color) {
        double lum = 0.299 * color.getRed() + 0.587 * color.getGreen() + 0.114 * color.getBlue();
        return lum;
    }

    public static Color toGrey(Color color) {
        int lumColor = (int) Math.round(luminence(color));
        Color greyColor = new Color(lumColor, lumColor, lumColor);
        return greyColor;
    }

    public static Picture makeGreyScale(Picture pic) {
        Picture greyPic = new Picture(pic.width(), pic.height());

        for (int i = 0; i < pic.height(); i++) {
            for (int j = 0; j < pic.width(); j++) {
                Color pixelColor = toGrey(pic.get(j, i));
                greyPic.set(j, i, pixelColor);
            }
        }
        return greyPic;
    }
    public static void main(String[] args) {
        Picture p = new Picture("lion2.jpg"); // or use any other colour image
        Picture greyscale = makeGreyScale(p);
        greyscale.show();
    }
}
