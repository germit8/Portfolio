import java.awt.Color;
import pointPackage.Point2D;

public class Voronoi {

    public static double pointDist(Point2D point1, Point2D point2) {
        double xDist = point1.getX() - point2.getX();
        double yDist = point1.getY() - point2.getY();

        return Math.sqrt(xDist * xDist + yDist * yDist);
    }

    public static int findClosestPoint(Point2D point, Point2D[] sites) {
        double[] distances = new double[sites.length];
        for (int i = 0; i < distances.length; i++) {
            distances[i] = pointDist(point, sites[i]);
        }

        double tempDist = distances[0];
        int temp = 0;
        for (int j = 0; j < distances.length - 1; j++) {
            if (tempDist > distances[j+1]) {
                tempDist = distances[j+1];
                temp = j + 1;
            }
        }
        return temp;
    }

    public static int[][] buildVoronoiMap(Point2D[] sites, int width, int height) {
        int[][] indexMap = new int[height][width];
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                Point2D newPoint = new Point2D(i, j);
                indexMap[i][j] = findClosestPoint(newPoint, sites);
            }
        }
        return indexMap;
    }

    public static void plotVoronoiMap(Point2D[] sites, Color[] colors, int[][] indexMap) {
        int height = indexMap.length;
        int width = indexMap[0].length;

        StdDraw.setCanvasSize(width, height);
        StdDraw.setXscale(0, width);
        StdDraw.setYscale(0, height);

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                int myIndex = indexMap[i][j];
                StdDraw.setPenColor(colors[myIndex]);
                StdDraw.point(i, j);
            }
        }

        for (int i = 0; i < sites.length; i++) {
            StdDraw.setPenColor(Color.BLACK);
            StdDraw.filledCircle(sites[i].getX(), sites[i].getY(), 3);
        }
    }

    public static void main(String[] args) {
        int width = 200;
        int height = 200;

        int nSites = 5;
        Point2D[] sites = new Point2D[nSites];
        sites[0] = new Point2D(50, 50);
        sites[1] = new Point2D(100, 50);
        sites[2] = new Point2D(50, 100);
        sites[3] = new Point2D(125, 50);
        sites[4] = new Point2D(100, 175);

        Color[] colors = new Color[nSites];
        colors[0] = Color.BLUE;
        colors[1] = Color.GREEN;
        colors[2] = Color.YELLOW;
        colors[3] = Color.ORANGE;
        colors[4] = Color.MAGENTA;

        int[][] indexmap = buildVoronoiMap(sites, width, height);
        plotVoronoiMap(sites, colors, indexmap);

    }

}