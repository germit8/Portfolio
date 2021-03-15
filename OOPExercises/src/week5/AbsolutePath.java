import java.util.Arrays;

public class AbsolutePath {

    public static void main(String[] args) {
        System.out.println(ensureAbsolute(args[0]));
        System.out.println(Arrays.toString(absoluteSplitPath(args[0])));
    }

    public static String ensureAbsolute(String path) {
        if (path.indexOf("/") == 0) {
            return path;
        } else {
            return System.getProperty("user.dir") + "/" + path;
        }
    }

    public static String[] absoluteSplitPath(String s) {
        String[] components = new String[5];
        String[] theRest = PathNames.splStrings(s);
        components[0] = ensureAbsolute(s);

        for (int i = 1; i < components.length; i++) {
            components[i] = theRest[i-1];
        }

        return components;
    }
}