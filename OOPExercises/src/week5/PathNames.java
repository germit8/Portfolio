import java.util.Arrays;

public class PathNames {
    public static void main(String[] args) {
        for (int i = 0; i < args.length; i++) {
            String[] myStrings = splStrings(args[i]);
            System.out.format("File: %s Type: %s [%s]" + "\n", myStrings[1], myStrings[3], myStrings[0]);
        }
    }

    public static String[] splStrings(String s) {
        String[] subs = new String[4];
        subs[0] = s.substring(0, s.lastIndexOf("/") + 1);
        subs[1] = s.substring(s.lastIndexOf("/") + 1);
        if (s.contains(".")) {
            subs[2] = s.substring(s.lastIndexOf("/") + 1, s.indexOf("."));
            subs[3] = s.substring(s.indexOf("."));
        } else {
            subs[2] = s.substring(s.lastIndexOf("/") + 1);
            subs[3] = "";
        }

        return subs;
    }
}
