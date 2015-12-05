package webCrawler;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class Util {
    public static String wget(String urlString, String charset) {
        final int retry = 5;

        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < retry; i++) {
            try {
                String temp;
                URL url = new URL(urlString);
                BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream(), charset));
                while ((temp = in.readLine()) != null) {
                    sb.append(temp);
                    sb.append("\n");
                }
                in.close();
                return sb.toString();
            } catch (Exception e) {
                //e.printStackTrace();
                if (i < retry - 1) {
                    System.out.println(urlString + " - retry - " + i + 1);
                } else {
                    System.out.println(e.getClass() + ": " + urlString);
                }
            }
        }
        return null;
    }

    public static String filter(String s) {
        StringBuilder sb = new StringBuilder();
        boolean isEscapeCharacter = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (isEscapeCharacter) {
                if (c == ';')
                    isEscapeCharacter = false;
            } else {
                if (c == '&') {
                    isEscapeCharacter = true;
                    continue;
                }
                if (c == '\r' || c == '\n' || c == '-' || c == ' ' || c == '\t')
                    continue;
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
