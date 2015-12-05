package webCrawler;

public class File {
    java.io.BufferedWriter out;
    public File(String path) {
        try {
            out = new java.io.BufferedWriter(new java.io.FileWriter(path));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void write(String s) {
        try {
            out.write(s);
            out.newLine();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void flush() {
        try {
            out.flush();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void close() {
        try {
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
