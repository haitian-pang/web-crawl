package webCrawler;

import java.util.concurrent.*;

class GrabSingleList extends Thread {
    final static String siteRoot = "http://www.soufoo.com/";
    File file;
    String url;

    public GrabSingleList(String url, File file) {
        this.url = url;
        this.file = file;
    }

    @Override
    public void run() {
        analysePage();
        file.flush();
    }

    private void analysePage() {
        System.out.println(url);
        String content = Util.wget(url, "gb2312");
        int from = 0;
        String guard0 = "<div class='list_user'>";
        String guard1 = "news_guwen.php?user=";
        while (true) {
            int index = content.indexOf(guard0, from);
            if (index < 0)
                break;
            index = content.indexOf(guard1, index + guard0.length());
            String s = content.substring(index, content.indexOf("'", index));

            String urlString = siteRoot + s;
            try {
                analysePerson(urlString);
            } catch (Exception e) {
                System.out.println("exception: " + urlString);
            }
            from = index + guard1.length();
        }
    }

    public void analysePerson(String url) {
        String content = Util.wget(url, "gb2312");
        //String guard0 = "<div class=\"g1\">";
        //String guard1 = "<div class=\"g11\">";
        String guard2 = "<div class=\"g12\">";
        String guardEnd = "</div>";


        String unameTitle = "用户名：";
        String nameTitle = "姓名：";
        String cellTitle = "手机：";
        String phoneTitle = "固话：";
        String qqTitle = "QQ：";
        String emailTitle = "Email：";

        int index = 0;
        index = content.indexOf(unameTitle, index) + unameTitle.length();
        String uname = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(nameTitle, index) + nameTitle.length();
        String name = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(cellTitle, index) + cellTitle.length();
        String cell = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(phoneTitle, index) + phoneTitle.length();
        String phone = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(qqTitle, index) + qqTitle.length();
        String qq = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(emailTitle, index) + emailTitle.length(); //
        String email = content.substring(index, content.indexOf(guardEnd, index));

        index = content.indexOf(guard2, index) + guard2.length(); //
        String address = content.substring(index, content.indexOf(guardEnd, index));
        index = content.indexOf(guard2, index) + guard2.length();
        index = content.indexOf(guard2, index) + guard2.length();
        String company = content.substring(index, content.indexOf(guardEnd, index));

        String message = Util.filter(uname)
                + "," + Util.filter(name)
                + "," + Util.filter(cell)
                + "," + Util.filter(phone)
                + "," + Util.filter(qq)
                + "," + Util.filter(email)
                + "," + Util.filter(address)
                + "," + Util.filter(company);
        //System.out.println(message);
        file.write(message);
    }
}

public class GrabSoufoo {
    File file;
    public void start(String savePath) {
        file = new File(savePath);
        ExecutorService pool = Executors.newFixedThreadPool(5);
        String s = GrabSingleList.siteRoot + "list_guwen.php?pn=";
        int begin = 1;
        int end = 269;

        //analysePage(s + 1);
        for (int i = begin; i <= end; i++) {
            GrabSingleList list = new GrabSingleList(s + i, file);
            pool.execute(list);
            //file.flush();
        }
        System.out.println("waiting.. end");
    }


    public static void main(String[] args) {
        GrabSoufoo g = new GrabSoufoo();
        g.start("e:/soufoo.txt");
        //g.analysePerson("http://www.soufoo.com/news_guwen.php?user=karenbqy");
    }
}
