
library(Rweibo);
library(Rwordseg);
library(wordcloud);
library(RODBC)
setwd("~/R4pht/微博抓取")

#######################################注册oauth
#registerApp(app_name = "yuebao", "1478608125", "c352b7f14524591fb329d07cf1a1c441");
#listApp('yuebao');
#roauth <- createOAuth(app_name = "yuebao", access_name = "rrweibo");
#roauth$login(username = "15201521812",password="");
#res8 <- statuses.update(roauth, status = "123");

#############################################读入微博后的互联网金融热词统计
############确定需要统计的 互联网金融 热词的备选项：每一天的每个词的相关微博的情绪统计和热度分析
#####
kw <- c('余额宝','天弘基金','汇添富','华夏基金','东方基金','活期宝','百发','南方基金','工银瑞信','民生加银','','','')




#############################################读入微博后的词频统计
wfreqcloud <- function(file)
{
  data <- read.csv(file,stringsAsFactors = F);
  data_ft <- data;
  stopword <- read.csv('stopword微博.dic',stringsAsFactors = F);
  #移除数字
  removeNumbers = function(x) { ret = gsub("[0-9０１２３４５６７８９]","",x) }
  #对词进行处理——移除数字
  data_ft$removenumber <- sapply(data_ft$Weibo, removeNumbers);
  #先处理中文分词
  data_ft$seg <- sapply(data_ft$removenumber, segmentCN);
  
  #############################################词频统计和显示
  freq <- function(x)
  {
    length(which(word==x));
  }
  word <- unlist(data_ft$seg);
  wordtable <- unique(unlist(data_ft$seg));
  #去掉单字
  quyi <- function(x)
  {
    if(nchar(x)>1&&length(which(x==stopword))==0)
      return (x)
    else
      return (NULL);
  }
  wordtable <- unlist(sapply(wordtable,quyi));
  #词频
  wordfreq <- sapply(wordtable,freq);
  #降序排列
  od <- order(wordfreq,decreasing=T);
  wordtable = wordtable[od];
  wordfreq = wordfreq[od];
  write.csv(data.frame(wordtable,wordfreq),paste('data/词频_',file,sep = ''));
  #画图并保存成图片
  png(paste('data/词云_',file,'.png',sep = ''), width=6, height=4, units="in", res=400) 
  wordcloud(wordtable[1:60],wordfreq[1:60],max.words =100,random.order = F, colors=brewer.pal(8, "Dark2"))#col = rainbow(length(wordfreq[1:60])),
  #wordcloud(dm$word, dm$freq, random.order=FALSE) 
  dev.off();
}



























#############################################crawl抓微博，并保存,分析
words <- '互联网金融~余额宝~增利宝~百发~百赚~创业~小贷~活期宝~活期通~天天富~微理财';

res11 <- NULL; 

tempdate <- Sys.Date();
i=1;
while(1)
{#
  print(i);
  if(tempdate!=Sys.Date())
  {

    res11 <- NULL; 
    
      file <- paste('data/kw',toString(tempdate),'.txt',sep = '');
    #  wfreqcloud(file);
     
  }
  tempdate <- Sys.Date();

  res11 <- web.search.content(words, page = 1, combinewith = res11 ,sleepmean = 60,sleepsd = 1,since=toString(Sys.Date()));#
  write.csv(res11,file = paste('data/kw',toString(Sys.Date()),'.txt',sep=''));    
}

