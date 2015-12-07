#  node = getNodeSet(gs,'//*[@id="jianjie"]/div[2]/div[3]')
#######################################  新浪手机
library(XML)
library(RCurl)
## crawl the directory pages
nums = c()
i_url_parse<-getURL("http://mobile.sina.com.cn/",encoding="utf-8")
tmp = unlist(strsplit(i_url_parse,split=c('<a href="http://tech.sina.com.cn/mobile/models/')))[2:length(tmp)]
for(i in 1:length(tmp))
{
  num = unlist(strsplit(tmp[i],'\\"'))[1]
  nums = c(nums,num)
}
gpage = c()
for(g in nums)
{
  gpage = c(gpage,paste('http://tech.sina.com.cn/mobile/models/',g,sep=''))
}
## crawl the item details
removeNumbers = function(x) { ret = gsub("[\r \n \t]","",x) }
ginfo = c()
for(g in gpage)
{
  Sys.sleep(1)
  gs1 = htmlParse(g)
  a <- getNodeSet(gs1, path = "//div[@class='main_p01_r_c']")
  b <- sapply(a,xmlValue)
  c <- iconv(b,"utf-8","gbk")
  cc = removeNumbers(c) 
  tres = unlist(strsplit(cc,split = '\n'))[c(2:4,6:12)]
  ginfo = c(ginfo,tres)
}
treasure = matrix(ginfo,ncol = length(gpage))
write.table(t(treasure),'手机基本信息.csv',row.names = F,col.names = F,sep=',')


gs1 = htmlParse('http://www.vmall.com/product/2183.html')
a <- getNodeSet(gs1, path = "//@class ")   #pro-inquire-item clearfix  = 'pro-comment-list'
b <- sapply(a,xmlValue)
c <- iconv(b,"utf-8","gbk")
cc = removeNumbers(c) 
tres = unlist(strsplit(cc,split = '\n'))[c(2:4,6:12)]
ginfo = c(ginfo,tres)


gs1 = htmlParse('http://plus.zealer.com/tag/19')
a <- getNodeSet(gs1, path = "//div[@class = 'inner_right']")   #pro-inquire-item clearfix
b <- sapply(a,xmlValue)
c <- iconv(b,"utf-8","gbk")
cc = removeNumbers(c) 
tres = unlist(strsplit(cc,split = '\n'))[c(2:4,6:12)]
ginfo = c(ginfo,tres)


###########################  zealer
gs2 = getURL('http://plus.zealer.com/tag/19')
tmp = unlist(strsplit(gs2,split=c('<a href="/post/')))[2:length(tmp)]
nums = c()
for(i in 1:length(tmp))
{
  num = unlist(strsplit(tmp[i],'\\"'))[1]
  nums = c(nums,num)
}

setwd('E:/Download')
gpage = c()
for(g in 124:5627)
{
  
  myurl = paste('http://plus.zealer.com/post/',g,sep='')
  if(url.exists(myurl))
  {
    Sys.sleep(3)
    gpage = c(gpage,paste('http://plus.zealer.com/post/',g,sep=''))
    gs1 = try(htmlParse(myurl))
    if(class(gs1)[1] == 'try-error')
      next;
    # content
    a <- getNodeSet(gs1, path = "//div[@class = 'inner_content']")
    b <- sapply(a,xmlValue)
    cc = removeNumbers(b) 
    # title
    a1 <- getNodeSet(gs1, path = "//h1[@class = 'inner_title']")
    b1 <- sapply(a1,xmlValue)
    c1 = removeNumbers(b1)
    # from
    a2 <- getNodeSet(gs1, path = "//div[@class = 'inner_from']")
    b2 <- sapply(a2,xmlValue)
    b2 = removeNumbers(b2)
    # time 
    a3 <- getNodeSet(gs1, path = "//span[@class = 'post_year']")
    b3 <- sapply(a3,xmlValue)
    output = paste(c1,b2,cc,b3,sep='\n')
    #  c <- iconv(b,"utf-8","gbk")
    tmp = try(write.table(output,paste(g,removeLabel(c1)),row.names = F,col.names=F))
    if(class(tmp)=='try-error')
      write.table(output,g,row.names = F,col.names=F)
    print(g)
  }  
}




removeLabel = function(x) { ret = gsub("[/ ： ．|「 」〃 ／ : 、 〔 〕 〈 〉 ＋ ＝ ； ，]","",x) }
tt = removeLabel(c1)
