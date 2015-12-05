





setwd('d:')

wechat = read.csv('qudaojingli.csv',stringsAsFactors=F,sep=',',header=F,col.names=c('姓名','地址','公司','微信号'))
wechat2 = read.csv('jingli1.csv',stringsAsFactors=F,sep=',',header=F,col.names=c('姓名','地址','公司','微信号'))
wdata = rbind(wechat,wechat2)
wdata = unique(wdata)
write.csv(wdata,'聚信托-经理-微信.csv',row.names=F,quote=F,fileEncoding='gb2312')



data1 = read.csv('聚信托-客户经理.csv',stringsAsFactors=F)
data2 = read.csv('聚信托-渠道经理.csv',stringsAsFactors=F)
data = rbind(data1,data2)
data = unique(data)
write.csv(data,'聚信托-经理.csv',row.names=F,quote=F,fileEncoding='gb2312')



result = merge(wdata,data,by = c('姓名','地址','公司'))
write.csv(result,'聚信托.csv',row.names=F,quote=F,fileEncoding='gb2312')

######################################
install.packages('RCurl')
library(XML)
library(RCurl)
library(RJSONIO)
#################################

f_renren_login <- function(name='****', 
                           pwd='****', 
                           cookie_file=NULL){
  require(RCurl)
  if(is.null(cookie_file)){
    
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile='./cookies')
    
    pinfo <- c(
      'email'=name,
      'password'=pwd,
      'origURL'='http://www.renren.com/Home.do',
      'domain'='renren.com'
    )
    
    # µÇÂ¼
    x <- try(ttt <- postForm('http://passport.renren.com/PLogin.do', 
                             .params=pinfo, curl=cH, style='post'), silent=T)
    if(class(x) == 'try-error'){cat('no!!!!!!', '\n');return(NULL)}
  } else{
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile=cookie_file)
  }
  return(cH)
}


# 校内好友关系
f_renren_sns <- function(cH=ch0, topk=3){
  
  require(RCurl)
  require(RJSONIO)
  
  the0url <- 'http://www.renren.com/330813003'
  the0get <- getURL(the0url, curl=ch0, .encoding='UTF-8')
  # write(the0get, 'xxx.txt')
  uid <- gsub('^.*\n\'id\':\'(\\d+)\'.*$', '\\1', the0get)
  # uid
  # "41021031"
  
  a1 <- gsub('^.*var friends=\\[(\\{.+\\})];.*$', '\\1', the0get)
  a1 <- strsplit(a1, '\\},\\{')[[1]]
  a1 <- gsub('\\{|\\}', '', a1)
  a1 <- paste('{', a1, '}', sep='')
  a_id <- unname(sapply(a1, function(x){fromJSON(x)$id}))
  a_name <- unname(sapply(a1, function(x){fromJSON(x)$name}))
  
  my_friends <- data.frame(u0=NA, id0=uid, u1=a_name, id1=as.character(a_id), stringsAsFactors=F)
  my_friends <- unique(my_friends)
  
  # 如果我没有好友 ="=
  if(nrow(my_friends) <= 0){
    cat('no!!!!!!i have no friends!!!!', '\n')
    return(NULL)
  } else{
    # 查找共同好友
    f_share <- function(id, name){
      share_url <- paste('http://friend.renren.com/shareFriends?p={%22init%22:true,%22uid%22:true,%22uhead%22:true,%22uname%22:true,%22group%22:true,%22net%22:true,%22param%22:{%22guest%22:', 
                         id, '}}', sep='')
      h <- getURL(share_url, curl=cH)
      h2 <- fromJSON(h)$candidate
      if(length(h2) > 0){
        newid <- sapply(h2, function(x){x$id})
        cat('#shareFriends with ', name, ' : ', length(newid), '\n')
        return(data.frame(id0=id, id1=newid))
      } else{
        cat('no shareFriends with ', name, '\n')
        return(NULL)
      }
    }
    sns_df <- NULL
    for(index in seq_len(nrow(my_friends))){
      userid <- my_friends$id1[index]
      username <- my_friends$u1[index]
      new_df <- f_share(userid, username)
      sns_df <- unique(rbind(sns_df, new_df))
    }
    sns_df$id0 <- as.character(sns_df$id0)
    sns_df$id1 <- as.character(sns_df$id1)
    
    # 去掉已经注销的账号
    sns_df <- sns_df[sns_df$id1 %in% my_friends$id1 & sns_df$id0 %in% my_friends$id1, ]
    
    require(igraph)
    
    people <- unique(data.frame(id=my_friends$id1, name=my_friends$u1, stringsAsFactors=F))
    people <- people[people$id %in% sns_df$id0, ]
    gg <- graph.data.frame(d=sns_df, directed=F, vertices=people)
    is.simple(gg)
    gg1 <- simplify(gg, remove.loops=T, remove.multiple=T)
    is.simple(gg1)
    dg <- degree(gg1)
    # V(gg1)[dg == 0]
    gg2 <- induced.subgraph(gg1, which(dg > 0))
    
    # 子群划分
    com <- walktrap.community(gg2, steps=4)
    subgroup <- split(com$names, com$membership)
    V(gg2)$sg <- com$membership
    
    # 图形的参数，这个需要设计一下  ="=
    # V(gg2)$degree <- degree(gg2, mode='in')
    V(gg2)$betweenness <- betweenness(gg2)
    top_b <- quantile(V(gg2)$betweenness, (length(V(gg2))-topk)/length(V(gg2)))
    V(gg2)$size <- 2
    V(gg2)$label <- NA
    V(gg2)$labelcex <- 3
    V(gg2)[betweenness>=top_b]$size <- 6
    V(gg2)[betweenness>=top_b]$label <- V(gg2)[betweenness>=top_b]$name
    V(gg2)$vertexcolor <- rainbow(max(V(gg2)$sg))[V(gg2)$sg]
    V(gg2)$framecolor <- rainbow(max(V(gg2)$sg))[V(gg2)$sg]
    
    png(paste('renren_mysns_', Sys.Date(), '.png', sep=''),width=1000,height=1000)
    par(mar=c(0,0,0,0))
    set.seed(17)
    plot(gg2,
         layout=layout.fruchterman.reingold,
         vertex.size=V(gg2)$size,
         vertex.label=V(gg2)$label,
         vertex.label.cex=V(gg2)$labelcex,
         vertex.label.color=1,
         vertex.label.dist=0,
         vertex.color=V(gg2)$vertexcolor,
         vertex.frame.color=V(gg2)$framecolor,
         edge.color=grey(0.8),
         edge.arrow.size=0.5,
         edge.arrow.width=0.5
    )
    dev.off()
    return(list(my_friends=my_friends, sns_df=sns_df))
  }
}
ch0 <- f_renren_login('442546205@qq.com', 'renren93')
renren_sns <- f_renren_sns(cH=ch0, topk=3)
head(renren_sns$sns_df)

####################################模拟登陆
f_juxintuo_login <- function(name='****', 
                           pwd='****', 
                           cookie_file=NULL){
  if(is.null(cookie_file)){
    
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile='./cookies')
   # getURL('http://www.juxintuo.com/Member_production.do?tyy=1',curl = cH)
    pinfo <- c(
      'username'=name,
      'password'=pwd,
      'type' = '1',
      'remember'='TRUE'
     # 'origURL'='http://juxintuo.com/lcsloginForm'
     # 'domain'='juxintuo.com'
    )
    
    
    x <- try(ttt <- postForm('http://www.juxintuo.com/lcslogin.jsp', 
                             .params=pinfo, curl=cH, style='post'), silent=F)
    if(class(x) == 'try-error'){print(cat('no!!!!!!', '\n'));return(NULL)}
  } 
  else{
    d <- debugGatherer()
    cH <- getCurlHandle(followlocation=T, verbose=T, 
                        debugfunction=d$update, 
                        ssl.verifyhost=F, ssl.verifypeer=F, 
                        cookiejar='./cookies', cookiefile=cookie_file)
  }
  return(cH)
}
islogin = f_juxintuo_login(name='aaron10',pwd='60097781')

getURL('http://www.juxintuo.com/mylogin.do?username=aaron10&password=60097781&type=1&remember=true',curl = islogin)
################################################聚信托
guests = c()

sites = 'http://www.juxintuo.com/memberDsfFindOnLoad.action'
for(i in 1:15)
{
  sites = c(sites,paste('http://www.juxintuo.com/memberDsfFind.action?type=0&begin=',i,'&area=0',sep=''))
  
}
#hehe = htmlParse(sites[2],handlers = islogin ,encoding='gb2312')

for(site in sites)
{
  i_url_parse<-getURL('http://www.juxintuo.com/mystore!loadstore.do?accountid=163',curl=islogin,encoding='gb2312')
  
  getNodeSet(i_url_parse,'//*[@id="main"]/div[2]/div[2]/div[2]/div/div[1]/dl[1]/dt[1]/a')
  
  for(i in 1:15)
  {
    xpath = paste('//*[@id="main"]/div[2]/div[2]/div[2]/div/div[',i,']/dl[1]/dt[1]/a',sep='')
    
    node<-getNodeSet(i_url_parse,'//*[@id="main"]/div[2]/div[2]/div[2]/div/div[1]/dl[1]/dt[1]/a')
    if(length(node)==1)
    {
      temp = xmlToList(node[[1]])
      guests = c(guests,temp[2]$.attrs[1])
    }
  }
}
length(guests)
guests = unique(guests)

gpage = c()
for(g in guests)
{
  gpage = c(gpage,paste('http://www.soufoo.com/',g,sep=''))
}
write.csv(gpage,'?Ѹ???????ʦ??????ҳ.csv')
ginfo = c()
for(g in guests)
{
  gs = htmlParse(paste('http://www.soufoo.com/',g,sep=''),encoding='gb2312')
  node = getNodeSet(gs,'//*[@id="gwa"]/div[1]',sessionEncoding='gb2312')
  uname = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[3]')
  mobile = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[5]')
  qq = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[7]')
  email = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="jianjie"]/div[2]/div[3]')
  last_login = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="jianjie"]/div[2]/div[4]')
  online = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="hudong"]/div[1]/div[1]')
  click = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="hudong"]/div[1]/div[2]')
  focus = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  ginfo = c(ginfo,uname,mobile,qq,email,last_login,online,click,focus)
}
treasure = t(matrix(ginfo,nrow=8))
write.csv(treasure,'搜富网理财师.csv')


########################爬到宜人贷第一页的所有投标数???
pieces = c()
urls = "http://www.soufoo.com/list_guwen.php"
for(i in 1:25)
{
  urls =c(urls,paste('http://www.yirendai.com/LenderInvest/applyInfoListPage.action?pager.offset=',i,'0&isJYD=&iapproveNo=&currRate=&iapproveAmt=&productType=',sep='') )
}


url1='http://www.soufoo.com/list_guwen.php'
i_url_parse<-htmlParse(url1,encoding="UTF-8")#
xpath = '/html/body/div[3]/div/div[2]/ul/li[1]/div[2]/div[1]/h3/a'
node<-getNodeSet(i_url_parse,xpath)
text = xmlValue(node[[1]])

for(url1 in urls)
{
  i_url_parse<-htmlParse(url1,encoding="UTF-8")#
  for(i in 2:10)
  {
    
    xpaths = c(xpath_title,xpath_sum,xpath_user,xpath_num,xpath_bids,xpath_term,xpath_per,xpath_bonus,xpath_timeleft)
    one_piece = c()
    for(j in xpaths)
    {
      node<-getNodeSet(i_url_parse,'/html/body/div[3]/div/div[2]/ul/li[1]/div[2]/div[1]/h3')
      text = xmlValue(node[[1]])
      
      pieces = c(pieces,text)
    }
  #  pieces = c(pieces,one_piece)
  }
}
yirendai = t(matrix(pieces,nrow=9))
yirendai = data.frame(yirendai)
yirendai[,1] = gsub(pattern="//n","",yirendai[,1])
yirendai[,1] = gsub(pattern=" ","",yirendai[,1])
yirendai[,1] = gsub(pattern=",","-",yirendai[,1])
colnames(yirendai)<-c('title',"sum",'user','num','bids','term','per','bonus','timeleft')

write.csv(yirendai,'yirendai.csv',quote=F,row.names=F)

url = 'http://www.js808.cn/Lend/t_bdefault.aspx'
url_parse = htmlParse(url,encoding='UTF-8')
xpath = '//*[@id="LoanListDiv"]/div[5]/div[3]/ul/li[1]'
node = getNodeSet(url_parse,xpath)
xmlValue(node[[1]])

sheet = c()
for(i in 6935:9000)
{
  isbill = url.exists(paste('http://www.yirendai.com/loan/view/9',i,sep=''))
  if(isbill){sheet = c(sheet,i)}
}

# Regular HTTP
if(omegahatExists) {
  txt = getURL("http://www.omegahat.org/RCurl/")
  # Then we could parse the result.
  if(require(XML))
   aa =  htmlTreeParse(txt, asText = TRUE)
}



#####################################搜富网
library(XML)
guests = c()

sites = 'http://www.soufoo.com/list_guwen.php'
for(i in 2:268)
{
  sites = c(sites,paste('http://www.soufoo.com/list_guwen.php?province=&city=&district=&sadd=&suser=&od=&order=desc&pn=',i,sep=''))
  
}

for(site in sites)
{
  i_url_parse<-htmlParse(site,encoding="gb2312")#
  for(i in 1:12)
  {
    xpath = paste('//*[@id="result"]/div[2]/div[',i,']/div[2]/div[1]/div[1]/a',sep='')
    
    node<-getNodeSet(i_url_parse,xpath)
    if(length(node)==1)
    {
      temp = xmlToList(node[[1]])
      guests = c(guests,temp[2]$.attrs[1])
    }
  }
}
length(guests)
guests = unique(guests)

gpage = c()
for(g in guests)
{
  gpage = c(gpage,paste('http://www.soufoo.com/',g,sep=''))
}
write.csv(gpage,'?Ѹ???????ʦ??????ҳ.csv')
ginfo = c()
for(g in guests)
{
  gs = htmlParse(paste('http://www.soufoo.com/',g,sep=''),encoding='gb2312')
  node = getNodeSet(gs,'//*[@id="gwa"]/div[1]',sessionEncoding='gb2312')
  uname = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[3]')
  mobile = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[5]')
  qq = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="gwa"]/div[7]')
  email = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="jianjie"]/div[2]/div[3]')
  last_login = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="jianjie"]/div[2]/div[4]')
  online = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="hudong"]/div[1]/div[1]')
  click = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  node = getNodeSet(gs,'//*[@id="hudong"]/div[1]/div[2]')
  focus = unlist(strsplit(xmlValue(node[[1]]),split='???'))[2]
  ginfo = c(ginfo,uname,mobile,qq,email,last_login,online,click,focus)
}
treasure = t(matrix(ginfo,nrow=8))
write.csv(treasure,'搜富网理财师.csv')
#######################################新版宜人贷
library(XML)
nums = c()

sites = c()
for(i in 1:10)
{
  sites = c(sites,paste('http://www.yirendai.com/loan/list/',i,sep=''))
  
}

for(site in sites)
{
  i_url_parse<-getURL(site,encoding="utf-8")#
  tmp = unlist(strsplit(i_url_parse,split=c('<a href="/loan/view/')))[2:11]
  for(i in 1:10)
  {
    num = unlist(strsplit(tmp[i],'">'))[1]
    nums = c(nums,num)
  }
}

gpage = c()
for(g in nums)
{
  gpage = c(gpage,paste('http://www.yirendai.com/loan/view/',g,sep=''))
}
ginfo = c()
for(g in gpage)
{
  Sys.sleep(1)
  gs = getURL(g,encoding='utf-8')
  tmp = unlist(strsplit(gs,split = '<em class=\"l elite_bg png\"></em><strong class=\"l\">'))[2]
  tmp = unlist(strsplit(tmp,'</strong>'))[1]
  sums = unlist(strsplit(gs,split='借款期限（月）'))[2]
  sum = unlist(strsplit(sums,split='</strong>'))
  
  amount = unlist(strsplit(sum[1],split='<strong>'))[2]
  amount = gsub(',|[<span>]|[</span>]','',amount)
  profit = unlist(strsplit(sum[2],split='<strong>'))[2]
  month = unlist(strsplit(sum[3],split='<strong>'))[2]
  perc = unlist(strsplit(sum[4],split='投标完成'))[2]
  
  sum2 = unlist(strsplit(sums,split='</span>'))
  ltime = unlist(strsplit(sum2[2],split='<span>'))[2]
  borrower = unlist(strsplit(sum2[3],split='<span>'))[2]
  
  sum3 = unlist(strsplit(sums,split='还款方式：'))[2]
  rway = unlist(strsplit(sum3,split='</td>'))[1]
  
  sum4 = unlist(strsplit(sums,split='保障计划：'))[2]
  warrant = unlist(strsplit(sum4,split='</td>'))[1]
  
  ginfo = c(ginfo,tmp,amount,profit,month,perc,ltime,borrower,rway,warrant)
}
treasure = t(matrix(ginfo,nrow=9))
treasure = cbind(treasure,gpage)
write.csv(treasure,'d:/宜人贷项目1.csv')
