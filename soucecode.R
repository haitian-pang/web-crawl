setwd('D:/BaiduYunDownload/大悦城')
library(Rweibo)


#######################################注册oauth
registerApp(app_name = "yuebao", "147860815", "c352b7f1454591fb329d07cf1a1c441")
listApp('yuebao')
roauth <- createOAuth(app_name = "yuebao", access_name = "rrweibo")
#roauth$login()
roauth$getLimits()

#星巴克地址
#英兰   116.364359,39.926318
#longt = 116.358
#  lati = 39.920


#新中关 116.32217,39.984018
#longt = 116.315611
#lati = 39.97826233831


#西单大悦城中心 116.379245,39.91707
#longt = 116.3725
#lati = 39.911

###########  清华大学第六教学楼
#longt = 116.3299
#lati = 40.00288


############   东直门
#longt = 116.436
#lati = 39.940

############  五道口
#longt = 116.33652
#  lati = 39.9927

##############  三里屯酒吧街

longt = 116.4551
lati = 39.936034

##########根据GPS坐标获取偏移后的坐标  
url3 = 'https://api.weibo.com/2/location/geo/gps_to_offset.json'

##########   根据关键词按地址位置获取POI点的信息  zoom=1:17  q=关键词
url4 = 'https://api.weibo.com/2/place/pois/search.json' 
result = weibo.api(roauth,URL=url4,paramlist = list(keyword='新中关购物广场'),httpmethod='GET')
############获取某地点的地图信息：用于经纬度校准,可视化
url2 = 'https://api.weibo.com/2/location/base/get_map_image.json'
result = weibo.api(roauth,URL=url2
                   ,paramlist=list(
                     center_coordinate = paste(longt,lati,sep=',')
                     , zoom=17
                     ,scale=TRUE
                   ),httpmethod='GET')
result
#############获取某个位置周边的动态

pat = function(x)
{
  tmp = x[[1]]
  
  creatime = tmp$created_at
  id = tmp$id
  text = tmp$text
  sources = tmp$source
  uid = tmp$user$id
  uclass = tmp$user$class
  uname = tmp$user$name
  ulocation = tmp$user$location
  udesc = tmp$user$description
  ugender = tmp$user$gender
  follower = tmp$user$followers_count
  friend = tmp$user$friends_count
  ustatuses = tmp$user$statuses_count
  ucreatime = tmp$user$created_at
  verified = tmp$user$verified
 # pid = tmp$annotations[[1]]$place$poiid
 # plon = tmp$annotations[[1]]$place$lon
 # ptitle = tmp$annotations[[1]]$place$title
 # plat = tmp$annotations[[1]]$place$lat
  distance = tmp$distance
  
  ttres = c(creatime,id,text,sources,uid,uclass,uname,ulocation,udesc,ugender,
           follower,friend,ustatuses,ucreatime,verified,distance)
  return(ttres)
}

roauth$getLimits()
url1 = 'https://api.weibo.com/2/place/nearby_timeline.json'
result = weibo.api(roauth,URL=url1
                   ,paramlist=list(
                     lat=lati,long= longt
                     ,count=49,page=1
                     , starttime='1294321600',endtime='1393493400'
                     ,range=200,offset=1
                   ),httpmethod='GET')
status = result$statuses
tres=pat(status[1])
for( j in 2:length(status))
{
  tmpp = pat(status[j])
  tres = c(tres,tmpp)
}
for( i in 2:1000) #  153:252  missing
{
  result = weibo.api(roauth,URL=url1
                     ,paramlist=list(
                       lat=lati,long= longt
                       ,count=49,page=i
                       , starttime='1294321600',endtime='1393493400'
                       ,range=200,offset=1
                     ),httpmethod='GET')
  
  if(result[1] == "User requests out of rate limit!")
  {
    i
    break
  }
  if(length(result$statuses)>0)
  {
    status = result$statuses
    for( j in 1:length(status))
    {
      tmpp = pat(status[j])
      tres = c(tres,tmpp)
    } 
  }
}

tres[length(tres)-15]

ss = t(matrix(data=tres,nrow = 16))
write.csv(ss,'三里屯200-6.csv')

