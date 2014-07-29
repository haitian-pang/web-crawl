
library(Rweibo);
#######################################注册oauth
registerApp(app_name = "yuebao", "14788125", "c3b7f14524591fb329d07cf1a1c441");
listApp('yuebao');
roauth <- createOAuth(app_name = "yuebao", access_name = "rrweibo");



#星巴克地址
#英兰   116.364359,39.926318
#新中关 116.32217,39.984018
#西单大悦城 116.379998,39.916897


#西单大悦城中心 116.379245,39.91707
longt = 116.37354
lati = 39.91125

##########根据GPS坐标获取偏移后的坐标  
url3 = 'https://api.weibo.com/2/location/geo/gps_to_offset.json'

##########   根据关键词按地址位置获取POI点的信息  zoom=1:17  q=关键词
url4 = 'https://api.weibo.com/2/location/pois/search/by_location.json' 

############获取某地点的地图信息：用于经纬度校准,可视化
url2 = 'https://api.weibo.com/2/location/base/get_map_image.json'
result = weibo.api(roauth,URL=url2
                   ,paramlist=list(
                     center_coordinate = paste(longt,lati,sep=',')
                     , zoom=17
                     ,scale=TRUE
                   ),httpmethod='GET')

#############获取某个位置周边的动态
url1 = 'https://api.weibo.com/2/place/nearby_timeline.json'
result = weibo.api(roauth,URL=url1
                   ,paramlist=list(
                     lat=lati,long= longt
                     ,count=49,page=1
                     , starttime='1394321600',endtime='1405867618'
                     ,range=200,offset=1
                   ),httpmethod='GET')
status = result$statuses

##########   获取某位置的动态 poiid=    since_id = max_id =  count= page = 
url5 =  'https://api.weibo.com/2/place/poi_timeline.json'

##########   获取某位置的签到的人的列表 有时间信息 poiid=  count=  page= 
url6 = 'https://api.weibo.com/2/place/pois/users.json'

##########  获取附近地点    long lat range count page 
url7 = 'https://api.weibo.com/2/place/nearby/pois.json'

##########  获取附近发位置微博的人  long lat range page count starttime endtime
url8='https://api.weibo.com/2/place/nearby/users.json'

