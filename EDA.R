#역 밀도 파악 + 주변 정류장 및 환경 파악 + 역 승하차 인원 파악

setwd('C:/Users/samsung/Desktop/대학교/4학년 1학기/피셋/선형대수학팀/주제분석/cute_daramg_data')
getwd()

library(tidyverse)
library(data.table)
library(gridExtra)

####################
####역 밀도 파악####
####################
#1. 서울시 내 지하철 주소 파악(행정동)

station_add <- data.frame()
for (i in 1:15){
  a <- fread(paste0('지하철주소/station',i,'.csv'),
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    data.table = FALSE,encoding = 'UTF-8')
  a <- a %>% select(c(2,3,4))
  a<- a[grep("서울특별시", a$지번주소),]
  station_add <- rbind(station_add, a)
  print(i)
}
row.names(station_add) <- NULL
station_add_null <- station_add[c(21,26,86,141,168,192,219,276),]
station_add <-station_add[-c(21,26,86,141,168,192,219,276),]
row.names(station_add) <- NULL
row.names(station_add_null) <- NULL

library(httr)
library(jsonlite)
address_list <- station_add$지번주소
KAKAO_MAP_API_KEY = "137b3fd90faf26739ed67086e95a834c"
행정동 <- c()
경도 <- c()
위도 <- c()
for(i in 1:length(address_list)){

  # 2. API 사용
  # GET함수: GET 프로토콜 형식으로 API 호출
  
  addr2coord_res <- 
    GET(
      # 카카오 주소검색API
      url = 'https://dapi.kakao.com/v2/local/search/address.json', 
      # 검색어
      query = list(query = address_list[i]),
      # 헤더(API마다 다를 수 있다
      add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY)))
  # 3. 결과 가공
  # KPMG 지리정보 데이터프레임
  
  kpmg_list <- addr2coord_res %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  # 검색 결과 중 원하는 인자  추출
  row_temp = kpmg_list$documents$address$region_3depth_h_name
  행정동[i] <- row_temp
  row_temp = kpmg_list$documents$address$x
  경도[i] <- row_temp
  row_temp = kpmg_list$documents$address$y
  위도[i] <- row_temp
  print(i)
  }


result <- cbind(station_add, 행정동,경도,위도)
행정동 <- c(rep(NA,8))
경도 <- c(rep(NA,8))
위도 <- c(rep(NA,8))
station_add_null <- cbind(station_add_null,행정동,경도,위도)
result <- rbind(result,station_add_null )
write.csv(result, "result.csv", row.names = FALSE)

result <- fread('result.csv',
           header = TRUE,
           stringsAsFactors = FALSE,
           data.table = FALSE)

#############################
#행정경계지도


library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)


h <- readOGR('Z_SOP_BND_ADM_DONG_PG/Z_SOP_BND_ADM_DONG_PG.shp')
crs = CRS('+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m')
proj4string(h) <- crs
to_crs = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
map = spTransform(h, to_crs)
map = fortify(map, region = "ADM_DR_CD")
map$id <- as.numeric(map$id)
map = map %>% filter(id <= 1174099)
ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black')



#############################
#2.지하철 승하차 인원 파악
#월별, 년도별 

train<- fread('train_time.csv',
                header = TRUE,
                stringsAsFactors = FALSE,
                data.table = FALSE)

train$사용월 %>% unique #2015.01 - 2020.09
train <- train %>% dplyr::select(c(1,3,10,11,12,13))


train$지하철역[train$지하철역 == "종로3가" ]  <- "종로3가(탑골공원)"
train$지하철역[train$지하철역 == "종로3가" ]  <- "종로3가(탑골공원)"
train$지하철역[train$지하철역 == "교대(법원.검찰청)" ]  <- "교대(법원·검찰청)"
train$지하철역[train$지하철역 == "청량리(지하)" ]  <- "청량리"
train$지하철역[train$지하철역 == "청량리(지상)" ]  <- "청량리"
train$지하철역[train$지하철역 == "양원" ]  <- "양원(서울시북부병원)"
train$지하철역[train$지하철역 == "선정릉" ]  <- "선정릉(한국과학창의재단)"
train$지하철역[train$지하철역 == "복정" ]  <- "복정(동서울대학)"
train$지하철역[train$지하철역 == "마곡나루(서울식물원)" ]  <- "마곡나루"
train$지하철역[train$지하철역 == "동대문역사문화공원(DDP)" ]  <- "동대문역사문화공원"
train$지하철역[train$지하철역 == "미아(서울사이버대학)" ]  <- "미아"
train$지하철역[train$지하철역 == "왕십리(성동구청)" ]  <- "왕십리"
train$지하철역[train$지하철역 == "낙성대(강감찬)" ]  <- "낙성대"
train$지하철역[train$지하철역 == "용마산(용마폭포공원)" ]  <- "용마산"
train$지하철역[train$지하철역 == "이촌" ]  <- "이촌(국립중앙박물관)"
train$지하철역[train$지하철역 == "상봉(시외버스터미널)" ]  <- "상봉"
train$지하철역[train$지하철역 == "성신여대입구(돈암)" ]  <- "성신여대입구"

###2020.09(월별0, 시간대 X)### 결론 : 시간대 차이는 없었고, 승하차 차이는 있음 
train_09 <- train %>% filter(사용월 == 202009 )
train_09$지하철역 %>% unique 
train_09 <- aggregate(train_09[,c("07시-08시 승차인원","07시-08시 하차인원",
                      "08시-09시 승차인원","08시-09시 하차인원")], 
          by = list(train_09$지하철역),sum)

train_09_join <- left_join(result, train_09, by = c('역명'='Group.1'))
names(train_09_join) <- c("선명","station", "지번주소","행정동","경도","위도",
                          "s_seven","h_seven","s_eight","h_eight")
train_09_join <- train_09_join%>% select(c(2,3,4,5,6,7,8,9,10))
train_09_join <-  aggregate(train_09_join[,c("s_seven","h_seven","s_eight","h_eight")], 
                         by = list(train_09_join$station),sum)

names(train_09_join)[1] <- c("station")

#7시 승차기준
boxplot(train_09_join$s_seven)
train_09_join %>% arrange(desc(s_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,s_seven), y = s_seven)) + geom_bar(stat = "identity") +
  coord_flip()

s_seven <- train_09_join %>% arrange(desc(s_seven)) %>% head(20)
s_seven_add <- left_join(s_seven, result[,c(2,5,6)], by = c('station'='역명'))
s_seven_add$위도  <- as.numeric(s_seven_add$위도)

s_seven_dong <- left_join(s_seven, result, by = c('station'='역명') )
s_seven_dong %>% dplyr::select(station,s_seven,h_seven,행정동) %>% distinct() %>%
  ggplot(aes(x=s_seven, y=h_seven, size=3))+
    geom_point(alpha = 0.5) +
    scale_size(range = c(0.5, 20), name ="노인 인구 수") +
    ylab("7시 하차 수") +
    xlab("7시 승차 수") +
    theme(legend.position = "none") +
    geom_text(aes(s_seven, h_seven, label = 행정동), colour = I(alpha("grey", 0.85)), size = 4)
#s_seven_add<- s_seven_add %>% aggregate(s_seven_add[,c("s_seven","h_seven","s_eight","h_eight","경도","위도")], 
#                                        by = list(s_seven_add$station),mean)

a <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                        fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 승차 기준")
#7시 하차기준
boxplot(train_09_join$h_seven)
train_09_join %>% arrange(desc(h_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,h_seven), y = h_seven)) + geom_bar(stat = "identity") +
  coord_flip()
h_seven <- train_09_join %>% arrange(desc(h_seven)) %>% head(20)
h_seven_add <- left_join(h_seven, result[,c(2,5,6)], by = c('station'='역명'))
h_seven_add$위도  <- as.numeric(h_seven_add$위도) 


b <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                        fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 하차 기준")

#8시 승차기준
boxplot(train_09_join$s_eight)
train_09_join %>% arrange(desc(s_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,s_eight), y = s_eight)) + geom_bar(stat = "identity") +
  coord_flip()


s_eight <- train_09_join %>% arrange(desc(s_eight)) %>% head(20)
s_eight_add <- left_join(s_eight, result[,c(2,5,6)], by = c('station'='역명'))
s_eight_add$위도  <- as.numeric(s_eight_add$위도) 


c <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_eight_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("8시 승차 기준")

#8시 하차기준
boxplot(train_09_join$h_eight)
train_09_join %>% arrange(desc(h_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,h_eight), y = h_eight)) + geom_bar(stat = "identity") +
  coord_flip()



h_eight <- train_09_join %>% arrange(desc(h_eight)) %>% head(20)
h_eight_add <- left_join(h_eight, result[,c(2,5,6)], by = c('station'='역명'))
h_eight_add$위도  <- as.numeric(h_eight_add$위도) 


d <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_eight_add , aes(x = 경도, y = 위도, colour ='463333')) +
  ggtitle("8시 하차 기준")

train_map_2020_09<- grid.arrange(a,b,c,d, ncol=2)
train_map_2020_09


###2019.09(월별0, 시간대 X)### 결론 : 시간대 차이는 없었고, 승하차 차이는 있음 
train_1909 <- train %>% filter(사용월 == 201909 )
train_1909$지하철역 %>% unique 
train_1909 <- aggregate(train_1909[,c("07시-08시 승차인원","07시-08시 하차인원",
                                  "08시-09시 승차인원","08시-09시 하차인원")], 
                      by = list(train_1909$지하철역),sum)

train_1909_join <- left_join(result, train_1909, by = c('역명'='Group.1'))
names(train_1909_join) <- c("선명","station", "지번주소","행정동","경도","위도",
                          "s_seven","h_seven","s_eight","h_eight")
train_1909_join <- train_1909_join%>% dplyr::select(c(2,3,4,5,6,7,8,9,10))
train_1909_join <-  aggregate(train_1909_join[,c("s_seven","h_seven","s_eight","h_eight")], 
                            by = list(train_1909_join$station),sum)

names(train_1909_join)[1] <- c("station")

#7시 승차기준
boxplot(train_1909_join$s_seven)
train_1909_join %>% arrange(desc(s_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,s_seven), y = s_seven)) + geom_bar(stat = "identity") +
  coord_flip()

s_seven <- train_1909_join %>% arrange(desc(s_seven)) %>% head(20)
s_seven_add <- left_join(s_seven, result[,c(2,5,6)], by = c('station'='역명'))
s_seven_add$위도  <- as.numeric(s_seven_add$위도)

a_1909 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 승차 기준")
#7시 하차기준
boxplot(train_1909_join$h_seven)
train_1909_join %>% arrange(desc(h_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,h_seven), y = h_seven)) + geom_bar(stat = "identity") +
  coord_flip()
h_seven <- train_1909_join %>% arrange(desc(h_seven)) %>% head(20)
h_seven_add <- left_join(h_seven, result[,c(2,5,6)], by = c('station'='역명'))
h_seven_add$위도  <- as.numeric(h_seven_add$위도) 


b_1909 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 하차 기준")

#8시 승차기준
boxplot(train_1909_join$s_eight)
train_1909_join %>% arrange(desc(s_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,s_eight), y = s_eight)) + geom_bar(stat = "identity") +
  coord_flip()


s_eight <- train_1909_join %>% arrange(desc(s_eight)) %>% head(20)
s_eight_add <- left_join(s_eight, result[,c(2,5,6)], by = c('station'='역명'))
s_eight_add$위도  <- as.numeric(s_eight_add$위도) 


c_1909 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_eight_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("8시 승차 기준")

#8시 하차기준
boxplot(train_1909_join$h_eight)
train_1909_join %>% arrange(desc(h_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,h_eight), y = h_eight)) + geom_bar(stat = "identity") +
  coord_flip()



h_eight <- train_1909_join %>% arrange(desc(h_eight)) %>% head(20)
h_eight_add <- left_join(h_eight, result[,c(2,5,6)], by = c('station'='역명'))
h_eight_add$위도  <- as.numeric(h_eight_add$위도) 


d_1909 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                             fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_eight_add , aes(x = 경도, y = 위도, colour ='463333')) +
  ggtitle("8시 하차 기준")

grid.arrange(a_1909,b_1909,c_1909,d_1909, ncol=2)
train_map_2020_08
train_map_2020_09


###2020.08(월별0, 시간대 X)### 결론 : 시간대 차이는 없었고, 승하차 차이는 있음 
train_08 <- train %>% filter(사용월 == 202008 )
train_08$지하철역 %>% unique 
train_08 <- aggregate(train_08[,c("07시-08시 승차인원","07시-08시 하차인원",
                                  "08시-09시 승차인원","08시-09시 하차인원")], 
                      by = list(train_08$지하철역),sum)

train_08_join <- left_join(result, train_08, by = c('역명'='Group.1'))
names(train_08_join) <- c("선명","station", "지번주소","행정동","경도","위도",
                          "s_seven","h_seven","s_eight","h_eight")
train_08_join <- train_08_join%>% dplyr::select(c(2,3,4,5,6,7,8,9,10))
train_08_join <-  aggregate(train_08_join[,c("s_seven","h_seven","s_eight","h_eight")], 
                            by = list(train_08_join$station),sum)

names(train_08_join)[1] <- c("station")


#7시 승차기준
boxplot(train_08_join$s_seven)
train_08_join %>% arrange(desc(s_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,s_seven), y = s_seven)) + geom_bar(stat = "identity") +
  coord_flip()

s_seven <- train_08_join %>% arrange(desc(s_seven)) %>% head(20)
s_seven_add <- left_join(s_seven, result[,c(2,5,6)], by = c('station'='역명'))
s_seven_add$위도  <- as.numeric(s_seven_add$위도)

a_08 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                                fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 승차 기준")
#7시 하차기준
boxplot(train_08_join$h_seven)
train_08_join %>% arrange(desc(h_seven)) %>% head(20) %>% 
  ggplot(aes(x=reorder(station,h_seven), y = h_seven)) + geom_bar(stat = "identity") +
  coord_flip()
h_seven <- train_08_join %>% arrange(desc(h_seven)) %>% head(20)
h_seven_add <- left_join(h_seven, result[,c(2,5,6)], by = c('station'='역명'))
h_seven_add$위도  <- as.numeric(h_seven_add$위도) 


b_08 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                                fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_seven_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("7시 하차 기준")

#8시 승차기준
boxplot(train_08_join$s_eight)
train_08_join %>% arrange(desc(s_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,s_eight), y = s_eight)) + geom_bar(stat = "identity") +
  coord_flip()


s_eight <- train_08_join %>% arrange(desc(s_eight)) %>% head(20)
s_eight_add <- left_join(s_eight, result[,c(2,5,6)], by = c('station'='역명'))
s_eight_add$위도  <- as.numeric(s_eight_add$위도) 


c_08 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                                fill = '#f4d9c6', colour = 'black')+
  geom_point(data = s_eight_add , aes(x = 경도, y = 위도, colour ='#835858')) +
  ggtitle("8시 승차 기준")

#8시 하차기준
boxplot(train_08_join$h_eight)
train_08_join %>% arrange(desc(h_eight)) %>% head(20)%>% 
  ggplot(aes(x=reorder(station,h_eight), y = h_eight)) + geom_bar(stat = "identity") +
  coord_flip()



h_eight <- train_08_join %>% arrange(desc(h_eight)) %>% head(20)
h_eight_add <- left_join(h_eight, result[,c(2,5,6)], by = c('station'='역명'))
h_eight_add$위도  <- as.numeric(h_eight_add$위도) 


d_08 <- ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                                fill = '#f4d9c6', colour = 'black')+
  geom_point(data = h_eight_add , aes(x = 경도, y = 위도, colour ='463333')) +
  ggtitle("8시 하차 기준")

grid.arrange(a_08,b_08,c_08,d_08, ncol=2)
train_map_2020_08
train_map_2020_09

