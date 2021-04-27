setwd('C:/Users/samsung/Desktop/대학교/4학년 1학기/피셋/선형대수학팀/주제분석/cute_daramg_data')
getwd()
library(data.table)
library(tidyverse)
library(car)
library(lmtest)
######################
#########지도#########
#####################
library(maptools)
library(spdep)
library(classInt)
library(rgdal)
library(gstat)
library(ggplot2)
library(RColorBrewer)

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
#################################
data_s <- fread('버스정류장행정동_승차.csv',
                header = TRUE,
                stringsAsFactors = FALSE,
                data.table = FALSE)
data_s <-data_s %>% filter(data_s$행정동 == '면목제3.8동')

data_s %>% group_by(행정동) %>% summarise(n = mean(승차총승객수평균))

data_h <- fread('버스정류장행정동_하차.csv',
                header = TRUE,
                stringsAsFactors = FALSE,
                data.table = FALSE)
data_h <-data_h %>% filter(data_h$행정동_h == '면목제3.8동')

data_h %>% group_by(행정동_h) %>% summarise(n = mean(하차총승객수평균))
################################
#데이터 준비 
set.seed(1234)
data <- fread('전처리_3.csv',
              header = TRUE,
              stringsAsFactors = FALSE,
              data.table = FALSE)

data[is.na(data)] <- 0

train_add <- fread('train_popl_With_add.csv',
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   data.table = FALSE)
a <- train_add %>% group_by(행정동) %>% summarise(n=n())

a$행정동[167] <- "종로1·2·3·4가동"
a$행정동[168] <- "종로5·6가동"
a[a$행정동 == '상계3.4동',1] <- '상계3·4동'
a[a$행정동 == '상계6.7동',1] <- '상계6·7동' 
a[a$행정동 == '중계2.3동',1] <- '중계2·3동'
a[a$행정동 == '면목3.8동',1] <- '면목3·8동' 
a[a$행정동 == '금호2.3가동',1] <- '금호2.3가동' 

data <- left_join(data,a,by =c("행정동"="행정동"))
data[is.na(data)] <- 0

data<- data[-276,]
row.names(data) <- NULL
#####################################
#버스 승차 수요
data_1 <- data %>% dplyr::select(-c(3,6,14,21,22,27,29,30,36,38,39,41))
lm.fit <- lm(log(승차총승객수평균)~., data = data_1[,-c(1,2)])
summary(lm.fit) #0.2587
outlierTest(lm.fit)  #320
bptest(lm.fit) #등분산성 o
shapiro.test(lm.fit$residuals) #정규성 o
dwt(lm.fit) #자기상관성 x (?)
car::vif(lm.fit)
which(car::vif(lm.fit)>10)
step(lm.fit,direction="both")


#################################
lm.fit_2 <- lm(log(승차총승객수평균)~종사자수 +버스정류장수_h + 하차총승객수평균 + 면적 + 
                 세대 + 세대당인구 + n  , 
               data = data_1[-c(320,256),-c(1,2)])
summary(lm.fit_2) #0.2522
outlierTest(lm.fit_2)  #12 (창신 2동)
bptest(lm.fit_2) #등분산 o 겨우만족..
shapiro.test(lm.fit_2$residuals) #정규성 o
dwt(lm.fit_2) #자기상관성 x (?)
car::vif(lm.fit_2) #다중공선성 x 


#####################
#공간자기상관성 검정#
#####################
h$ADM_DR_CD <- as.character(h$ADM_DR_CD)
my_shp <- h[which(h$ADM_DR_CD <= 1174099), ] ##서울시 shp만 뽑음! 
plot(my_shp, axes = TRUE)

# neighbor 객체 (인접 or not : binary 형태)
my_nb <- poly2nb(my_shp) # 폴리곤으로부터 neighbor 객체 형성 (Queen's neighbor)

# list weight 객체 (공간가중치 행렬 생성),style = 'B':열표준화X , style = "W': 열표준화 O
my_listw <- nb2listw(my_nb, style = "W", zero.policy = TRUE) #row standardize
plot(my_shp, axes = TRUE)
plot(my_listw, coordinates(my_shp), add=TRUE, col='red')
##### Moran's I ####
moran.test(log(data_1$승차총승객수평균), my_listw , zero.policy = TRUE) # 0.12 _공간자기존재 


##### local Moran's I (lisa) #####
a <- localmoran(log(data_1$승차총승객수평균), my_listw, zero.policy = TRUE)
my_i <- a[,1] # local moran 지수
sigma <- sd(my_i, na.rm = TRUE) # local moran 지수의 표준편차
my_break <- c(-Inf, -sigma, 0, sigma, Inf) # 표준편차 기준으로 급간 생성
my_group <- cut(my_i, my_break) # local moran 지수에 급간 적용
my_pal <- brewer.pal(4, "RdPu")
my_color <- my_pal[my_group]
plot(my_shp, axes=  TRUE, col = my_color) #지도지도
moran <- moran.plot(log(data_1$승차총승객수평균), listw = my_listw) #상관관계 있음!

##### local Moran's I (getis ord) 별다섯개!!!! #####
# creates centroid and joins neighbours within 0 and x units
nb <- dnearneigh(coordinates(my_shp),0,2550)
nb_lw <- nb2listw(nb, style = 'B')
plot(my_shp, border = 'lightgrey')
plot(nb, coordinates(my_shp), add=TRUE, col = '#ffa5a5')
local_g <- localG(log(data_1$하차총승객수평균), nb_lw)
sigma <- sd(local_g, na.rm = TRUE) # local moran 지수의 표준편차
my_break <- c(-Inf, -sigma, 0, sigma, Inf) # 표준편차 기준으로 급간 생성
my_group <- cut(local_g, my_break) # local moran 지수에 급간 적용
my_pal <- brewer.pal(4, "Reds")
my_color <- my_pal[my_group]
plot(my_shp, axes=  TRUE, col = my_color)
#plot(nb, coordinates(my_shp), add=TRUE, col = 'lightgrey')
#moran <- moran.plot(log(data_1$하차총승객수평균), listw = my_listw) #상관관계 있음!
moran.test(log(data_1$승차총승객수평균), nb_lw , zero.policy = TRUE) # 0.07_공간자기존재 


######################
#######공간회귀#######
######################

h$ADM_DR_CD <- as.character(h$ADM_DR_CD)
#my_shp <- h[which(h$ADM_DR_CD <= 1174099), ]##서울시 shp만 뽑음! 
my_shp <- h[which(h$ADM_DR_CD <= 1174099 &
                    h$ADM_DR_CD != 1121054 & h$ADM_DR_CD != 1116070), ]

#my_shp_data <- left_join(my_shp, data_1 , by )
plot(my_shp, axes = TRUE)
nb <- dnearneigh(coordinates(my_shp),0,3000)
nb_lw <- nb2listw(nb, style = 'B')

#lm.LMtests(lm.fit_3, nb_lw, zero.policy=NULL, test="all", spChk=NULL, naSubset=TRUE)
#공간회귀적합_등분산성 안만족 
#slx
lm.fit_2_slx = spatialreg::lmSLX(log(승차총승객수평균)~종사자수 +버스정류장수_h + 하차총승객수평균 + 면적 + 
                                    세대 + 세대당인구 + n , 
                                 data = data_1[-c(320,256),-c(1,2)], listw=nb_lw)
summary(lm.fit_3_slx) #0.2468 ㅅㅂ
outlierTest(lm.fit_3_slx)  #364
bptest(lm.fit_3_slx) #등분산성 o 
shapiro.test(lm.fit_3_slx$residuals) #정규성 o
dwt(lm.fit_3_slx) #자기상관성 x 

impacts(lm.fit_3_slx)

a <- predict(lm.fit_3_slx, data_1[-c(320,256), -1],
             listw=nb_lw)
data_predict = cbind(data_1[-c(320,256),],a)
lm.LMtests(lm.fit_2, nb_lw, zero.policy=NULL, test="all", spChk=NULL, naSubset=TRUE) #sem 선택


#sem
lm.fit_2_sem = spatialreg::errorsarlm(log(승차총승객수평균)~종사자수 +버스정류장수_h + 하차총승객수평균 + 면적 + 
                                        세대 + 세대당인구 + n , 
                                      data = data_1[-c(320,256),-c(1,2)], listw=nb_lw)
#세대, 세대당 인구 많고, 지하철 수 적은 지역 -> 승차 수요 높다 !!!!!
summary(lm.fit_2_sem) #종사자수 유의 x
spatialreg::Hausman.test(lm.fit_2_sem)  #ok

a <- predict(lm.fit_2_sem, data_1[-c(320,256),-c(1,2)],
             listw=nb_lw)
data_predict = cbind(data_1[-c(320,256),-c(1,2)],a)



