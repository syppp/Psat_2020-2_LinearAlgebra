#구의1동 자양1동 자양2동 광진구 _군자역,건대입구, 잠실역 등등  

setwd('C:/Users/samsung/Desktop/대학교/4학년 1학기/피셋/선형대수학팀/주제분석/cute_daramg_data')
getwd()
library(data.table)
library(tidyverse)
library(car)
library(lmtest)
library(cluster)
library(factoextra)

data_sem <- fread('gajagaja.csv',
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          data.table = FALSE) #승차평균인원 기준 8,10 번째  

data_bus <- fread('9월기준 노선별 정류장 행정동 승하차인원 데이터.csv',
                header = TRUE,
                stringsAsFactors = FALSE,
                data.table = FALSE)


data_bus_ja1 <- data_bus %>% filter(행정동 =='자양1동')  %>% group_by(ARS.ID)%>%
  mutate(승차평균 = mean(승차총승객평균)) %>% mutate(하차평균 = mean(하차총승객평균))#버스수47, 정류장수 13

data_bus_ja1_ars <- data_bus_ja1 %>% select(c(4,15,16)) %>% distinct()

data_bus_ja2 <- data_bus %>% filter(행정동 =='자양2동')%>% group_by(ARS.ID)%>%
  mutate(승차평균 = mean(승차총승객평균)) %>% mutate(하차평균 = mean(하차총승객평균)) #버스수41, 정류장수 14
data_bus_ja2_ars <- data_bus_ja2 %>% select(c(4,15,16)) %>% distinct()

data_bus_gu1 <- data_bus %>% filter(행정동 =='구의1동') %>% group_by(ARS.ID)%>%
  mutate(승차평균 = mean(승차총승객평균)) %>% mutate(하차평균 = mean(하차총승객평균))#버스수47, 정류장수 14
data_bus_gu1_ars <- data_bus_gu1 %>% select(c(4,15,16)) %>% distinct()


data_bus_gw <- rbind(data_bus_ja1_ars,data_bus_ja2_ars, data_bus_gu1_ars)
###########################
#########clustering########
###########################
cluster_1 <- data_bus_gw[,-1]
corrplot::corrplot(cor(cluster_1), method = 'number') 
#cluster_1 = scale(cluster_1[,-1]) %>% as_tibble()  
#Funcluter = pam 
fviz_nbclust(x = cluster_1, FUNcluster = pam, method='wss') 
fviz_nbclust(x = cluster_1 , FUNcluster = pam, method = "silhouette")

pam1 <- pam(cluster_1, 3)
pam1$silinfo$avg.width #0.359..

fviz_cluster(pam1)
result_pam <- cbind(pam1$clustering,preprocess[,2])


#Funcluter = kmeans 
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method='wss') 
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method = "silhouette")
kmeans1 <- kmeans(cluster_1, nstart = 1, iter.max = 15, centers = 3)
fviz_cluster(kmeans1, cluster_1)
kmeans1$cluster
result_cluster <- cbind(kmeans1$cluster,data_bus_gw) #2: 승차 많음(11개)
result_cluster_real <- result_cluster %>% filter(...1 == '1')
result_cluster_real$ARS.ID

result <- data_bus[which(data_bus$ARS.ID %in%  c(5680, 5677, 5668, 5596, 5189, 
                                       5190, 5191, 5193, 5562, 5572, 5679)),c(4,6)] %>%
      distinct()

result_name <-left_join(result, result_cluster_real, by = c('ARS.ID'='ARS.ID'))########찐##########
#41개 중 11개 추출


data_bus_geondae <- data_bus %>% filter(행정동 =='화양동')  %>% group_by(ARS.ID)%>%
  mutate(승차평균 = mean(승차총승객평균)) %>% mutate(하차평균 = mean(하차총승객평균))
data_bus_geondae <- data_bus_geondae %>% select(c(4,15,16)) %>% distinct()
data_bus_geondae_2 <- data_bus[which(data_bus$ARS.ID %in%  c(5180, 5224, 5225, 5226, 5227, 5228,
                                                             5229, 5230, 5231, 5232, 5234,
                                                          5263, 5233, 5571, 5566, 5561, 5556)),c(4,5)]

data_bus_geondae_3 <- left_join(data_bus_geondae, data_bus_geondae_2, by=c('ARS.ID'='ARS.ID')) ########찐##########
result_name_gd <- data_bus_geondae_3 %>% filter(역명 == '건대입구역사거리') %>% dplyr::select(c(1,4))
result_name_gd <- as.data.frame(result_name_gd)
colnames(result_name_gd) <- c('ARS.ID','정류장명')

#최종후보
candi <- rbind(result_name,result_name_gd)
#write.csv(candi, '광진구최종후보.csv',row.names=FALSE)

real <- fread('광진구최종후보.csv',
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  data.table = FALSE)
real <- real[-10,]
#시각화
ggplot() + geom_polygon(data = map, aes(x = long, y = lat, group = group),
                        fill = 'white', colour = 'black')+
  geom_point(data = real , aes(x = x, y = y, colour ='#835858')) +
  ggtitle("광진구 최종 후보 노선 ")+coord_map()

######################
#####최소신장나무#####
######################

# 4. 위경도에서 거리정보 ------

map_dist_df <- real[-1,] %>% dplyr::select(c(2,3,4))


## 4.1. 위경도 - 거리 ------
map_dist_list <- list()
library(Imap)
for (i in 1:nrow(map_dist_df)) {
  
  map_dist_list[[i]] <- gdist(lon.1 = map_dist_df$x[i], 
                              lat.1 = map_dist_df$y[i], 
                              lon.2 = map_dist_df$x, 
                              lat.2 = map_dist_df$y, 
                             )
}

## 4.2. 리스트를 행렬로 변환 ------

map_dist_mat <- sapply(map_dist_list, unlist)

colnames(map_dist_mat) <- map_dist_df[,1]
rownames(map_dist_mat) <- map_dist_df[,1]


map_mst <- ape::mst(map_dist_mat)
plot(map_dist_df$x, map_dist_df$y, xlab = "", ylab = "", asp=1,
     yaxt='n', xaxt='n', ann=TRUE, col=2)
text(map_dist_df$x, map_dist_df$y, labels =map_dist_df$정류장명)
for (i in 1:nrow(map_dist_mat)) {
  w1 <- which(map_mst[i, ] == 1)
  segments(map_dist_df$x[i], map_dist_df$y[i], map_dist_df$x[w1], map_dist_df$y[w1])
}

## 역이름 안겹치게 지도에 쓰기~~
library(ggrepel)
library(gridExtra)
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
gw <- c(1105060,1105053,1105064, 1105065, 1105066, 1105067)

map = map %>% filter(id %in% gw)
#gw <- c(1105053,1105054,1105055,1105056,1105057,1105058,1105059,1105063,1105064,1105065,
#   1105066,1105067,1105060,1105061,1105062)
ggplot() + 
  geom_polygon(data = map, aes(x = long, y = lat, group = group),  fill = '#F8F3E4', colour = '#735C4F') +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_point(data = real[-1,], aes(x =x, y = y,cex=1),size=2, col = '#ffa5a5') +
  geom_text_repel(data= real[-1,], aes(x=x, y=y, label = 정류장명))
#############################
#####네트워크최소신장나무####
#############################
# 1. 데이터 가져오기 ------
library(igraph)
map_dist_mat[lower.tri(map_dist_mat)] <- 0

# 2. 데이터 전처리 ------

map_dist_g <- graph.adjacency(map_dist_mat, weighted=TRUE)
map_dist_df <- get.data.frame(map_dist_g)
DT::datatable(map_dist_df)
# 3. 시각화 -------------

plot(map_dist_g, edge.arrow.size=.1, edge.curved=.1,
     vertex.size=5, vertex.color="orange")
# 2. 최소신장 알고리즘 ------
map_dist_mst_g <- mst(map_dist_g)

# 3. 정적 시각화 -------------

V(map_dist_mst_g)$label.cex <- 0.6

plot(map_dist_mst_g, edge.arrow.size=.1, edge.curved=.1,
     edge.label.font=0.8,
     vertex.size=3, vertex.color=3", asp=0)

# 4. 동적 네트워크 시각화 -------------
## 4.1. 엣지리스트 데이터프레임 변환
map_dist_mst_df <- get.data.frame(map_dist_mst_g)

## 4.2. 노드 마스터
map_dist_node <- unique(c(map_dist_mst_df$from, map_dist_mst_df$to))
map_dist_node <- data.frame(node_nm = map_dist_node, node_val=as.numeric(factor(map_dist_node))-1)

## 4.3. 엣지 데이터프레임
map_dist_mst_df <- left_join(map_dist_mst_df, map_dist_node, by=c("from" = "node_nm")) 
map_dist_mst_df <- left_join(map_dist_mst_df, map_dist_node, by=c("to" = "node_nm"))

el <- data.frame(from=map_dist_mst_df$node_val.x, 
                 to  =map_dist_mst_df$node_val.y)


## 4.4. 시각화
library(networkD3)
forceNetwork(Links = el, Nodes = map_dist_node, Source="from", Target="to",
             NodeID = "node_val", Group = "node_nm", linkWidth = 2,
             fontSize=12, zoom=TRUE, legend=FALSE,
             charge=-300, opacityNoHover = TRUE)
