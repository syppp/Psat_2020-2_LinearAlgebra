library(spatstat)
library(data.table)
library(tidyverse)
data(sporophores)
setwd('C:/Users/samsung/Desktop/대학교/4학년 1학기/피셋/선형대수학팀/주제분석/cute_daramg_data')
getwd()

data_k_function <- fread('9월기준 노선별 정류장 행정동 승하차인원 데이터.csv',
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  data.table = FALSE)

data_k_function_ars <- data_k_function %>% group_by(ARS.ID) %>% mutate(mean_h = mean(하차총승객평균)) %>%
 mutate(mean_s = mean(승차총승객평균)) %>% dplyr::select(c(4,15,16)) %>% distinct()

library(factoextra)
library(cluster)
set.seed(1234)
cluster_1 <- data_k_function_ars[,c(2,3)]
cluster_1 <- scale(cluster_1) %>% as_tibble()  
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method='wss') 
fviz_nbclust(x = cluster_1, FUNcluster = kmeans, method = "silhouette")

kmeans1 <- kmeans(cluster_1, nstart = 1, iter.max = 40, centers = 7)
fviz_cluster(kmeans1, cluster_1)
kmeans1$cluster
result_cluster_1 <- cbind(kmeans1$cluster, data_k_function_ars)
result_cluster_1 <- as.data.frame(result_cluster_1)
colnames(result_cluster_1) <- c('cluster','ARS.ID','mean_h','mean_s')

#############

result_cluster_1
data_xy<- fread('bus/버스승차좌표.csv',
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         data.table = FALSE)
data_xy <- data_xy %>% dplyr::select(c(3,5,6))
result_cluster_1_xy <- left_join(result_cluster_1, data_xy, by =c('ARS.ID'='버스정류장ARS번호_nu'))
result_cluster_1_xy <- result_cluster_1_xy %>% distinct() #6개..


############
#k-function#
############

k <- result_cluster_1_xy[which(result_cluster_1_xy$cluster %in% c(1)),]
library(spatstat)
library(maptools)
# Read point data
#S  <- readShapePoints("Walmarts.shp")

plot(k$X좌표, k$Y좌표, xlab = "", ylab = "", asp=1,
     yaxt='n', xaxt='n', ann=TRUE, col=2)

SP <- SpatialPoints(k[,c("X좌표", "Y좌표")])
P  <- as(SP, "ppp")
##SP <- as(S, "SpatialPoints")
# Get Cape data
#S2  <- readOGR('Z_SOP_BND_ADM_DONG_PG/Z_SOP_BND_ADM_DONG_PG.shp')
#S2$ADM_DR_CD <- as.character(S2$ADM_DR_CD)
#S2 <- S2[which(S2$ADM_DR_CD <= 1174099), ]

#SP2 <- as(S2, "SpatialPolygons")
#W   <- as(SP2, "owin")
library(spatstat)
library(sp)
library(ggplot2)
library(maptools)
#P$window <- W
plot(P)
K <- Kest(P, correction="Ripley")
n <- 100
sporeK <- envelope(P, fun= Kest, nsim= n, verbose=F)

plot(sporeK)
#plot(K, xlab="d (m)", ylab="K(d)")
sporeL <- envelope(P, fun= Lest, nsim= n, verbose=F)
plot(sporeL)
L <- Lest(P,correction="Ripley")
plot(L, xlab="d (m)", ylab="K(d)")



#########################3
data_k_function %>% colnames()
candi_station <- data_k_function %>% dplyr::select(c(4,6))
candi_s <- left_join(k, candi_station, by = c('ARS.ID' = 'ARS.ID')) 
view(candi_s)
candi_s$정류장명 %>% unique() 
write.csv(candi_s, '승차정류장후보.csv', row.names= FALSE)
