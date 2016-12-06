#Spatial dependence library
library(spdep)

#process wards (2001 casweb with population)
wards_w_pop <- readOGR(dsn = 'data/xxxwardsWPop2001', layer = 'xxx2001wardsWPop')

#All pubs
pubs_w_names <- readRDS('data/pubsWithNames_BirminghamToNewcastle.rds')

pubs_geo <- pubs_w_names %>% dplyr::select(name,real_ale,latFinal,lonFinal)

coordinates(pubs_geo) <- ~lonFinal+latFinal

proj4string(pubs_geo) <- '+init=epsg:4326'

proj4string(wards_w_pop)

wards_w_pop <- spTransform(wards_w_pop, CRS('+init=epsg:4326'))

plot(wards_w_pop)

points(pubs_geo, col='green')

#Keep only wards containing positive number of pubs
wards_sub <- wards_w_pop[pubs_geo,]

plot(wards_sub)
points(pubs_geo, col='green')

#Keep only pubs within the new subset of wards
pubs_sub <- pubs_geo[wards_sub,]

plot(wards_sub)
points(pubs_sub, col='green')

#save those subsets. Can load again via RDS, right?
saveRDS(wards_sub,'data/wardsub.rds')
saveRDS(pubs_sub,'data/pubsub.rds')


#~~~~~~~~~~~~~~~~~
#contiguity matrix for subset of wards

#Reload subsetted wards and pubs
wards_sub <- readRDS('data/wardsub.rds')
pubs_sub <- readRDS('data/pubsub.rds')

contig <- poly2nb(wards_sub, row.names = wards_sub@data$ZONE_CODE)
#Queen contiguity option
#contig <- poly2nb(wards_sub, row.names = wards_sub@data$ZONE_CODE,queen = T)

mx <- nb2mat(contig,zero.policy = T)

#OK. Weights add to 1 over rows.
apply(mx,1,sum)

#should all sum to one
mx %*% rep(1,nrow(mx)) %>% table

#~~~~~~~~~~
#So find number of real ale pubs per head of pop
zonesForPubs <- pubs_sub %over% wards_sub

pubs_sub <- cbind(pubs_sub@data,zonesForPubs$ZONE_CODE)

#Change real ale into a flag
pubs_sub$realAleFlag <- ifelse(is.na(pubs_sub$real_ale),0,1)

#summarise count of real ale pubs per ward
realAlePubsPerWard <- pubs_sub %>% group_by(`zonesForPubs$ZONE_CODE`) %>% 
  summarise(realAlePubs = sum(realAleFlag==1))

wards_n_pubs <- wards_sub

wards_n_pubs@data <- merge(wards_n_pubs@data,realAlePubsPerWard,
                           by.x = 'ZONE_CODE', 
                           by.y = 'zonesForPubs$ZONE_CODE')

saveRDS(wards_n_pubs,"data/wards_n_pubs_realAleCountShpfile.rds")

#~~~~~~~~~~~~~~~
#real ale pubs per head of pop
wards_n_pubs <- readRDS("data/wards_n_pubs_realAleCountShpfile.rds")

wards_n_pubs@data$realAlePerHead <- wards_n_pubs@data$realAlePubs / wards_n_pubs@data$KS0010001

contigAvs <- mx %*% wards_n_pubs@data$realAlePerHead

zonePlusContig <- cbind(wards_n_pubs@data$realAlePerHead, contigAvs) %>% 
  data.frame

names(zonePlusContig) <- c('realAlePubsPerHead','averageRealAlePubsNeighbourZones')

plot(log(zonePlusContig))

ggplot() +
  geom_point(data = zonePlusContig, aes(x = averageRealAlePubsNeighbourZones * 1000, y = realAlePubsPerHead * 1000)) +
  scale_y_log10()

#~~~~~~~~~~~~~~~~~~~
#Or x nearest neighbours
#This converts wards to their centroids - will find nearest neighs to those points
coords <- coordinates(wards_sub)

#k nearest neighbours. Let's try 16
contig_nk <- knn2nb(knearneigh(coords,k=16), row.names = wards_sub@data$ZONE_CODE)

#plot(contig_nk,coords,col='red',lwd=2,add=TRUE)

mx2 <- nb2mat(contig_nk,zero.policy = T)

#~~~
contigAvsNN <- mx2 %*% wards_n_pubs@data$realAlePerHead

zonePlusContigNN <- cbind(wards_n_pubs@data$realAlePerHead, contigAvsNN) %>% 
  data.frame

names(zonePlusContigNN) <- c('realAlePubsPerHead','averageRealAlePubsNeighbourZones')

#plot(log(zonePlusContigNN))

ggplot() +
  geom_point(data = zonePlusContigNN, aes(x = averageRealAlePubsNeighbourZones * 1000, y = realAlePubsPerHead * 1000)) +
  scale_y_log10()
