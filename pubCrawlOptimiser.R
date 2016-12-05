library(osmar)
library(pryr)
library(dplyr)
library(ggmap)
library(maptools)
library(rgdal)
library(rgeos)
library(httr)
library(jsonlite)
library(reshape2)
library(tidyr)
library(TSP)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD ALL PUBS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Via http://wiki.openstreetmap.org/wiki/Tag:amenity%3Dpub
#http://overpass-turbo.eu/
pubs <- fromJSON('data/pubs_fromBirminghamToNewcastle.json')

pubs <- pubs$elements

#will need this later
nodez <- pubs$nodes

#tags is attached dataframe; keep only columns, make a single df
pubs <- cbind(pubs[,c(1:4),],pubs$tags)
#pubs <- do.call(cbind, list(pubs[,c(1:4),],pubs$tags, pubs$nodes))

#How many have names? ~15K
table(is.na(pubs$name))

#Keep only those - for any with nodez too
#pubs <- pubs[which(!is.na(pubs$name)),]
#nodez <- nodez[which(!is.na(pubs$name))]

#~~~~~~~~~
#For those that are ways, not nodes, we need their location from the way points
#Use their mean
#Most of them...
#Use nodes list to access each way's location
#Get average location for location of pub
#https://journal.r-project.org/archive/2013-1/eugster-schlesinger.pdf
src <- osmsource_api()

waylatlon <- data.frame(lon = as.numeric(), lat = as.numeric())

#test
#way <- nodez[130]

for(way in nodez) {
  
  #print(class(way))
  if(is.null(way)){
    waylatlon <- rbind(waylatlon,data.frame(lon = NA,lat = NA))
    #rbind(waylatlon,c(0,0))
  } else {
    
    wayNodeLatLon <- data.frame(lon = as.numeric(), lat = as.numeric())
    
    #get coordinates for each of the way points
    for(thisnode in way[[1]]){
      
      #print(thisnode)
      obj <- get_osm(node(thisnode), source = src)
      
      #Sys.sleep(0.1)
      
      wayNodeLatLon <- rbind(wayNodeLatLon,
                             data.frame(lon = obj$nodes$attrs$lon,
                                        lat = obj$nodes$attrs$lat))
      
    }
    
    #Then find their mean
    meanLonLat <- data.frame(lon = mean(wayNodeLatLon$lon),lat = mean(wayNodeLatLon$lat))
    
    waylatlon <- rbind(waylatlon,meanLonLat)
    
  }
  
}

#Should be same size... 
table(pubs$type)
table(is.na(waylatlon$lon))

#drop into pubs, merge both
pubs$waylon <- waylatlon$lon
pubs$waylat <- waylatlon$lat

pubs$lonFinal <- pubs$lon
pubs$latFinal <- pubs$lat

pubs$lonFinal[pubs$type == 'way'] <- pubs$waylon[pubs$type == 'way']
pubs$latFinal[pubs$type == 'way'] <- pubs$waylat[pubs$type == 'way']

#Should all have locations now
table(!is.na(pubs$latFinal))
table(!is.na(pubs$lonFinal))

#drop those we didn't manage to get geocodes for
#pubs <- pubs[!is.na(pubs$latFinal),]

#Keep only those with names
pubs_w_names <- pubs %>% 
  filter(!is.na(name))

#check they all have names
table(!is.na(pubs_w_names$name))

saveRDS(pubs_w_names,'data/pubsWithNames_BirminghamToNewcastle.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAP ALL PUBS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pubs_w_names <- readRDS('data/pubsWithNames_BirminghamToNewcastle.rds')

output <- ggplot() +
  geom_point(data = pubs_w_names, 
             aes(x = lonFinal, y = latFinal, colour = factor(0 + (!is.na(pubs_w_names$real_ale))),
                 shape = factor(0 + (!is.na(pubs_w_names$real_ale)))),
             size = 3) +
  theme(line = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SUBSET PUBS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Useful info on proj4 coordinate ref systems
#http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
#http://gis.stackexchange.com/questions/64654/choosing-the-correct-value-for-proj4string-for-shape-file-reading-in-r-maptools


#First, tell R the pub locations have web mercator projection
pubs_geo <- pubs_w_names
coordinates(pubs_geo) <- ~lonFinal+latFinal

#can now plot that directly...
plot(pubs_geo)

#Currently no projection...
proj4string(pubs_geo)

#So set it to latlon
#http://gis.stackexchange.com/questions/48949/epsg-3857-or-4326-for-googlemaps-openstreetmap-and-leaflet
proj4string(pubs_geo) <- '+init=epsg:4326'

#~~~~~~~~~~~~~~
#Get travel to work areas
ttwas <- readShapePoly('data/England_ttwa_2001/england_ttwa_2001.shp')
proj4string(ttwas)

#Or use readOGR as it'll read in the CRS, if there is one.
ttwas <- readOGR(dsn = 'data/England_ttwa_2001', layer = 'england_ttwa_2001')
proj4string(ttwas)

#Which is British National Grid, though a messy version
#http://spatialreference.org/ref/epsg/27700/
ttwas_latlon <- spTransform(ttwas, CRS('+init=epsg:4326'))

#And...
plot(ttwas_latlon)
points(pubs_geo, col='red')

#~~~~~~~~~~~~~~~
#Now they're the same coordinate system, we can subset one with the other

#A couple of options here. If we just wanted Sheffield...
#(Note the full name of the Sheffield TTWA) ->
#ttwas_latlon@data$NAME[grepl("sheffield",ttwas_latlon@data$NAME,ignore.case = T)]

#Option one: subset as you would any df, but we can use particular polygons
sheffoPubs <- pubs_geo[ttwas_latlon[ttwas_latlon$NAME=='Sheffield & Rotherham',], ]

#Does the red deer have real ale?
sheffoPubs$real_ale[grepl("red deer",sheffoPubs$name, ignore.case = T)]

plot(sheffoPubs)

#Bit too far and wide for a pub crawl. We can select manually as well...
#xy=locator(2,"p") 
#plot(sheffoPubs, xlim=xy$x,ylim=xy$y)

#Get four points to build bounding box
xy=locator(4,"p") 

#http://gis.stackexchange.com/questions/206929/r-create-a-boundingbox-convert-to-polygon-class-and-plot
boundingBox <- Polygon(xy)
boundingBoxPoly <- SpatialPolygons(list(Polygons(list(boundingBox), ID = "a")), proj4string = CRS('+init=epsg:4326'))

sheffoPubs <- sheffoPubs[boundingBoxPoly,]

#Looks better
plot(sheffoPubs)

#save
saveRDS(sheffoPubs,'data/sheffoPubsWithNames.rds')

#~~~~~~~~~~~~~~~~~~~~~~
#On map
map <- get_map("Sheffield", zoom = 13, source = "osm", color = "bw")
mapPoints <- ggmap(map)
#mapPoints

#back to plain ol' df
sheffoPubs <- data.frame(sheffoPubs)

output <- mapPoints +
  geom_point(data = sheffoPubs, 
             aes(x = lonFinal, y = latFinal, colour = factor(0 + (!is.na(sheffoPubs$real_ale))),
                 shape = factor(0 + (!is.na(sheffoPubs$real_ale)))),
             size = 3) +
  theme(line = element_blank(),
        #text = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

output


#~~~~~~~~~~~~~~~~~
#NOTE: we can also find out what TTWAs all the pubs are in using %over%
whereAreAllPubs <- pubs_geo %over% ttwas_latlon

pubs_plusTTWAs <- cbind(data.frame(pubs_geo),whereAreAllPubs) %>% 
  dplyr::select(name,NAME,lonFinal,latFinal)

ggplot() +
  geom_point(data = pubs_plusTTWAs,
             aes(x = lonFinal, y = latFinal, colour = NAME)) +
  #scale_colour_manual(values = palette(rainbow( unique(pubs_plusTTWAs$NAME) %>% length() ))) +
  guides(colour = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PICK MY FAVOURITE PUBS FROM THAT LIST----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Or reload just sheffo pubs
#sheffoPubs <- readRDS('data/sheffoPubsWithNames.rds')

faves <- c(
  "Noah's Ark",
  "Cobden View",
  "The Stag's Head",
  "Fagan's",
  "Porter Brook",
  "Walkley Cottage",
  "Crucible Corner",
  "Lescar Hotel",
  "The Grapes",
  "The York",
  "The Hanover",
  "Union Hotel",
  "The Cremorne",
  "The Sheaf View",
  "The Brothers Arms",
  "Rutland Arms",
  "The Washington",
  "The Shakespeare",
  "The Fat Cat",
  "Kelham Island Tavern",
  "Closed Shop",
  "The White Lion",
  "The Devonshire Cat",
  "The Red Deer",
  "The Bath Hotel",
  "Brown Bear",
  "The Beer Engine",
  "The University Arms",
  "Sheffield Tap"
)

subz <- sheffoPubs[sheffoPubs$name %in% faves,]

#Testing on a smaller subset. That's a lot of routes...
#(~25K for 161 pubs)
#subz <- subz[sample(1:nrow(subz),10),]

#or do them all.
#161^2 = 25921. At one per 0.3 seconds that's a little over two hours
#subz <- pubs_w_names

#pubs_w_names[grepl("red deer",pubs_w_names$name, ignore.case = T),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GET TIMES BETWEEN PUBS ACCOUNTING FOR SHEFFIELD HILLS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#https://developers.google.com/maps/documentation/distance-matrix/intro
#Using without API key, single distance query at a time, not whole matrix
#Base URL, will knock query together below
googleURL <- "https://maps.googleapis.com/maps/api/distancematrix/json"

#google does lat/lon = y/x
pubs_n_locations <- data.frame(
  name = subz$name,
  location = paste0(subz$latFinal,",",subz$lonFinal))

locations <- pubs_n_locations[,2]
pubnames <- pubs_n_locations[,1]

#turn locations into a melted list of pairs
#(This is one way of avoiding the matrix API limits)
locationMatrix <- matrix(nrow=length(locations) , ncol=length(locations))
rownames(locationMatrix) <- locations
colnames(locationMatrix) <- locations

locationPairs <- melt(locationMatrix)

#Do the same with venue names - these will be used below for dcast
pubnameMatrix <- matrix(nrow=length(pubnames) , ncol=length(pubnames))
rownames(pubnameMatrix) <- pubnames
colnames(pubnameMatrix) <- pubnames

pubNamePairs <- melt(pubnameMatrix)

#Sticking google's replies here
results <- matrix(nrow=nrow(locationPairs) , ncol=4)

#loop over pairs, thus only two `elements' per query
#for(i in 1:nrow(locationPairs)) {
for(i in 2421:nrow(locationPairs)) {
  
  #set google distance matrix query
  #See https://developers.google.com/maps/documentation/distancematrix/ for option info
  qry <- paste("origins=", locationPairs$Var1[i],
               "&destinations=", locationPairs$Var2[i] ,
               "&sensor=FALSE",
               "&mode=walking",
               #"&key=",
               #apikey,
               sep=""#no spaces
  )
  
  #Get the JSON
  gimme <- GET(
    googleURL,  
    query = qry
    #If using in Leeds University, obv: comment this out if not, or if using Leeds Uni wifi
    #Use this to see details of proxy connection: c(use_proxy("www-cache.leeds.ac.uk:3128", 8080), verbose())
    #c(use_proxy("www-cache.leeds.ac.uk:3128", 8080))
  )
  
  #http://blog.rstudio.org/2014/03/21/httr-0-3/
  stop_for_status(gimme)
  
  store <- content(gimme)
  
  #Being conservative: 0.3 seconds should hit ~66 elements per 10 seconds
  Sys.sleep(0.3)
  
  if(store$rows[[1]]$elements[[1]]$status=="OK") {
    results[i,1] <- store$rows[[1]]$elements[[1]]$distance$value
    results[i,2] <- store$rows[[1]]$elements[[1]]$duration$value
    results[i,3] <- store$rows[[1]]$elements[[1]]$distance$text
    results[i,4] <- store$rows[[1]]$elements[[1]]$duration$text
  } else {
    results[i,1] <- "xxx"
    results[i,2] <- "xxx"
    results[i,3] <- "xxx"
    results[i,4] <- "xxx"
  }
  
  print(paste0(i,":",pubNamePairs$Var1[i]," -- ",pubNamePairs$Var2[i],
               ", result: ",results[i,2], " seconds"))
  
  
  
}#end for

#save just in case - 841 searches
#saveRDS(results,'data/fave29Pubs_googlematrix.rds')
saveRDS(results,'data/allPubsWithNames_googlematrix_first3697.rds')

#Just verbal description of time and distance
#useresults <- results[,3:4]

#metres and seconds
useresults <- results[,1:2]

#join up locations. One matrix for distance, one for time
distance <- cbind(pubNamePairs, useresults)
distance <- distance[,c(1,2,4)]

distancematrix <- dcast(distance, Var1 ~ Var2, value.var = "1")

time <- cbind(pubNamePairs, useresults)
time <- time[,c(1,2,5)]

timematrix <- dcast(time, Var1 ~ Var2, value.var = "2")

#transpose so origins are columns
#distancematrix <- t(distancematrix)
#timematrix <- t(timematrix)

write.csv(distancematrix, file="data/distresults.csv", row.names = F)
write.csv(timematrix, file="data/timeresults.csv", row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FIND OPTIMAL PUB CRAWL, ASYMMETRIC MATRIX (TIME DEPENDS ON HILLINESS)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#TSP package for travelling salesman.
#We want: asymmetric, cos Sheffield hills means time is different
#Depending on which way you're walking
#And Hamiltonian: one visit per pub.

#https://cran.r-project.org/web/packages/TSP/TSP.pdf
timematrix2 <- apply(timematrix, 2, as.integer)

pathObj <- ATSP(as.matrix(timematrix2[,2:ncol(timematrix)]), labels = subz$name)

n_of_cities(pathObj)
labels(pathObj)

#From TSP vignette
#https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion","arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

tours <- sapply(methods, FUN = function(m) solve_TSP(pathObj, method = m, start = 2L), simplify = F)

dotchart(sort(c(sapply(tours, tour_length))),xlab = "tour length", xlim = c(0, 20000))


#Single tour
#http://stackoverflow.com/questions/29301916/how-to-specify-a-starting-city-using-the-tsp-package-in-r
tour <- solve_TSP(pathObj,method = "farthest_insertion", start = 24L)

tour_length(tour)/3600
as.integer(tour)
subz$name
subz$name[as.integer(tour)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FIND OPTIMAL PUB CRAWL, SYMMETRIC MATRIX (JUST USE DISTANCES)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So no need for google querying, just find distance matrix

#Not the right coordinate system
#We could convert to British National Grid as its demoninated in metres
#distMatrix2 <- dist(sheffoPubs[,c('lonFinal','latFinal')]) %>% as.matrix()

#But...
pubs_geo <- sheffoPubs
pubs_geo <- sheffoPubs[sheffoPubs$name %in% faves,]

coordinates(pubs_geo) <- ~lonFinal+latFinal
proj4string(pubs_geo) <- '+init=epsg:4326'

#Units is KM
#http://finzi.psych.upenn.edu/library/sp/html/spDistsN1.html
distMatrix <- spDists(pubs_geo)

pathObj <- TSP(distMatrix, labels = pubs_geo$name)

pathObj
n_of_cities(pathObj)
labels(pathObj)

#From TSP vignette
#https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion","arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

tours <- sapply(methods, FUN = function(m) solve_TSP(pathObj, method = m, start = 167L), simplify = F)

dotchart(sort(c(sapply(tours, tour_length))),xlab = "tour length", xlim = c(0, 100))

redDeer <- which(grepl("red deer",pubs_geo$name, ignore.case = T))

#Single tour
#http://stackoverflow.com/questions/29301916/how-to-specify-a-starting-city-using-the-tsp-package-in-r
tour <- solve_TSP(pathObj,method = "farthest_insertion", start = redDeer)

tour_length(tour)
as.integer(tour)
pubs_geo$name[as.integer(tour)]

subz <- data.frame(pubs_geo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PLOT PUB CRAWL----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

map <- get_map("Sheffield", zoom = 13, source = "osm", color = "bw")
mapPoints <- ggmap(map)
mapPoints

output <- mapPoints +
  geom_point(data = subz, 
             aes(x = lonFinal, y = latFinal, colour = factor(0 + (!is.na(subz$real_ale))),
                 shape = factor(0 + (!is.na(subz$real_ale)))),
             size = 3) +
  geom_path(data = subz[as.integer(tour),], aes(x = lonFinal,y = latFinal)) +
  geom_point(data = subz[as.integer(tour)[1],], aes(x = lonFinal,y = latFinal),
             colour = "green",size = 5) +
  geom_point(data = subz[as.integer(tour)[length(as.integer(tour))],], aes(x = lonFinal,y = latFinal),
             colour = "red",size = 5) +
  geom_text(data = subz[c(as.integer(tour)[1],as.integer(tour)[length(as.integer(tour))]),],
            aes(label = name, x = lonFinal, y = latFinal), size = 3, colour="black") +
  theme(line = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

output

#ggsave('images/largeCrawl.png',output,dpi=300,width = 30,height = 30)


#xy=locator(2,"p") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CUTTINZ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Sheffield via http://jsfiddle.net/LzezLgtz/9/
#function is long/lat, web form is lat/lon
#Too much data to load over web
#bb <- corner_bbox(-1.5185165405273438,53.354342454080886,-1.4409255981445312,53.39540854114089)
#ua <- get_osm(bb, source = src)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD SHEFFIELD PUBS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Via http://wiki.openstreetmap.org/wiki/Tag:amenity%3Dpub
#http://overpass-turbo.eu/
# pubs <- fromJSON('data/sheffieldPubs_OSM.json')
# 
# pubs <- pubs$elements
# 
# #will need this later
# nodez <- pubs$nodes
# 
# #tags is attached dataframe; keep only columns, make a single df
# pubs <- cbind(pubs[,c(1:4),],pubs$tags)
# #pubs <- do.call(cbind, list(pubs[,c(1:4),],pubs$tags, pubs$nodes))
# 
# #~~~~~~~~~
# #For those that are ways, not nodes, we need their location from the way points
# #Use their mean
# #Most of them...
# #Use nodes list to access each way's location
# #Get average location for location of pub
# #https://journal.r-project.org/archive/2013-1/eugster-schlesinger.pdf
# src <- osmsource_api()
# 
# waylatlon <- data.frame(lon = as.numeric(), lat = as.numeric())
# 
# #test
# way <- nodez[130]
# 
# for(way in nodez) {
#   
#   #print(class(way))
#   if(is.null(way)){
#     waylatlon <- rbind(waylatlon,data.frame(lon = NA,lat = NA))
#     #rbind(waylatlon,c(0,0))
#   } else {
#     
#     wayNodeLatLon <- data.frame(lon = as.numeric(), lat = as.numeric())
#     
#     #get coordinates for each of the way points
#     for(thisnode in way[[1]]){
#       
#       #print(thisnode)
#       
#       obj <- get_osm(node(thisnode), source = src)
#       
#       wayNodeLatLon <- rbind(wayNodeLatLon,
#                              data.frame(lon = obj$nodes$attrs$lon,
#                                         lat = obj$nodes$attrs$lat))
#       
#     }
#     
#     #Then find their mean
#     meanLonLat <- data.frame(lon = mean(wayNodeLatLon$lon),lat = mean(wayNodeLatLon$lat))
#     
#     waylatlon <- rbind(waylatlon,meanLonLat)
#     
#   }
#   
# }
# 
# #Should be same size... yup.
# table(pubs$type)
# table(0 + is.na(waylatlon$lon))
# 
# #drop into pubs, merge both
# pubs$waylon <- waylatlon$lon
# pubs$waylat <- waylatlon$lat
# 
# pubs$lonFinal <- pubs$lon
# pubs$latFinal <- pubs$lat
# 
# pubs$lonFinal[pubs$type == 'way'] <- pubs$waylon[pubs$type == 'way']
# pubs$latFinal[pubs$type == 'way'] <- pubs$waylat[pubs$type == 'way']
# 
# #Should all have locations now
# table(0 + is.na(pubs$latFinal))
# table(0 + is.na(pubs$lonFinal))
# 
# #keep only those we have names for
# #1072 to 161
# pubs_w_names <- pubs %>% 
#   filter(!is.na(name))
# 
# saveRDS(pubs_w_names,'data/pubsWithNames.rds')

#reddeer <- pubs_w_names[grepl("red deer",pubs_w_names$name,ignore.case = T),] 

