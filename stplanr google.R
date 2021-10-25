#Data structures and methods for reproducible streetnetwork analysis:  overview and implementations in R

#Work has been done at city level (Newcastle) and Regional level (this code is set up for regional datasets) 
#Travel flow aggregation: Nationally scalable methods for interactive and online visualisation of transport behaviour at the road network level


library(stplanr)
library(sf)
sf_use_s2(FALSE)

#write.csv(Google_Travel, "North_East_Google_OD.csv")

#Set up path and retrieve GoogleKey, set up in seperate document

Path = getwd()
source("Google_Key.R") #This means code can be shared, as key is anonymised in a seperate file

####Set up location files/MSOAs - this will be adjusted with focus
MSOA_CENTROIDS=st_read(dsn = (paste0(Path, "/Official_MSOA_Centroids")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Population_Weighted_Centroids")%>%
  st_transform(4326)%>%st_make_valid()
# In this case, population- weighted centroids have been used - does this change the results?. 

#Import MSOAs - UK geodata portal
#MSOA<- st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
#  st_transform(4326)%>%st_make_valid()


#Make CENTROIDS
#This function turns midpoint coordinates into seperate columns
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
  
}


#Make a destinations variable
DESTS <- MSOA_CENTROIDS %>% sfc_as_cols()

#Get origins - these may be limited by a border, in this case it's Newcastle/Tyne & Wear
#UA = st_read(dsn = (paste0(Path, "/UAs")),layer="Counties_and_Unitary_Authorities_(December_2020)_UK_BFC")%>%
#  st_transform(4326)%>%st_make_valid()
#Newcastle <- UA[UA$CTYUA20NM %in% c("Newcastle upon Tyne","Gateshead","North Tyneside", "South Tyneside",  "Sunderland"), ]
#ORIGS<- DESTS[Newcastle,]

#For regional - see below
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)
# Subset the sf object - remove this if not filtering
Regions <- Regions[Regions$rgn17nm %in% c("North West"), ]
ORIGS<- DESTS[Regions,]
Regions$rgn17nm

#Clean Up files
rm(MSOA,Newcastle,UA,Regions)


#Code to set up destinations - nearest 50 centroids to each MSOA. Dests includes ALL MSOAs to avoid complications of the border
#Finding nearest MSOAs

dist_matrix<-st_distance(ORIGS,DESTS) %>% data.frame() #make a matrix of distances
colnames(dist_matrix) <- DESTS$msoa11nm
rownames(dist_matrix) <- ORIGS$msoa11nm
library(units)
dist_matrix<- drop_units(dist_matrix)
dist_matrix[dist_matrix == 0] <- NA

# find the 50 nearest stations and create new data frame
library(tidyr)
library(dplyr)

#Creates a list of the nearest centroids
near <- dist_matrix %>% 
  mutate(ORIGS=rownames(.)) %>% 
  gather('DESTS','dist',-ORIGS) %>% 
  filter(!is.na(dist)) %>% 
  group_by(ORIGS) %>% 
  arrange(dist) %>% 
  slice(1:25) %>% 
  mutate(dist_rank=1:25)

#make a list of MSOAs
name<-unique(near$ORIGS)
#name<-data.frame(name)
N = length(name)
#201,217,233,235,238,256
#library(lwgeom) 
G_Tran_Results<-data.frame()
for (i in 1:N){ 
try({
    D<-near%>% filter(., ORIGS == name[i], #filter the distance matrix - 1 to select 1 origin, and 2 to only focus on top 25 dests
                    dist_rank == c(1:25)) %>% 
    rename(., msoa11nm = DESTS)%>%  
    left_join(., (DESTS[,c(3,4,5)]), by = "msoa11nm")%>% #Adding MSOA coordinates for destinations - these may fall outside of T&W
    ungroup()%>% #Allows removal of columns used for grouping
    select("lon", "lat")%>%
    as.matrix()#%>% 
    #round(., digits = 3)
  
  O<- ORIGS%>% filter(., msoa11nm == name[i])%>% st_drop_geometry()%>%select("lon", "lat")
  
  r = dist_google(from = O, to = D, 
                  g_units = "metric", 
                  google_api = Google_Key,
                  mode = "transit",
                  arrival_time = strptime("2020-08-27 09:00:00",
                                          format = "%Y-%m-%d %H:%M:%S", tz = "BST"
                  )
  )
  Line_Dist<- 
    near%>% 
    filter(., ORIGS == name[i]) %>% 
    rename(., msoa11nm = DESTS)%>% 
    right_join(., (DESTS[,c(3,4,5)]), by = "msoa11nm")%>%
    drop_na()%>%
    ungroup()
  
  Results <- r %>%  
    mutate(from_addresses = name[i],
           dist_rank = c(1:25))%>%
    rename(., ORIGS = 1) %>%
    left_join(.,Line_Dist)%>%
    select(- c("to_addresses", "currency", "fare", "lon", "lat", "geometry"))%>%
    rename(., DESTS = msoa11nm,
           Google_Dist = distances,
           Line_Dist = dist)
  G_Tran_Results<-rbind(Results, G_Tran_Results)
  
})
}
################# - section below is for 50 nearest MSOAs
#   D<-near%>% filter(., ORIGS == name[i], #filter the distance matrix - 1 to select 1 origin, and 2 to only focus on top 25 dests
#                     dist_rank == c(26:50)) %>% 
#     rename(., msoa11nm = DESTS)%>%  #
#     left_join(., (DESTS[,c(3,7,8)]), by = "msoa11nm")%>% #Adding MSOA coordinates for destinations - these may fall outside of T&W
#     drop_na()%>%
#     ungroup()%>% #Allows removal of columns used for grouping
#     select("lon", "lat")%>%
#     as.matrix()#%>% 
#     #round(., digits = 3)
#   
#   O<- ORIGS%>% filter(., msoa11nm == name[i])%>% st_drop_geometry()%>%select("lon", "lat")
# 
#   r = dist_google(from = O, to = D, 
#                   g_units = "metric", 
#                   google_api = Google_Key, 
#                   mode = "transit",
#                   arrival_time = strptime("2021-08-10 09:00:00",
#                                           format = "%Y-%m-%d %H:%M:%S", tz = "BST")
#   )
#   
#   Line_Dist<- 
#     near%>% 
#     filter(., ORIGS == name[i]) %>% 
#     rename(., msoa11nm = DESTS)%>% 
#     right_join(., (DESTS[,c(3,7,8)]), by = "msoa11nm")%>%
#     drop_na()%>%
#     ungroup()
#   
#   Results <- r %>%  
#     mutate(from_addresses = name[i],
#            dist_rank = c(26:50))%>%
#     rename(., ORIGS = 1) %>%
#     left_join(.,Line_Dist)%>%
#     select(- c("to_addresses", "currency", "fare", "lon", "lat", "geometry"))%>%
#     rename(., DESTS = msoa11nm,
#            Google_Dist = distances,
#            Line_Dist = dist)
#   G_Tran_Results<-rbind(Results, G_Tran_Results)
# })
# }



#Need to sort the NAs - Stage 1: replace transit with walking

#This identifies the OD pairs that have 'NA' google distance, and finds their coordinates
Empty<-G_Tran_Results %>% filter(.,is.na(Google_Dist))%>% 
  merge(MSOA_CENTROIDS,., by.x = "msoa11nm", by.y = "ORIGS") %>%
  select(c("msoa11nm", "geometry", "DESTS", "Line_Dist"))%>%
  sfc_as_cols()%>%
  rename(ORIGS = msoa11nm,
         Orig_lon = lon,
         Orig_lat = lat)%>%
  st_drop_geometry()%>%
  merge(MSOA_CENTROIDS,., by.x = "msoa11nm", by.y = "DESTS")%>%
  sfc_as_cols()%>%
  st_drop_geometry() %>%
  rename(DESTS = msoa11nm,
         Dest_lon = lon,
         Dest_lat = lat)

#Reapply the API code for walking instead of transit
N = nrow(Empty)
Walk = data.frame()
for(i in 1:N){
  r = dist_google(from = c(Empty[i,6],Empty[i,7]),
                  to = c(Empty[i,8],Empty[i,9]),
                  g_units = "metric", 
                  google_api = Google_Key,
                  mode = "walking")
  Walk<- rbind(Walk,r)
}

Walk<- Walk %>% mutate(., ORIGS = Empty[,1],
                       DESTS = Empty[,4])%>%
  right_join(.,Empty) %>%
  rename(., Google_Dist = distances) %>%
  select(c("ORIGS", "DESTS", "Google_Dist", "duration", "Line_Dist")) %>%
  mutate(., Mode = "Walking",
         dist_rank = NA)

#Next - some MSOAs couldnt be found - these will also be tested for walking

Exc=vector()
N=length(name)
for (i in 1:N){
  Z = filter(G_Tran_Results, ORIGS == name[i]) #%>% filter(.,is.na(Google_Dist))
  X = 25 - nrow(Z)
  Exc[i]<- X
}
Exc<- data.frame(cbind(name,Exc))%>%filter(.,Exc!=0)  #This shows how many are missing for each MSOA

#make a list of MSOAs
name<-Exc$name
N = length(name)
Walk_Res<-data.frame()
#Re-try for walking
for (i in 1:N){ 
  try({
    D<-near%>% filter(., ORIGS == name[i], #filter the distance matrix - 1 to select 1 origin, and 2 to only focus on top 25 dests
                      dist_rank == c(1:25)) %>% 
      rename(., msoa11nm = DESTS)%>%  
      left_join(., (DESTS[,c(3,4,5)]), by = "msoa11nm")%>% #Adding MSOA coordinates for destinations - these may fall outside of T&W
      ungroup()%>% #Allows removal of columns used for grouping
      select("lon", "lat")%>%
      as.matrix()#%>% 
    #round(., digits = 3)
    
    O<- ORIGS%>% filter(., msoa11nm == name[i])%>% st_drop_geometry()%>%select("lon", "lat")
    
    r = dist_google(from = O, to = D, 
                    g_units = "metric", 
                    google_api = Google_Key,
                    mode = "walking",
                    
                    )
    Line_Dist<- 
      near%>% 
      filter(., ORIGS == name[i]) %>% 
      rename(., msoa11nm = DESTS)%>% 
      right_join(., (DESTS[,c(3,4,5)]), by = "msoa11nm")%>%
      drop_na()%>%
      ungroup()
    
    Results <- r %>%  
      mutate(from_addresses = name[i],
             dist_rank = c(1:25))%>%
      rename(., ORIGS = 1) %>%
      left_join(.,Line_Dist)%>%
      select(- c("to_addresses", "currency", "fare", "lon", "lat", "geometry"))%>%
      rename(., DESTS = msoa11nm,
             Google_Dist = distances,
             Line_Dist = dist)
    Walk_Res<-rbind(Results, Walk_Res)
    
  })
}
Walk_Res<-Walk_Res%>% mutate(., Mode = "Walking",
                         dist_rank = NA) 

#These are all the MSOAs being accessed by walking
Walk<-rbind(Walk_Res,Walk)


#Final results
Google_Travel = G_Tran_Results%>%mutate(., Mode = "Transit") %>%
  rbind(.,Walk) %>% filter(., !is.na(Google_Dist))
write.csv(Google_Travel, "Place_Google_OD.csv")






