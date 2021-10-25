# https://rpubs.com/Hussein-Mahfouz/Cumulative-Accessibility-Measure <- DEVELOPED FROM THIS CODE
#http://rstudio-pubs-static.s3.amazonaws.com/485343_dc9f11a9c9824945be5544369ddc88d4.html

#working in WGS 84 throughout - check all CRS

#In the transport project Wd, set up a file path - Open-Trip-Planner/*your project name*/graphs/default
Path = getwd() #Path to to working directory
#Reading in shapefiles for origins and destinations
library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(tmap)
#library(rgdal)
#Read in 'regions' file and set crs
Regions = sf::st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")%>%sf::st_transform(4326)
# Subset the sf object - remove this if not filtering
Regions <- Regions[Regions$rgn17nm %in% c("North East"), ]

#Y_Area<-st_area(BB)


#Import MSOA file (national)
MSOA<- st_read(dsn = (paste0(Path, "/MSOA")),layer="Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries")%>%
  st_transform(4326)%>%st_make_valid()
#Import Population file & tidy
#Population data from UK.gov
MSOA_Population = read.csv("MSOA_Population.csv") 
MSOA_Population = MSOA_Population[,c(1,2,3,53)]
#merge
MSOA <- merge(MSOA, MSOA_Population, by.x="msoa11nm", by.y="MSOA_Name") #Merge
#Import Employment File (Nomis) Business Register and Employment Survey
MSOA_Emp = read.csv("Employment_Count_2019.csv") 
#merge
MSOA <- merge(MSOA, MSOA_Emp, by.x="msoa11nm", by.y="MSOA_Name") #Merge
#Filter - only include MSOAs that lie within the region of focus
POLYGONS <- MSOA[Regions,]

#Make bounding box for the area of focus - in this case it's the North East
BB <- st_as_sfc(st_bbox(POLYGONS))%>% 
  st_sf()

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
#find centroids then split into two columns
CENTROIDS <- st_centroid(POLYGONS) %>% sfc_as_cols()
CENTROIDS<- st_make_valid(CENTROIDS)
rm(MSOA, MSOA_Emp, MSOA_Population, Regions)

#Now import OSM data
library(osmdata)
#install.packages("osmdata")

#Make openstreetmaps query - adapt how needed, this is set to find roads.
query <- opq(BB)%>%
  add_osm_feature(key = 'highway')  
#Produce/save this query - this is the roads file, downloaded through openstreetmap. 
osmdata_xml(query, filename = paste0(Path, "/Open-Trip-Planner/North_East/graphs/default/North_East.osm"))

#This will make a shape of the query - not essential to run this
Roads <- osmdata_sf(query)

#A useful map to check everything overlaps....

Roads <- Roads$osm_lines

library(rgdal)
#Highways <- spTransform(Highways, CRS("+init=epsg:27700"))
tmap_mode("view")
tm_shape(Regions) +
  tm_polygons()+
tm_shape(CENTROIDS)+
  tm_dots(col = "blue")+
tm_shape(Roads)+
  tm_lines()



#####Introducing Open Trip Planner
library(opentripplanner)

#This will work from the folder that was set up at the start. This folder must include the OSM data and GTFS
#GTFS MUST BE IN ZIP FORMAT
path_data <- file.path("Open-Trip-Planner", "OTP_TEST")
dir.create(path_data)
path_otp <- otp_dl_jar(path_data)


#This is building a graph in the designated file path - it will use the GTFS and OSM map data included in the directory
log <- otp_build_graph(otp = path_otp, dir = path_data, memory = 6000, analyst = TRUE)  

 # Launch OTP and load the graph
otp_setup(otp = path_otp, dir = path_data)

# connect r to otp
otpcon <- otp_connect()


# LOOPING FUNCTION TO GET REACH ISOCHRONE OF EACH HEXAGON CENTROID

# variable with number of hexagons (for looping function)

nrows <- nrow(CENTROIDS)
#nrows<-5
# empty list to store output
reachPolygons<-list()

# get reach polygon for each centroid and store in reachPolygons list
for (i in 1:nrows){
  reachPolygons[[i]] <- otp_isochrone(otpcon = otpcon,
                                      fromPlace = c(CENTROIDS$lon[i], CENTROIDS$lat[i]),
                                      mode = c("WALK", "TRANSIT"),
                                      maxWalkDistance = 1000,
                                      date_time = as.POSIXct(strptime("2021-08-04 09:00", "%Y-%m-%d %H:%M")),
                                      cutoffSec = (60*60)) # Cut offs in seconds - 1 hour here
}
warnings()
#Creates a collection of shapefiles that show the distance that can be travelled from the centroid
#Warnings are 'normal' - these are points too far from the road network

otp_stop(warn=FALSE)
#tmap_mode("view")
#Useful way to check code has worked
tm_shape(Regions)+
  tm_borders()+
  tm_shape(reachPolygons[[6]]) +
  tm_fill(col = "antiquewhite2") +
  tm_borders()

  


####Step 1: "Intersecting Isochrones with Hexagons - how many jobs thes the isochrone interesct with 
library(lwgeom) 


totalJobs <-list()

#Now that we have the isochrones, we need to calculate how many jobs each isochrone intersects with.
#For each intersected MSOA, we get: (area of intersection / total area of MSOA) * jobs in MSOA
#We then sum all the results to get the number of jobs accessible for the hexagon we are querying from
POLYGONS$Area<- st_area(POLYGONS)

for (i in 1:nrows){
  totalJobs[[i]] <- 0    # there are some points that OTP couldn't route from. try() used to assign 0 value to these points
  try({
    totalJobs[[i]] <- reachPolygons[[i]] %>% 
      st_make_valid() %>%   # some geometries are invlid (this prevents an error)
      st_intersection(POLYGONS) %>%          # intersect reachPolygon with POLYGONS
      mutate(int_area = st_area(.) %>% as.numeric()) %>% # add column with intersection area with each MSOA
      mutate(int_jobs = (int_area/Area)*Emp_Count) %>% # add column with the ratio of int_jobs: jobs intersected
      summarise(total_jobs = sum(int_jobs))  # summarize and get the sum of jobs intersected by reachPolygon
  })
}

#Have job reach for hexagon, need to now put this back in the shapefile to calculate/plot results


# add a column for the number of jobs reachable within 60 minutes using ALL MODES of public transport
for (i in 1:nrows){
  POLYGONS$jobs_60_all[i] <- 0    #set default value = O: will be given to bad geometries
  try({
    POLYGONS$jobs_60_all[i] = totalJobs[[i]][[1]] # add totalJobs to new column in MSOA_NE_POL called jobs_60_all
  })
}
POLYGONS$jobs_60_all= round(as.numeric(POLYGONS$jobs_60_all))
hist(POLYGONS$jobs_60_all)
#POLYGONS$jobs_60_all<-format(POLYGONS$jobs_60_all)
####Step 2: Calculate Accessibility Scores

#Now lets get the accessibility score for each hexagon. This is the % of the total jobs in the GCR that are reachable from this hexagon
# percentage of jobs accessible from each hexagon - ALL MODES OF PUBLIC TRANSPORT
POLYGONS$access_per_60_all = ((POLYGONS$jobs_60_all/ POLYGONS$Emp_Count)*100)
#hist(POLYGONS$access_per_60_all)
#hist(POLYGONS$jobs_60_all)
summary(POLYGONS$access_per_60_all)
######Step 3: Scores for whole study area

#Weigh each hexagon by population
#divide sum by total population 

# as.numeric used to return a number instead of a matrix

# Average jobs reached using all modes
GCR_avg_jobs_all <- as.numeric((POLYGONS$jobs_60_all %*% POLYGONS$Age_16_to_64) / sum(POLYGONS$Age_16_to_64))
cat(" Average Job Reach Using All Modes :", GCR_avg_jobs_all, "\n")


# Average accessibility using all Modes (%):
GCR_avg_acc_all <- as.numeric((POLYGONS$access_per_60_all %*% POLYGONS$Age_16_to_64) / sum(POLYGONS$Age_16_to_64))
cat(" Average Accessibility Using All Modes :", GCR_avg_acc_all, "\n")


##Looking at regional level scores - consider this for if scale up?
#Acc_DF <- as.data.frame(MSOA_NE_POL$All_Ages)   #convert sf to dataframe
#Acc_DF %>% group_by(znng_t_) %>% 
#  summarise(score_formal = (access_per_60_formal %*% pop2018cap) / sum(pop2018cap), # formal transit modes
#            jobs_formal  = (jobs_60_formal %*% pop2018cap) / sum(pop2018cap),
#            jobs_all     = (jobs_60_all %*% pop2018cap) / sum(pop2018cap),
#            score_all    = (access_per_60_all %*% pop2018cap) / sum(pop2018cap), # all transit modes
#            inc_abs      = (score_all - score_formal),   # accessibility increase
#            inc_rel      = ((score_all - score_formal)/ score_formal)*100,   # % increase FROM formal modes TO all modes
#            inc_factor   = (score_all/score_formal))    # increase as a multiple





#############Mapping

library(sf)
# to plot metro as line
#cairo_metro <- st_read("Cairo Shapefiles/Metro_Trips_TfC.shp")
# to add text labels for the new towns on the outskirts
#cairo_new_towns <- st_read("Cairo Shapefiles/Central-Inner_Shiyakha_NDC_CAPMAS.shp") %>% 
#  filter(zoning_tfc == "Outer")
# Remove 10th of Ramadan City and Giza_Outer Labels)
#cairo_new_towns <- cairo_new_towns[!duplicated(cairo_new_towns$name_citya),] %>% 
#  filter(!grepl('Giza_Outer|10th of Ramdan', name_citya))


library(tmap)
tmap_mode("plot")

# breaks argument used instead of style
breaks = c(0, 25000,50000,100000,200000, 300000, 400000, 500000) 

NE_OD<- read.csv("NE_Travel_OD.csv") %>% rename(., msoa11cd = X)
Map<-merge(POLYGONS,NE_OD)
Map<-Map[,c(1:14)]

tm_shape(Map) +
  tm_fill("jobs_60_all",
          #style = "jenks",    # used instead of user defined breaks
          breaks = breaks,
          palette = 'GnBu', # for specific colors: c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba', '#253494')
          #legend.hist = TRUE,
          title = "Jobs Within 60 min \n(Public Transit, AM Peak)",
          colorNA = "black") +
  tm_layout(title = "Accessibility Across the North East",        # add a title
            title.size = 1.5,
            title.color = "azure4",
            title.position = c("left", "top"),
            inner.margins = c(0.09, 0.15, 0.10, 0.3),    # increase map margins to make space for legend
            fontfamily = 'Georgia',
            #bg.color = "grey95",
            frame = TRUE) +
  #tm_borders(col = "grey40", lwd = 0.1)+
  tm_legend(title.size=0.9,
            text.size = 0.6,
            #frame = "grey",
            position = c("right", "centre")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) #+
  #tm_shape(cairo_metro) + 
  #tm_lines(col = 'firebrick4', lwd = 1.5, alpha = 0.8) +
  #tm_add_legend(type = "line", labels = 'Cairo Metro', col = 'firebrick4', lwd = 1.5) + 
  #tm_shape(cairo_new_towns) + 
  #tm_text(text = "name_citya", col = 'black', size = 0.7, 
  #        alpha = 0.7, bg.color = "white", bg.alpha = 0.5)



#######################################

Regions = st_read(dsn = (paste0(Path, "/Regions")),layer="Regions_(December_2017)_Boundaries")
# Subset the sf object
Regions <- Regions[Regions$rgn17nm %in% c("North East"), ]
Regions = as(Regions, "Spatial") #changing file type
#North_East <- spTransform(North_East, CRS("+init=epsg:27700"))

UA = st_read(dsn = (paste0(Path, "/UAs")),layer="Counties_and_Unitary_Authorities_(December_2020)_UK_BFC")
Newcastle <- UA[UA$CTYUA20NM %in% c("Newcastle upon Tyne","Gateshead","North Tyneside", "South Tyneside",  "Sunderland"), ]
Newcastle = as(Newcastle, "Spatial") #changing file type
Newcastle <- spTransform(Newcastle, CRS("+init=epsg:27700"))


#Regions = as(Regions, "Spatial") #changing file type




###
otp_pointset(CENTROIDS, "lsoa", path_data)

toPlace   = CENTROIDS[rep(seq(1, nrow(CENTROIDS)), times = nrow(CENTROIDS)),]
fromPlace = CENTROIDS[rep(seq(1, nrow(CENTROIDS)), each  = nrow(CENTROIDS)),]

routes <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace,
                   toPlace = toPlace,
                   fromID = fromPlace$msoa11nm,
                   toID = toPlace$msoa11nm,
                   get_geometry = FALSE)
routes <- routes[,c("fromPlace","toPlace","duration")]
# Use the tidyr package to go from long to wide format
routes_matrix <- tidyr::pivot_wider(routes, 
                                    names_from = "toPlace", 
                                    values_from = "duration") 


