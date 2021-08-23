library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

LondonBoroughs <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

library(tidyverse)
Outdoor <- read_csv(here::here("statistical-gis-boundaries-london", "Outdoor 2019 Incident data.csv"))
Outdoor <- Outdoor %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), 
           crs = 4326)%>%
  # project it too - remember that 27700 is
  # British National Grid
  st_transform(., 27700)


summary(Outdoor)

tmap_mode("view")

BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Outdoor) +
  tm_dots(col = "blue")
#remove duplicates

Outdoor <- distinct(Outdoor)

OutdoorSub <- Outdoor[BoroughMap,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(OutdoorSub) +
  tm_dots(col = "blue")


Borough <- as.owin(BoroughMap)
plot(Borough)

OutdoorSub <- OutdoorSub %>%
  as(., 'Spatial')

BluePlaquesSub.ppp <- ppp(x=OutdoorSub@coords[,1],
                          y=OutdoorSub@coords[,2],
                          window=Borough)

BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="London")

BluePlaquesSub.ppp %>%
density(., sigma=1000) %>%
   
plot()



transportation
transportation <- read_csv(here::here("statistical-gis-boundaries-london", "transportation 2019 Incident data.csv"))
transportation <- transportation %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), 
           crs = 4326)%>%
  # project it too - remember that 27700 is
  # British National Grid
  st_transform(., 27700)


tmap_mode("view")

BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(transportation) +
  tm_dots(col = "blue")
#remove duplicates

transportation <- distinct(transportation)

transportationSub <- transportation[BoroughMap,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(transportationSub) +
  tm_dots(col = "blue")


Borough <- as.owin(BoroughMap)
plot(Borough)

transportationSub <- transportationSub %>%
  as(., 'Spatial')

transportationSub.ppp <- ppp(x=transportationSub@coords[,1],
                          y=transportationSub@coords[,2],
                          window=Borough)

transportationSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="London")

transportationSub.ppp %>%
  density(., sigma=1000) %>%
  plot()




Non-Residential
Non-Residential <- read_csv(here::here("statistical-gis-boundaries-london", "Non-Residential 2019 Incident data.csv"))
Non-Residential <- Non-Residential %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), 
           crs = 4326)%>%
  # project it too - remember that 27700 is
  # British National Grid
  st_transform(., 27700)


tmap_mode("view")

BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Non-Residential) +
  tm_dots(col = "blue")
#remove duplicates

Non-Residential <- distinct(Non-Residential)

Non-ResidentialSub <- Non-Residential[BoroughMap,]
#check to see that they've been removed
tmap_mode("view")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Non-ResidentialSub) +
  tm_dots(col = "blue")


Borough <- as.owin(BoroughMap)
plot(Borough)

Non-ResidentialSub <- Non-ResidentialSub %>%
  as(., 'Spatial')

residentialSub.ppp <- ppp(x=Non-ResidentialSub@coords[,1],
                             y=Non-ResidentialSub@coords[,2],
                             window=Borough)

Non-ResidentialSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="London")

Non-ResidentialSub.ppp %>%
  density(., sigma=1000) %>%
  plot()



library(plotly)
library(rgdal)
library(broom)
library(crosstalk)
library(spdep)
library(car)
library(fs)
library(janitor)
library(dplyr)
library(forcats)
library(corrr)

maindata<-read_csv(here::here("statistical-gis-boundaries-london", "data 2.csv"))

is.data.frame(maindata)

library(corrr)

Correlation <- maindata%>%
  dplyr::select(population,
                building,
                traffic,
                London,
                road) %>%
  correlate() 
rplot(Correlation)

Regressiondata2<- maindata%>%
  dplyr::select(AttendanceTime,
                building, 
                traffic,
                London,
                road)

model2 <- Regressiondata2 %>%
  lm(AttendanceTime ~
       building+ traffic+ London + road,
     data=.)

summary(model2)

# test Standard Autocorrelation

model_data <- model2 %>%
  augment(., Regressiondata2)


DW <- durbinWatsonTest(model2)
tidy(DW)


LondonBoroughs <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))

library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)

maindata<-read_csv(here::here("statistical-gis-boundaries-london", "data 2.csv"))

LondonProfiles <- BoroughMap%>%
  left_join(.,
            maindata, 
            by = c("GSS_CODE" = "code"))

coordsW <- LondonProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

Regressiondata3<- LondonProfiles%>%
  dplyr::select(AttendanceTime,
                building, 
                traffic,
                London,
                road)

model3 <- Regressiondata3 %>%
  lm(AttendanceTime ~
       building+ traffic+ London + road,
     data=.)

summary(model3)

LonWardProfiles <- LondonProfiles %>%
  mutate(model3resids = residuals(model3))

LWard_nb <- LonWardProfiles %>%
  poly2nb(., queen=T)

plot(LWard_nb, st_geometry(coordsW), col="red")

plot(LonWardProfiles)

Lward.queens_weight <- LWard_nb %>%
  nb2listw(., style="C")

spatial <- LonWardProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()
spatial



