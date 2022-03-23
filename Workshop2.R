## ----install-packages----------------------------------------------------
## install.packages(c("tidycensus", "tidyverse", "terra",
##                    "tmap", "mapview", "rosm", "crsuggest"))


## ----basic-usage-----------------------------------------------------------------------
library(tigris)

tx_counties <- counties(state = "TX")


## ----show-tx-counties------------------------------------------------------------------
tx_counties


## ----basic-plot------------------------------------------------------------------------
plot(tx_counties$geometry)


## ----travis-tracts---------------------------------------------------------------------
travis_tracts <- tracts(state = "TX", county = "Travis")

plot(travis_tracts$geometry)

imperial_blocks <- blocks(state = "CA", county = "Imperial")
plot(imperial_blocks$geometry)

## ----travis-roads----------------------------------------------------------------------
travis_roads <- roads(state = "TX", county = "Travis")

plot(travis_roads$geometry)


## ----dc-landmarks----------------------------------------------------------------------
dc_landmarks <- landmarks("DC", type = "point")

plot(dc_landmarks$geometry)


## ----michigan-tiger--------------------------------------------------------------------
mi_counties <- counties("MI")

plot(mi_counties$geometry)


## ----michigan-cb-----------------------------------------------------------------------
mi_counties_cb <- counties("MI", cb = TRUE)

plot(mi_counties_cb$geometry)


## ----get-yearly-data-------------------------------------------------------------------
tarrant90 <- tracts("TX", "Tarrant", cb = TRUE, year = 1990)
tarrant00 <- tracts("TX", "Tarrant", cb = TRUE, year = 2000)
tarrant10 <- tracts("TX", "Tarrant", cb = TRUE, year = 2010)
tarrant20 <- tracts("TX", "Tarrant", cb = TRUE, year = 2020)



## ----plot-yearly-data------------------------------------------------------------------
par(mfrow = c(2, 2))

plot(tarrant90$geometry, main = "1990")
plot(tarrant00$geometry, main = "2000")
plot(tarrant10$geometry, main = "2010")
plot(tarrant20$geometry, main = "2020")




## ----mapview-------------------------------------------------------------
library(mapview)
## 
## mapview(tarrant20)


## ----sync----------------------------------------------------------------
## library(leafsync)
## 
## sync(mapview(tarrant90), mapview(tarrant20))
## 


## ----all-tracts------------------------------------------------------------------------
us_tracts <- tracts(cb = TRUE)


## --------------------------------------------------------------------------------------
us_tracts


## ----tidycensus-geometry---------------------------------------------------------------
library(tidycensus)
options(tigris_use_cache = TRUE)

tx_population <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  state = "TX",
  year = 2020,
  geometry = TRUE #<<
) 



## ----show-geometry---------------------------------------------------------------------
tx_population


## ----plot-geometry---------------------------------------------------------------------
plot(tx_population["value"])


## ----geom-sf---------------------------------------------------------------------------
library(tidyverse)

tx_map <- ggplot(tx_population, aes(fill = value)) + 
  geom_sf()

MedianIncome_ImperialACS <- get_acs(geography = "block group",
                                    variables = "B19013_001",
                                    state = "California",
                                    county = "Imperial",
                                    year = 2020,
                                    geometry = FALSE, #TRUE
                                    key = "de659383d010f1cea444bd5ab3db1797f51a0458")

MedianIncome_ImperialACS

write.csv(MedianIncome_ImperialACS, "MedianIncome_ImperialACS.csv", row.names = FALSE)

MedianIncome_ImperialACS %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

## ----plot-geom-sf----------------------------------------------------------------------
tx_map


## ----get-hennepin-data-----------------------------------------------------------------
hennepin_race <- get_decennial(
  geography = "tract",
  state = "MN",
  county = "Hennepin",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))


## ----glimpse-hennepin-data-------------------------------------------------------------
dplyr::glimpse(hennepin_race)

imperial_race <- get_decennial(
  geography = "block",
  state = "CA",
  county = "Imperial",
  variables = c(Hispanic = "P2_002N", White = "P2_005N", Black = "P2_006N",
                Native = "P2_007N"),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))
# %>%
#   filter(value != 0)
glimpse(imperial_race)

## ----polygons-map--------------------------------------------------------
library(tmap)

hennepin_black <- filter(hennepin_race, 
                         variable == "Black")

tm_shape(hennepin_black) + 
  tm_polygons() 


imperial_poc <- imperial_race%>%
  group_by(GEOID)%>%
  filter(variable == "White")%>%
  summarize(POC = (summary_value - value), summary_value = summary_value)%>%
  gather(key = "variable", value = "value", 2)%>%
  mutate(percTest = 100 * (value / summary_value),
         percent = ifelse(is.nan(percTest), 0, percTest))

tm_shape(imperial_poc)+
  tm_polygons()

## ----choropleth-show-----------------------------------------------------
tm_shape(hennepin_black) + 
  tm_polygons(col = "percent")

tm_shape(imperial_poc)+
  tm_polygons(col = "percent", title = "2020 US Census")+
  tm_layout(title = "Percent POC\nby Census Block", frame = FALSE,
            legend.outside = TRUE)


## ----custom-choropleth-show----------------------------------------------
tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "2020 US Census") + 
  tm_layout(title = "Percent Black\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)



## ----jenks-show----------------------------------------------------------
tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "jenks",
              n = 7,
              palette = "viridis",
              title = "2020 US Census",
              legend.hist = TRUE) + 
  tm_layout(title = "Percent Black population\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)


## ----bubbles-code----------------------------------------------------------------------
symbol_map <- tm_shape(hennepin_black) + 
  tm_polygons() + 
  tm_bubbles(size = "value", alpha = 0.5, 
             col = "navy") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom")


## ----bubbles-map-----------------------------------------------------------------------
symbol_map


## ----facet-map-code--------------------------------------------------------------------
facet_map <- tm_shape(hennepin_race) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 7,
          palette = "Blues") + 
  tm_layout(legend.position = c(-0.7, 0.15)) #<<


## ----facet-map-------------------------------------------------------------------------
facet_map


## ----dot-density-map-------------------------------------------------------------------
hennepin_dots <- hennepin_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 250,
    group = "variable"
  )

hennepin_base <- hennepin_race %>%
  distinct(GEOID, .keep_all = TRUE)

dot_map <- tm_shape(hennepin_base) + 
  tm_polygons(col = "white") + 
  tm_shape(hennepin_dots) + 
  tm_dots(col = "variable", 
          palette = "Set1", 
          size = 0.01) + 
  tm_layout(legend.outside = TRUE)


## ----dot-density-map-show--------------------------------------------------------------
dot_map


## ----get-basemap, results = "hide"-----------------------------------------------------
library(rosm)

basemap <- osm.raster(
  st_bbox(hennepin_black), 
  zoom = 10,
  type = "cartolight",
  crop = TRUE
)

imperial_poc$geometry <- st_cast(imperial_poc$geometry, "MULTIPOLYGON")

basemap <- osm.raster(
  st_bbox(imperial_poc),
  zoom = 10,
  type = "cartolight",
  crop = TRUE
)

## ----show-basemap----------------------------------------------------------------------
tm_shape(basemap) + 
  tm_rgb()



## ----map-with-basemap-show-----------------------------------------------
tm_shape(basemap) + 
  tm_rgb() + 
  tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "2020 US Census", 
              alpha = 0.6) + #<< 
  tm_layout(title = "Percent Black\nby Census tract",
            legend.outside = TRUE)

tm_shape(basemap) + 
  tm_rgb()+
  tm_shape(imperial_poc)+
  tm_polygons(col = "percent", title = "2020 US Census",
              alpha = 0.4)+
  tm_layout(title = "Percent POC\nby Census Block", frame = FALSE,
            legend.outside = TRUE)+ 
  tm_scale_bar(position = c("left", "BOTTOM"))+ 
  tm_compass(position = c("right", "top"))
# tm_shape(basemap) + 
#   tm_rgb()+
#   tm_shape(imperial_poc%>%
#              filter(!is.nan(percent)))+
#   tm_polygons(col = "percent", title = "2020 US Census")+
#   tm_layout(title = "Percent POC\nby Census Block", frame = FALSE,
#             legend.outside = TRUE)

## ----cartographic-elements-show------------------------------------------
tm_shape(basemap) + 
  tm_rgb() + 
  tm_shape(hennepin_black) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "2020 US Census", 
              alpha = 0.6) + #<< 
  tm_layout(title = "Percent Black\nby Census tract",
            legend.outside = TRUE) + 
  tm_scale_bar(position = c("left", "BOTTOM")) + 
  tm_compass(position = c("right", "top")) + 
  tm_credits("Basemap (c) CARTO, OSM", 
             bg.color = "white",
             position = c("RIGHT", "BOTTOM"), 
             bg.alpha = 0,
             align = "right")


## ----write-shp-----------------------------------------------------------
library(sf)
## 
## st_write(tx_population, "data/tx_population.shp")


## ----interactive-tmap-1--------------------------------------------------
tmap_mode("view")
# tmap_mode("plot")

tm_shape(hennepin_black) +
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "Percent Black<br/>by Census tract",
              alpha = 0.6)

tm_shape(imperial_poc)+
  tm_polygons(col = "percent",
              title = "Percent POC <br>by Census Block",
              alpha = 0.4)


## ----interactive-tmap-2--------------------------------------------------
tmap_options(basemaps = c("Esri.WorldTopoMap", "Stamen.TonerLite", #<<
                          "CartoDB.DarkMatter"),
             check.and.fix = TRUE) #<<

tm_shape(hennepin_black) +
  tm_polygons(col = "percent",
              style = "quantile",
              n = 7,
              palette = "Purples",
              title = "Percent Black<br/>by Census tract",
              alpha = 0.6,
              id = "NAME") #<<

imperial_erase <- imperial_poc%>%
  erase_water(area_threshold = 0.9)

tmap_obj <- tm_shape(imperial_erase)+
  tm_polygons(col = "percent",
              title = "Percent POC <br>by Census Block",
              alpha = 0.4,
              id = "NAME")

## ----save-tmap-----------------------------------------------------------
# tmap_save(tmap_obj, "hennepin_black.html")
tmap_save(tmap_obj, "imperial_poc.html")

## ----mapview-1-----------------------------------------------------------
mapview(hennepin_black, zcol = "percent")
mapview(imperial_poc, zcol = "percent")

##----pop-density-----------------------------------------------------------
tmap_mode("plot")

library(crsuggest)
# View the mi_crs object and pick a suitable CRS for the state
mi_crs <- suggest_crs(mi_density)
# Use that CRS for your map
mi_density_map <- tm_shape(mi_density, projection = 6497) + 
  tm_polygons(col = "density", style = "cont",
              palette = "Blues", title = "People/km2")

mi_density_map


## ----national-data---------------------------------------------------------------------
us_percent_hispanic <- get_decennial(
  geography = "county",
  variables = "P2_002N",
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))


## ----plot-national-map-----------------------------------------------------------------
tmap_mode("plot")

national_map <- tm_shape(us_percent_hispanic) + 
  tm_polygons(col = "percent", palette = "plasma", 
              lwd = 0.05, style = "cont",
              legend.is.portrait = FALSE,
              title = "% Hispanic, 2020 Census") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom")


## ----show-national-map-----------------------------------------------------------------
national_map


## ----rescaled-map----------------------------------------------------------------------
us_rescaled <- shift_geometry(us_percent_hispanic)

rescaled_map <- tm_shape(us_rescaled) + 
  tm_polygons(col = "percent", palette = "plasma", 
              lwd = 0.05, style = "cont",
              legend.is.portrait = FALSE,
              title = "% Hispanic, 2020 Census") + 
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom",
            legend.outside.size = 0.2)


## ----rescaled-map-show-----------------------------------------------------------------
rescaled_map


## ----shifted-map-----------------------------------------------------------------------
us_shifted <- shift_geometry(us_percent_hispanic,
                             position = "outside",
                             preserve_area = TRUE)

shifted_map <- tm_shape(us_shifted) + 
  tm_polygons(col = "percent", palette = "plasma", 
              lwd = 0.05, style = "cont",
              legend.is.portrait = FALSE,
              title = "% Hispanic, 2020 Census") + 
  tm_layout(legend.position = c("left", "BOTTOM"))


## ----shifted-map-show------------------------------------------------------------------
shifted_map


## ----king-asian----------------------------------------------------------
king_asian <- get_decennial(
  geography = "tract",
  variables = c(asian = "P2_008N",
                total = "P2_001N"),
  state = "WA",
  county = "King",
  geometry = TRUE,
  year = 2020,
  output = "wide"
) %>%
  mutate(percent = 100 * (asian / total))

mapview(king_asian, zcol = "percent")



## ----erase-king-asian----------------------------------------------------
library(sf)

king_erase <- king_asian %>%
  st_transform(6596) %>% #<<
  erase_water(area_threshold = 0.9) #<<

mapview(king_erase, zcol = "percent")

imperial_erase <- imperial_poc%>%
  st_transform(6596)%>%
  erase_water(area_threshold = 0.75)

tm_shape(basemap) + 
  tm_rgb()+
  tm_shape(imperial_erase)+
  tm_polygons(col = "percent", title = "2020 US Census",
              alpha = 0.4)+
  tm_layout(title = "Percent POC\nby Census Block", frame = FALSE,
            legend.outside = TRUE)+ 
  tm_scale_bar(position = c("left", "BOTTOM"))+ 
  tm_compass(position = c("right", "top"))

## ----dasymetric-dots-----------------------------------------------------
hennepin_dots_dasy <- hennepin_race %>%
  st_transform(26914) %>%
  as_dot_density(
    value = "value",
    values_per_dot = 250,
    group = "variable",
    erase_water = TRUE
  )

hn_basemap <- osm.raster(st_bbox(hennepin_race),
                         zoom = 10,
                         type = "cartolight",
                         crop = TRUE)

tm_shape(hn_basemap) +
  tm_rgb() +
  tm_shape(hennepin_dots_dasy) +
  tm_dots(col = "variable",
          palette = "Set1",
          size = 0.01) +
  tm_layout(legend.outside = TRUE)
