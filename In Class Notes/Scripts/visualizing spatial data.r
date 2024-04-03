# Additinal Resources

  # https://walker-data.com/census-r/modeling-us-census-data.html
  # https://geocompr.robinlovelace.net/

setwd('B:/UO Teaching/EDLD 652/Slides/Week 8')

require(here)
require(sf)
require(ggplot2)
require(osmdata)
require(tigris)
require(tidyverse)
require(viridis)
require(tidycensus)
require(tmap)
require(elevatr)

################################################################################
#
# Visualizing Geographic Data
#
################################################################################

################################################################################
#             Working with Shape Files
################################################################################

# - US Census Bureau TIGER/Line Files
  # https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

  # Go and explore the files for OREGON
  # Each folder belongs to a county represented by FIPS code
    # https://www.cccarto.com/fipscodes/oregon/
  # 41001 --> Baker County
  # 41039 --> Lane County

  # Download the files in the Lane County Folder
  # Unzip each folder

  # The description of these files
  # https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2008/rel_file_desc_2008.pdf

# Read the .shp files

lane_addrfeat  <- sf::st_read(here('41039/tl_rd22_41039_addrfeat/tl_rd22_41039_addrfeat.shp'))
lane_areawater <- sf::st_read(here('41039/tl_rd22_41039_areawater/tl_rd22_41039_areawater.shp'))
lane_edges     <- sf::st_read(here('41039/tl_rd22_41039_edges/tl_rd22_41039_edges.shp'))
lane_faces     <- sf::st_read(here('41039/tl_rd22_41039_faces/tl_rd22_41039_faces.shp'))
lane_water     <- sf::st_read(here('41039/tl_rd22_41039_linearwater/tl_rd22_41039_linearwater.shp'))
lane_roads     <- sf::st_read(here('41039/tl_rd22_41039_roads/tl_rd22_41039_roads.shp'))


ggplot() +
  geom_sf(data = lane_roads,color = "gray60")+
  theme_void()

ggplot() +
  geom_sf(data = lane_roads,color = "gray60")+
  geom_sf(data = lane_water,color="#518FB5")+
  theme_void()

ggplot() +
  geom_sf(data = lane_roads,color = "gray60")+
  geom_sf(data = lane_water,color="#518FB5")+
  geom_sf(data = lane_areawater,fill="#518FB5")+
  theme_void()

# Get the coordinates for bounding box for Eugene

bb <- osmdata::getbb(place_name = 'Eugene')
bb

ggplot() +
  geom_sf(data = lane_roads,color = "gray60")+
  geom_sf(data = lane_water,color="#518FB5",size=1.2)+
  geom_sf(data = lane_areawater,fill="#518FB5")+
  coord_sf(xlim = bb[1, ], ylim = bb[2, ])+
  theme_void()

# Other countries

  # https://gadm.org/download_country.html

  # TURKEY

  tur0  <- sf::st_read(here('gadm41_TUR_shp/gadm41_TUR_0.shp'))
  tur1  <- sf::st_read(here('gadm41_TUR_shp/gadm41_TUR_1.shp'))
  tur2  <- sf::st_read(here('gadm41_TUR_shp/gadm41_TUR_2.shp'))
  
  
  ggplot() +
    geom_sf(data = tur0)+
    theme_void()
  
  ggplot() +
    geom_sf(data = tur1)+
    theme_void()
  
  
  ggplot() +
    geom_sf(data = tur2)+
    theme_void()
  
  istanbul <- tur2[tur2$NAME_1=='Istanbul',]
  
  ggplot() +
    geom_sf(data = istanbul)+
    theme_void()
  
  # Germany
  
  ger0  <- sf::st_read(here('gadm41_DEU_shp/gadm41_DEU_0.shp'))
  ger1  <- sf::st_read(here('gadm41_DEU_shp/gadm41_DEU_1.shp'))
  ger2  <- sf::st_read(here('gadm41_DEU_shp/gadm41_DEU_2.shp'))
  
  ggplot() +
    geom_sf(data = ger0)+
    theme_void()
  
  ggplot() +
    geom_sf(data = ger1)+
    theme_void()
  
  
  ggplot() +
    geom_sf(data = ger2)+
    theme_void()
  
  bayern <- ger2[ger2$NAME_1=='Bayern',]
  
  ggplot() +
    geom_sf(data = bayern)+
    theme_void()

  # Colombia
  
  col0  <- sf::st_read(here('gadm41_COL_shp/gadm41_COL_0.shp'))
  col1  <- sf::st_read(here('gadm41_COL_shp/gadm41_COL_1.shp'))
  col2  <- sf::st_read(here('gadm41_COL_shp/gadm41_COL_2.shp'))
  
  ggplot() +
    geom_sf(data = col0)+
    theme_void()
  
  ggplot() +
    geom_sf(data = col1)+
    theme_void()
  
  ggplot() +
    geom_sf(data = col2)+
    theme_void()
  
  caldas <- col2[col2$NAME_1=='Caldas',]
  
  ggplot() +
    geom_sf(data = caldas)+
    theme_void()
  
################################################################################
#               Working with the TIGRIS package
################################################################################

# https://uribo.github.io/rpkg_showcase/spatial/tigris.html

# Get the fips codes

View(fips_codes)

# Getting Shape files from the Tigris package

r  <- roads(state='41', county='039', year=2022) 
aw <- area_water(state='41', county='039', year=2022)
lw <- linear_water(state='41', county='039', year=2022) 

ggplot() +
  geom_sf(data = r,color = "gray60")+
  geom_sf(data = lw,color="#518FB5",size=1.2)+
  geom_sf(data = aw,fill="#518FB5")+
  theme_void()


bb <- osmdata::getbb(place_name = 'Eugene')
bb

ggplot() +
  geom_sf(data = r,color = "gray60")+
  geom_sf(data = lw,color="#518FB5",size=1.2)+
  geom_sf(data = aw,fill="#518FB5")+
  theme_void()+
  annotate('text',
           x=mean(bb[1,]),
           y=mean(bb[2,]),
           label='Eugene')
################################################################################
# Get a map of counties for a given state

or <- counties(state='41',year=2022)

ggplot()+
  geom_sf(data=or)

ggplot()+
  geom_sf(data=or)+
  theme_void()
################################################################################
# Assume that you have a variable to map on each county

  # First, you need to create a new data frame

    election <- data.frame(matrix(c(
                           'Baker',23.62,
                           'Benton',67.86,
                           'Clackamas',	53.96,
                           'Clatsop',	54.02,
                           'Columbia',	42.94,
                           'Coos',	38.42,
                           'Crook',	24.61,
                           'Curry',	40.59,
                           'Deschutes',	52.67,
                           'Douglas',	29.78,
                           'Gilliam',	27.50,
                           'Grant',	20.21,
                           'Harney',	19.95,
                           'Hood River',	66.95,
                           'Jackson',	46.77,
                           'Jefferson',	36.88,
                           'Josephine',	35.73,
                           'Klamath',	28.29,
                           'Lake',18.15,
                           'Lane',	60.46,
                           'Lincoln',	56.58,
                           'Linn',	36.50,
                           'Malheur',	27.62,
                           'Marion',	48.86,
                           'Morrow',	26.79,
                           'Multnomah',	79.21,
                           'Polk',	47.46,
                           'Sherman',	21.52,
                           'Tillamook',	47.76,
                           'Umatilla',	32.41,
                           'Union',	28.47,
                           'Wallowa',	31.56,
                           'Wasco',	46.74,
                           'Washington',	65.54,
                           'Wheeler',	22.49,
                           'Yamhill',	46.12),
                           nrow=36,ncol=2,byrow=TRUE))
    
    colnames(election) <- c('NAME','vote_dem')
    election$vote_dem <- as.numeric(election$vote_dem)
    
  # Join with the OR County Shape file
    
  or2 <- left_join(or,election)

  # Fill with color by the variable
  
  ggplot()+
    geom_sf(data=or2,aes(fill=vote_dem))+
    theme_void()+
    scale_fill_viridis()
################################################################################  

# Get a map of US states

st <- states()

ggplot()+
  geom_sf(data=st)

ggplot()+
  geom_sf(data=st)+
  coord_sf(xlim = c(-180,-60), 
           ylim = c(20,72))

ggplot()+
  geom_sf(data=st)+
  coord_sf(xlim = c(-180,-60), 
           ylim = c(20,72))+
  theme_void()


ggplot()+
  geom_sf(data=st)+
  coord_sf(xlim = c(-125,-65), 
           ylim = c(25,50))+
  theme_void()

################################################################################  
# Add a variable at the state level

# Age-adjusted number of deaths per 100,000 total population.
# https://www.cdc.gov/nchs/pressroom/sosmap/heart_disease_mortality/heart_disease.htm

death_rate <- data.frame(matrix(c("Alabama", 237.5,
                                  "Alaska", 139.8,
                                  "Arizona", 144.8,
                                  "Arkansas", 222.5,
                                  "California", 144,
                                  "Colorado", 128.1,
                                  "Connecticut", 138.4,
                                  "Delaware", 159.6,
                                  "Florida", 143.1,
                                  "Georgia", 183.7,
                                  "Hawaii", 125,
                                  "Idaho", 151.9,
                                  "Illinois", 171.4,
                                  "Indiana", 183.9,
                                  "Iowa", 172.9,
                                  "Kansas", 167,
                                  "Kentucky", 204.5,
                                  "Louisiana", 221.5,
                                  "Maine", 146.2,
                                  "Maryland", 168.3,
                                  "Massachusetts", 126.9,
                                  "Michigan", 205,
                                  "Minnesota", 118.1,
                                  "Mississippi", 245.6,
                                  "Missouri", 196.7,
                                  "Montana", 162.7,
                                  "Nebraska", 143.8,
                                  "Nevada", 201.3,
                                  "New Hampshire", 146.5,
                                  "New Jersey", 166.3,
                                  "New Mexico", 152.7,
                                  "New York", 183.9,
                                  "North Carolina", 156.2,
                                  "North Dakota", 147.3,
                                  "Ohio", 196.9,
                                  "Oklahoma", 244.1,
                                  "Oregon", 134,
                                  "Pennsylvania", 175.7,
                                  "Rhode Island", 150.5,
                                  "South Carolina", 170.9,
                                  "South Dakota", 155.2,
                                  "Tennessee", 212,
                                  "Texas", 173.9,
                                  "Utah", 155.6,
                                  "Vermont", 167.1,
                                  "Virginia", 152,
                                  "Washington", 134.6,
                                  "West Virginia", 197.8,
                                  "Wisconsin", 162.2,
                                  "Wyoming", 160.4),
                                nrow=50,ncol=2,byrow=TRUE))
  
colnames(death_rate) <- c('NAME','rate')
death_rate$rate <- as.numeric(death_rate$rate)

# Join with the OR County Shape file

st2 <- left_join(st,death_rate)

ggplot()+
  geom_sf(data=st2,aes(fill=rate))+
  coord_sf(xlim = c(-125,-65), 
           ylim = c(25,50))+
  theme_void()+
  scale_fill_viridis()

bb <- osmdata::getbb(place_name = 'Oregon')
bb

ggplot()+
  geom_sf(data=st2,aes(fill=rate))+
  coord_sf(xlim = c(-125,-65), 
           ylim = c(25,50))+
  theme_void()+
  scale_fill_viridis()+
  annotate('text',
           x=mean(bb[1,]),
           y=mean(bb[2,]),
           label='OR',
           color='white')


# Other countries

tur1  <- sf::st_read(here('gadm41_TUR_shp/gadm41_TUR_1.shp'))

pop <- data.frame(matrix(c("Istanbul", 18.65,
                                  "Ankara", 6.78,
                                  "Izmir", 5.23,
                                  "Bursa", 3.75,
                                  "Antalya", 3.15,
                                  "Konya", 2.69,
                                  "Adana", 2.67,
                                  "Sanlıurfa", 2.54,
                                  "Gaziantep", 2.53,
                                  "Kocaeli", 2.44,
                                  "Mersin", 2.25,
                                  "Diyarbakir", 2.12,
                                  "Hatay", 1.98,
                                  "Manisa", 1.72,
                                  "Kayseri", 1.69,
                                  "Samsun", 1.6,
                                  "Balikesir", 1.47,
                                  "Kahramanmaras", 1.38,
                                  "Aydin", 1.35,
                                  "Tekirdağ", 1.34,
                                  "Van", 1.32,
                                  "Sakarya", 1.27,
                                  "Denizli", 1.24,
                                  "Mugla", 1.23,
                                  "Eskisehir", 1.06,
                                  "Mardin", 1.02,
                                  "Trabzon", 0.96,
                                  "Malatya", 0.95,
                                  "Ordu", 0.89,
                                  "Erzurum", 0.88,
                                  "Afyonkarahisar", 0.88,
                                  "Adiyaman", 0.74,
                                  "Sivas", 0.74,
                                  "Batman", 0.74,
                                  "Tokat", 0.7,
                                  "Elazıg", 0.69,
                                  "Zonguldak", 0.69,
                                  "Kutahya", 0.68,
                                  "Osmaniye", 0.66,
                                  "Canakkale", 0.66,
                                  "Sırnak", 0.65,
                                  "Corum", 0.61,
                                  "Agrı", 0.6,
                                  "Giresun", 0.53,
                                  "Isparta", 0.52,
                                  "Aksaray", 0.51,
                                  "Yozgat", 0.49,
                                  "Edirne", 0.49,
                                  "Düzce", 0.48,
                                  "Mus", 0.47,
                                  "Kastamonu", 0.44,
                                  "Usak", 0.44,
                                  "Kirklareli", 0.43,
                                  "Nigde", 0.43,
                                  "Bitlis", 0.42,
                                  "Rize", 0.4,
                                  "Amasya", 0.4,
                                  "Siirt", 0.39,
                                  "Bolu", 0.38,
                                  "Nevsehir", 0.36,
                                  "Yalova", 0.35,
                                  "Bingol", 0.33,
                                  "Kirikkale", 0.32,
                                  "Hakkari", 0.32,
                                  "Kars", 0.32,
                                  "Burdur", 0.32,
                                  "Karaman", 0.31,
                                  "Karabuk", 0.3,
                                  "Kirşehir", 0.29,
                                  "Erzincan", 0.28,
                                  "Bilecik", 0.27,
                                  "Sinop", 0.26,
                                  "Iğdir", 0.24,
                                  "Bartin", 0.24,
                                  "Çankiri", 0.23,
                                  "Artvin", 0.2,
                                  "Kilis", 0.17,
                                  "Gumushane", 0.17,
                                  "Ardahan", 0.11,
                                  "Tunceli", 0.1,
                                  "Bayburt", 0.1),
                                nrow=81,ncol=2,byrow=TRUE))

colnames(pop) <- c('NAME_1','pop')
pop$pop <- as.numeric(pop$pop)

# Join with the OR County Shape file

tur1 <- left_join(tur1,pop)

ggplot()+
  geom_sf(data=tur1,aes(fill=pop))+
  theme_void()+
  scale_fill_viridis(option='turbo')+
  labs(title = "Percentage of People Living in Turkish Cities")

################################################################################  
#                            Visualize US CENSUS DATA
################################################################################

# Get a US Census API Key

  # https://api.census.gov/data/key_signup.html

  # Then run the following code,
  # and add 
  # CENSUS_API_KEY = "YOUR API KEY"

    usethis::edit_r_environ()


################################################################################
# 2000, 2010, and 2020 decennial US Census Data

# Variable list,

  # ?load_variables

  census_2020 <- load_variables(2020, "pl", cache = TRUE)

  View(census_2020)
  
  # Total number of people
  
  total <- get_decennial(geography = "state", 
                         variables = "P1_001N", 
                         year = 2020,
                         geometry = TRUE)
  
  total <- shift_geometry(total,position = 'below')
  
    # This rescales data so Alaska, Hawaii, and Puerto Rico
    # are transformed to appropriate coordinates
  
  ggplot(total) +
    geom_sf(aes(fill = value, color = value)) +
    guides(color = "none")+
    theme_void()+
    scale_fill_viridis(option='turbo')+
    scale_color_viridis(option='turbo')+
    labs(title = "Total Number of People in the US",
         subtitle = "2020 US Census")
  
  # Proportion of Hispanic/Latino
  
  hisp <- get_decennial(geography = "state", 
                         variables = "P2_002N", 
                         year = 2020,
                         geometry = TRUE)
  
  hisp <- shift_geometry(hisp,position = 'below')
  
  hisp$prop <- hisp$value/total$value
  
  
  ggplot(hisp) +
    geom_sf(aes(fill = prop, color = prop)) +
    guides(color = "none")+
    theme_void()+
    scale_fill_viridis(option='turbo')+
    scale_color_viridis(option='turbo')+
    labs(title = "Proportion of Hispanic/Latino People in the US",
         subtitle = "2020 US Census")
  
  # State Specific Values
  
  total_or <- get_decennial(geography = "county", 
                         variables = "P1_001N", 
                         year = 2020,
                         state = 'OR',
                         geometry = TRUE)
  
  hisp_or <- get_decennial(geography = "county", 
                        variables = "P2_002N", 
                        year = 2020,
                        state = 'OR',
                        geometry = TRUE)
  
  hisp_or$prop <- hisp_or$value/total_or$value
  
  ggplot(hisp_or) +
    geom_sf(aes(fill = prop, color = prop)) +
    guides(color = "none")+
    theme_void()+
    scale_fill_viridis(option='turbo')+
    scale_color_viridis(option='turbo')+
    labs(title = "Proportion of Hispanic/Latino People in Oregon, US",
         subtitle = "2020 US Census")
  
################################################################################

# American Community Survey
  # https://www.census.gov/programs-surveys/acs

# Variable list
# ?load_variables

acs5_2021 <- load_variables(2021, "acs5", cache = TRUE)

View(acs5_2021)

# B24134_005, logging
# B06011_001, median income

# Median income by county in Oregon

med_income <- get_acs(geography  = "county",
                       state     = "OR",
                       variables = c(med_income = "B06011_001"),
                       year      = 2021,
                       geometry  = TRUE)

ggplot(med_income) +
  geom_sf(aes(fill = estimate, color = estimate)) +
  guides(color = "none")+
  theme_void()+
  scale_fill_viridis(option='turbo')+
  scale_color_viridis(option='turbo')+
  labs(title = "Median income by county in Oregon",
       subtitle = "2017-2021 American Community Survey")

# Commuting

transportation <- get_acs(geography  = "state",
                          variables = c(total   = "B08101_001",
                                        drove   = "B08101_009",
                                        carpool = "B08101_017",
                                        public  = "B08101_025",
                                        walk    = "B08101_033",
                                        other   = "B08101_041",
                                        home    = "B08101_049"),
                year      = 2021,
                output    = 'wide',
                geometry  = TRUE)

    # nameE--Estimate, nameM--margin of error

transportation <- shift_geometry(transportation,position = 'below')

transportation$p <- transportation$homeE/transportation$totalE

ggplot(transportation) +
  geom_sf(aes(fill = p, color = p)) +
  guides(color = "none")+
  theme_void()+
  scale_fill_viridis(option='turbo')+
  scale_color_viridis(option='turbo')+
  labs(title = "Proportion of People Working from Home",
       subtitle = "2017-2021 American Community Survey")

################################################################################  
#                     Working with the TMP package
################################################################################

tm_shape(transportation) +
  tm_polygons("p") +
  tm_layout(legend.outside = TRUE)

# Change colors

tm_shape(transportation) +
  tm_polygons("p",
              palette = "magma",
              border.col = "gray90",
              lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

# Continuous legend

tm_shape(transportation) +
  tm_polygons("p",
              palette = "magma",
              border.col = "gray90",
              lwd = 0.1,
              style='cont') +
  tm_layout(legend.outside = TRUE)

# Add text

centroids <- st_centroid(transportation)

tm_shape(transportation) +
  tm_polygons("p",
              palette = "magma",
              border.col = "gray90",
              lwd = 0.1,
              style='cont') +
  tm_shape(centroids) +
  tm_text("NAME", size = 0.5) +
  tm_layout(legend.outside = TRUE)

# Elevation Data

tur  <- sf::st_read(here('gadm41_TUR_shp/gadm41_TUR_2.shp'))
tur_elev <- get_elev_raster(tur, z = 9)

tm_shape(tur_elev) +
  tm_raster(midpoint = NA,
            style = "cont",
            palette = c("#E2FCFF", "#83A9CE", "#485C6E", 
                        "#181818", "#5C5B3E", "#AAA971",
                        "#FCFCD3", "#ffffff")) +
  tm_layout(legend.outside=TRUE) 

# Interactive maps

tmap_mode("view")

tm_shape(transportation) +
  tm_polygons("p",
              palette = "magma",
              border.col = "gray90",
              lwd = 0.1,
              style='cont') +
  tm_shape(centroids) +
  tm_text("NAME", size = 0.5) +
  tm_layout(legend.outside = TRUE)

  # See for more tmap
  # https://geocompr.robinlovelace.net/adv-map.html












