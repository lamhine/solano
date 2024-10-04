library(tidyverse)
library(tidycensus)
library(tigris)
library(maps)
library(sf)
library(ggrepel)
library(mapview)
library(poLCA)

census_api_key("f77de7a3ed4802ce5f3a28b99ba1105b8bb39529")

pull_data = function(my_state, my_county){
  
  acs = load_variables(2018, 'acs5', cache = TRUE) 
  
  concepts = acs %>% 
    filter(concept == 'TENURE BY OCCUPANTS PER ROOM')
  
  concepts_string = concepts %>% 
    dplyr::select(1) %>% 
    pull() # this creates a character vector of just the concepts we want
  
  raw =  get_acs(geography = 'tract', variables = concepts_string, year = 2018, state = my_state, 
                 county = my_county,geometry = TRUE, cache = TRUE)
  
  fixed_data = concepts %>% 
    dplyr::select(name, label) %>% 
    right_join(raw, by = c('name' = 'variable')) %>% 
    dplyr::select(2,3,5,7) %>% 
    spread(key = label, value = estimate, -4)
  
  names(fixed_data) = make.names(names(fixed_data))
  
  fill_data = fixed_data %>% 
    mutate(
      total_under_1 = Estimate..Total..Owner.occupied..0.50.or.less.occupants.per.room + Estimate..Total..Owner.occupied..0.51.to.1.00.occupants.per.room + Estimate..Total..Renter.occupied..0.50.or.less.occupants.per.room + Estimate..Total..Renter.occupied..0.51.to.1.00.occupants.per.room,
      total_over_1 = Estimate..Total..Owner.occupied..1.01.to.1.50.occupants.per.room + Estimate..Total..Owner.occupied..1.51.to.2.00.occupants.per.room + Estimate..Total..Owner.occupied..2.01.or.more.occupants.per.room + Estimate..Total..Renter.occupied..1.01.to.1.50.occupants.per.room + Estimate..Total..Renter.occupied..1.51.to.2.00.occupants.per.room + Estimate..Total..Renter.occupied..2.01.or.more.occupants.per.room
    ) %>% 
    dplyr::select(GEOID,geometry ,total_under_1, total_over_1, Estimate..Total) %>% 
    mutate(ratio_over_1 = total_over_1/ Estimate..Total)
  
  roads_data = primary_secondary_roads(my_state) %>% 
    filter(RTTYP %in% c('U','S','I'))
  
  counties_data = counties(cb = TRUE)
  water_data = area_water(my_state,my_county)
  
  cities_data = places(my_state)
  
  return(list(fill = fill_data, roads = roads_data, counties = counties_data, water =  water_data, cities = cities_data))
}


mapper = function(input_data, my_cities, legend_position = 'bottom left'){ #accepts list object from pull_data function
  map_frame = input_data$fill
  limits = st_bbox(map_frame$geometry)
  roads = input_data$roads
  county = input_data$counties
  water = input_data$water
  cities = input_data$cities %>% 
    filter(NAME %in% my_cities) 
  
  legend_placer = function(position = 'bottom left'){
    if(position == 'bottom left'){
      ret = (list(c(0,0),c(0.02,0.04)))
    }else if(position == 'top left'){
      ret =(list(c(0,1),c(0.02,0.96)))
    }else if (position == 'bottom right'){
      ret=(list(c(1,0),c(0.98,0.04)))
    }else if(position == 'top right'){
      ret=(list(c(1,1),c(0.98,0.96)))
    }else(stop('invalid input: must supply one of: bottom left, bottom right, top left, top right'))
    return(theme(legend.justification = ret[[1]], legend.position = ret[[2]]))
  }
  
  ggplot() +
    theme_void()  + #whiteout
    legend_placer(legend_position) +
    labs(fill = 'Share of housing\nunits with more\nthan one person\nper room')+ #label legend
    geom_sf(data = county, alpha = 1, fill = '#e6e6e6', color = NA,aes(geometry = geometry)) + #grey landmass
    geom_sf(data = map_frame, alpha = 1,color = NA, aes(geometry = geometry, fill = ratio_over_1)) + #our census map
    scale_fill_gradient(low = '#ffff99', high = '#b10026', na.value = '#ffffcc' ) + # custom fill palette
    geom_sf(data = water, fill = '#cce6ff',color = NA, aes(geometry = geometry)) + #add water
    theme(panel.background = element_rect(fill = '#cce6ff', color = NA)) + #blue background to mesh with water
    #geom_sf(data = roads, color = 'white', aes(geometry = geometry)) + #add roads
    geom_text_repel(color = 'black', data = cities, aes(label = NAME, geometry = geometry), stat = "sf_coordinates") + #add city names
    geom_sf(data = county, alpha = 0, aes(geometry = geometry)) + 
    coord_sf(xlim = c(as.numeric(limits$xmin),as.numeric(limits$xmax)), ylim = c(as.numeric(limits$ymin),as.numeric(limits$ymax))) #set framing(very important)
}

bay = pull_data(
  my_state = 'CA', 
  my_county = c("Alameda","Contra Costa","Marin","Napa","San Francisco", "San Mateo", "Solano","Sonoma"))

mapper(
  bay, 
  my_cities = c("Alameda", "Albany", "Berkeley", "Hayward", 
                "Oakland", "Piedmont", "Redwood City", "Richmond",
                "San Francisco", "San Leandro", "San Mateo", 
                "San Rafael", "Santa Rosa", "Vallejo"),
  legend_position = 'bottom left')








## Get map data for place and county layers and create initial map
places_ca <- places("CA") 
counties_ca <- counties("CA")
map <- mapview(counties_ca, alpha.regions = 0) + mapview(places_ca, alpha.regions = 0)

## Load in SRPS data exported from MonQcle
srps_data <- read_csv(
  "Library/CloudStorage/GoogleDrive-lamhine@stanford.edu/My Drive/SRPS/Research Materials/Structural_Racism_in_Statistical_Report_2024-10-02-21-37-23.csv")

## Clean up SRPS data

# remove unnecessary columns and filter to only original
srps_data <- srps_data %>% 
  filter(series_title == "original") %>% 
  dplyr::select(-c("Effective Date", "Valid Through Date", "series_title")) 

# change all "-" character to NA
srps_data <- srps_data %>% 
  mutate(across(where(is.character), ~na_if(., ".")))

# separate place name at comma
srps_data <- srps_data %>% 
  separate_wider_delim("Jurisdictions", ", ", names = c("city", "county", "state", "usa"), too_few="align_end", too_many = "drop")

# fix missing data point
srps_data[9,1] <- "San Francisco"

# drop unneeded columns
srps_data <- srps_data %>% dplyr::select(-c("state", "usa"))

# map elections data
srps_elections <- left_join(places_ca, dplyr::select(srps_data, city, elections) , by = c("NAME" = "city"))
map_elections <- map + mapview(srps_elections, zcol = "elections")

# map mayoral district elections
srps_elections_mayor <- left_join(places_ca, dplyr::select(srps_data, city, `elections_positions Mayor or city manager`) , by = c("NAME" = "city"))
map_elections_mayor <- map + mapview(srps_elections_mayor, zcol = "elections_positions Mayor or city manager")

# map 
