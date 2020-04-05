

# this code scrapes the counts from the LA County Corona Virus 19 website
# it also gets the shape file of communities for Los Angeles County
# joins both by name, when there is more than one match name in the shape file, the 
# data is sorted by area and the biggest area is selected
# not all names matched
# Moises Evangelista  2020-03-25

library(rvest)
library(tidyverse)
library(sf)

options(scipen = 999)

rm(list = ls()) #start with empty workspace

# create a folder to download files if it does not exist

ifelse(!dir.exists(file.path("~/GitHub/covid19")),
       dir.create(file.path("~/GitHub/covid19")), FALSE)

setwd("~/GitHub/covid19")

currntTime <- Sys.time()

filename <- paste0("cnts_"
                   , currntTime %>% format(., '%Y%m%d_%p')
)

if(file.exists(  paste0(filename,
                        ".rds")) ) {
  
  cnts <- readRDS( paste0(filename,
                          ".rds")
  )
  
  rm(filename)
  
  print("cnts file loaded")
  
} else {
  
  x <- read_html("http://publichealth.lacounty.gov/media/Coronavirus/locations.htm")
  
  dataAsOf <- x %>% 
    html_nodes(".tytul,table") %>% 
    html_text() %>% 
    sub(".* (\\d+\\D{2} \\d+/\\d+).*", "\\1", .) %>% 
    paste0(., "/2020") %>% 
    as.POSIXct(., format = '%I%p %m/%d/%Y') %>% 
    .[!is.na(.)]
  
  x <- x %>% 
    html_nodes("table") %>%
    html_table() %>% 
    .[[1]] %>% 
    as.data.frame() %>% 
    setNames(.[1,]%>% unlist() %>% as.character()) %>% 
    setNames(gsub("[^[:alnum:]]", "", perl = TRUE, names(.))) %>%
    mutate(type = case_when(grepl("Locations|Deaths|Age Group|^Investigation|CITY/COMMUNITY", Locations ) ~ Locations ),
           type = sub(".*(Locations|Deaths|Age Group|Investigation|CITY/COMMUNITY).*", "\\1", type),
           type = zoo::na.locf(type,  na.rm = FALSE),
           type = case_when(is.na(type)~ "locations" , 
                            TRUE ~ type),
           TotalCases = gsub("[^[:digit:]]", "", TotalCases),
           TotalCases = as.numeric(TotalCases),
           Rate = gsub("[^[:digit:]\\.]", "", Rate),
           Rate = as.numeric(Rate),
           cleanLocations = gsub("[^[:alnum:]/ -]", "", perl = TRUE, Locations),
           cleanLocations = trimws(cleanLocations),
           cleanLocations = gsub("\\s+" , " ", cleanLocations),
           dataAsOf = dataAsOf
    ) %>% 
    rowid_to_column("cnt_id") 
  
  saveRDS(x
          ,paste0("cnts_"
                  , currntTime %>% format(., '%Y%m%d_%p')
                  ,".rds" ) )
  
  print("scraped data")
  
}

# get shapefiles

if(file.exists("Communities.Rdata")) {
  
  load("Communities.Rdata")
  
  print("shapefileLoaded")
  
} else {
  
  ifelse(!dir.exists(file.path("./shapefiles")),
         dir.create(file.path("./shapefiles")), FALSE)
  
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir = td, fileext = ".zip")
  # download into the placeholder file
  download.file("http://egis3.lacounty.gov/dataportal/wp-content/uploads/2010/10/Communities1.zip"
                , tf)
  
  # get the name of the first file in the zip archive
  fname = unzip(tf, exdir = "shapefiles") #, list=TRUE)  $Name %>% .[grepl("shp$",.)]
  
  Communities = sf::st_read("./shapefiles/Communities.shp") %>% # 
    st_transform(., 2229) %>% # change coordinte system to LA County official system
    sf::st_transform(., 4326) # change it to GPS satellite navigation system
  
  sf::st_crs(Communities)
  
  save(Communities, file = "Communities.Rdata")
  
}

# get roads file

if(file.exists("tigerroads.Rdata")) {
  
  load("tigerroads.Rdata")
  
  print("shapefileLoaded")
  
} else {
  
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir = td, fileext = ".zip")
  # download into the placeholder file
  download.file("http://egis3.lacounty.gov/dataportal/wp-content/uploads/2011/04/tigerroads.zip"
                , tf)
  
  # get the name of the first file in the zip archive
  
  fname = unzip(tf, exdir = "shapefiles") #, list=TRUE)  $Name %>% .[grepl("shp$",.)]
  
  tigerroads = sf::st_read("./shapefiles/tigerroads.shp") %>% 
    filter(grepl("S1200|S1100", MTFCC)) %>% 
    st_transform(., 2229) %>% # t2000 <- tigerroads %>% # change coordinate system to LA County official system
    rmapshaper::ms_simplify(., keep = 0.01,
                            keep_shapes = TRUE) %>% 
    sf::st_transform(., 4326) # change it to GPS satellite navigation system
  
  sf::st_crs(tigerroads)
  
  save(tigerroads, file = "tigerroads.Rdata")
  
  unlink(td)
  unlink(tf)
  unlink("shapefiles")
  
}

Communities <- Communities %>% 
  rowid_to_column("comm_id")

Communities %>% 
  st_drop_geometry(.) %>%
  count(COMMTYPE, NAME, PO_NAME, STATNAME, LABEL_CITY,LABEL_COMM) %>%
  #count(COMMTYPE, NAME) %>% #
  View("mapNames")

# the table with counts by city/community
# has values that are not in the shape file or the shape file has duplicates e.g. Covina

clnCnts <- cnts %>%
  filter(type == "CITY/COMMUNITY" | type == "Locations" & grepl("Beach|Pasadena", Locations)
  ) %>% 
  filter(!is.na(TotalCases) & TotalCases > 0) %>%
  mutate(COMMTYPE = case_when(grepl("^Los Angeles", cleanLocations) ~ "Los Angeles",
                              grepl("^Unincorporated", cleanLocations) ~ "Unincorporated",
                              grepl("^City of Los Angeles", cleanLocations) ~ "City of Los Angeles",
                              TRUE ~ "City"),
         cleanLocations = sub("^Los Angeles - |^Unincorporated - |^- |City of ", "", cleanLocations),
         cleanLocations = trimws(cleanLocations),
         cleanLocations = case_when(cleanLocations == "Athens-Westmont" ~ "Westmont / West Athens",
                                    cleanLocations == "Azuza" ~ "Azusa",
                                    TRUE ~ cleanLocations)) %>% 
  filter(COMMTYPE != "City of Los Angeles")

combo <- clnCnts %>% 
  left_join(Communities %>% 
              st_drop_geometry(.) %>% 
              mutate( cleanLocations = gsub("[^[:alnum:]/ -]", "", perl = TRUE, NAME),
                      cleanLocations = trimws(cleanLocations),
                      cleanLocations = gsub("\\s+" , " ", cleanLocations)
              ) %>% 
              arrange(desc(AREA_SQMI)) %>% 
              group_by(cleanLocations, COMMTYPE) %>% 
              slice(1) %>% 
              ungroup() %>% 
              select(cleanLocations, COMMTYPE, comm_id)
  ) %>% 
  full_join(Communities) %>% 
  mutate(matchType = case_when(!is.na(comm_id) & !is.na(cnt_id) ~ "OnBoth",
                               is.na(comm_id) & !is.na(cnt_id) ~ "OnCount_NotInCommunity",
                               !is.na(comm_id) & is.na(cnt_id)  ~ "OnCommunity_NotIncounts",
                               TRUE ~ "Other")) %>% 
  st_as_sf()

combo %>% st_drop_geometry() %>% 
  count(matchType, comm_id = ifelse(is.na(comm_id), "NotMatched", "community"),
        cnt_id = ifelse(is.na(cnt_id), "NotMatched", "count"))

combo %>% st_drop_geometry() %>%
  select(Locations, comm_id, cnt_id, cleanLocations,matchType
         , NAME, COMMTYPE, TotalCases) %>%
  View()

# fucntion to determine where to place the dots

st_centroid_within_poly <- function (poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}

combo_Points <- combo %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid_within_poly(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid_within_poly(.x)[[2]]))

combo %>%
  st_drop_geometry(.) %>%
  count(type, NAME, cnt_id, comm_id, sort = TRUE) %>% 
  View()

combo_Points %>%
  st_drop_geometry(.) %>% View

labelsBreaks <- pretty(combo_Points %>% 
                         st_drop_geometry(.) %>% 
                         filter(!is.na(lon)) %>% 
                         .$TotalCases , 5)

labelsBreaksRate <- pretty(combo_Points %>% 
                             st_drop_geometry(.) %>% 
                             filter(!is.na(lon)) %>% 
                             .$Rate , 5)

labelsBreaksRate <- combo %>%
  st_drop_geometry() %>% 
  .$Rate %>% 
  # .[.<200] %>% #order()
  BAMMtools::getJenksBreaks(., k = 6, subset = NULL)

age.cat <- function(x, lower = 0, upper, #by = 10,
                    sep = "-", above.char = "+") {
  
  c(paste(seq(lower, upper - by, by = by),
          seq(lower + by - 1, upper - 1, by = by),
          sep = sep),
    paste(upper, above.char, sep = ""))
  
}


age.cat <- function(x, lower = 0, upper, by = 10,
                    sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  cut(floor(x), breaks = BAMMtools::getJenksBreaks(x, k = 6, subset = NULL),
      right = FALSE, labels = labs)
}



# %>% 
#   .[, HC04_EST_VC25_Grps := cut(HC04_EST_VC25,
#                                 # breaks = jenksBreaks$brks, 
#                                 breaks = BAMMtools::getJenksBreaks(.$HC04_EST_VC25, 6, subset = NULL),
#                                 include.lowest = TRUE)] %>% 
#   setattr(., "comment", listOfAttr) # %>%   setattr(., "comment2", listOfAttr2) 


matchedCommunities <- combo %>%
  st_drop_geometry(.) %>% 
  group_by(matchType) %>% 
  summarise(Total_CommunityCases = sum(TotalCases, na.rm = TRUE),
            NumberOfCommunities = n()) %>% # View
  ungroup

Cairo::CairoPDF(file = "LACountyCOVID19_2020040412PM.pdf",
                #units = "in", dpi = 150,
                width = 8, 
                height = 11, 
                pointsize = 10)

ggplot() +
  theme_void() +
  coord_equal(xlim = c(0, 90), ylim = c(0, 60), expand = FALSE) +
  annotation_custom(gridExtra::tableGrob(matchedCommunities %>% 
                                           as.data.frame()
                                         , rows = NULL
                                         , theme = gridExtra::ttheme_minimal(base_size = 10
                                                                             , padding = unit(c(2, 2), "mm")))
                    , xmin = 20, xmax = 50, ymin = 30, ymax = 55
  ) +
  # scale_size_manual( "Total Cases") +
  hrbrthemes::theme_ipsum_rc(
    plot_title_size = 20,
    subtitle_size = 15,
    caption_size = 10
  ) +
  labs(
    title = "Type of matches between the cities and communities reported vs
       the cities and communities on the shape file"
    ,caption = "Note: Counts do not match total reported for the county because some couts are suppressed")

# ggplot() +
#   geom_sf(data = combo
#           ,aes(fill = TotalCases), size = .001) +
#   # geom_point(data = combo_Points %>% 
#   #              filter(!is.na(TotalCases)), aes(x = lon, y = lat, size = TotalCases)) +
#   scale_fill_gradient2(name = "Total Cases"
#                        ,low = "darkgreen", mid = "white", high = "darkred"
#                        ,midpoint = median(labelsBreaks)
#                        , breaks = as.integer(labelsBreaks), 
#                        labels = prettyNum(labelsBreaks, big.mark = ",")) +
#   hrbrthemes::theme_ipsum_rc(plot_title_size = 20, subtitle_size = 15, caption_size = 10) +
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_line(color = "gray91"),
#         panel.grid.minor = element_line(color = "gray94"),
#         strip.background = element_rect(colour = NA,
#                                         fill="blue"), # element_blank(),
#         strip.text.x = element_text(size = 10, 
#                                     colour = "white", 
#                                     angle = 00,
#                                     hjust = 0 #, lineheight = .5
#         ),
#         axis.text.x =  element_blank()
#         ,axis.text.y =  element_blank()
#         ,axis.title =  element_blank(),
#         legend.position = "top",
#         legend.key.width = unit(2, "cm") )

ggplot() +
  geom_sf(data = combo, size = .001) +
  geom_sf(data = Communities %>% 
            filter(grepl("Long Beach|^Pasadena", NAME)) %>% 
            group_by(NAME) %>% 
            arrange(desc(AREA_SQMI)) %>% 
            slice(1) %>% 
            ungroup, color = "red", size = .001) +
  geom_sf(data = tigerroads %>% 
            filter(MTFCC == "S1100")
          ,color = "cornsilk",
          alpha = .75, size = .1) +
  geom_point(data = combo_Points %>% 
               filter(!is.na(lon))
             , aes(x = lon, y = lat
                   , size = TotalCases
                   , color = TotalCases),
             alpha = .75
             ,stroke = .01) +
  # ggrepel::geom_text_repel(data = data_PRI %>% 
  #                            filter(grepl("Los Angeles|Orange|Kern|Ventura|Riverside|San Bernardino", County))
  #                          ,aes(x = Calendar.Year, y = PRI
  #                               ,label = County
  #                               ,color = County)
  #                          ,size = 5
  #                          ,show.legend = FALSE
  #                          ,nudge_y      = 0.05,
  #                          direction    = "x",
  #                          angle        = 00,
  #                          vjust        = 0,
#                          segment.size = 0.2)
scale_color_gradient2(
  name = "Total Cases"
  ,low = "darkgreen",
  mid = "#f1a340",
  high = "darkred"
  ,midpoint = median(labelsBreaks)
  ,breaks = as.integer(labelsBreaks),
  labels = prettyNum(labelsBreaks, big.mark = ",")
) +
  # scale_size_manual( "Total Cases") +
  hrbrthemes::theme_ipsum_rc(
    plot_title_size = 20,
    subtitle_size = 15,
    caption_size = 10
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray91"),
    panel.grid.minor = element_line(color = "gray94"),
    strip.background = element_rect(colour = NA,
                                    fill = "blue"),
    # element_blank(),
    strip.text.x = element_text(
      size = 10,
      colour = "white",
      angle = 00,
      hjust = 0 #, lineheight = .5
    ),axis.text.x = element_blank()
    ,axis.text.y = element_blank()
    ,axis.title = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    #legend.margin = margin(),
    legend.key.width = unit(2, "cm")
  ) +
  labs(size = "Total Cases",
       title = paste0("Novel Coronavirus in Los Angeles County
       as of ",
                      cnts$dataAsOf %>% unique() %>% format("%B %d, %Y (%p)")
       )
       ,caption = "Note: Pasadena and Long Beach have their own health departments (show outlined in red)
       separate from the County.
       Sources: http://publichealth.lacounty.gov/media/Coronavirus/locations.htm (counts),
        http://egis3.lacounty.gov/dataportal/wp-content/uploads/2010/10/Communities1.zip (shapefile)")

ggplot() +
  geom_sf(data = combo
          , size = .001) +
  geom_sf(data = combo %>% 
            filter(!is.na(comm_id)) %>% 
            mutate(Rate_Groups = cut(Rate,
                                     # breaks = jenksBreaks$brks, 
                                     breaks = BAMMtools::getJenksBreaks(.$Rate, 6, subset = NULL),
                                     include.lowest = TRUE)
                   # ,Rate_Groups = case_when(grepl(labelsBreaksRate[length(labelsBreaksRate)-1] %>%
                   #                                 round(.) %>%
                   #                                 paste0("\\(",.), Rate_Groups) ~ 
                   #                           paste0(labelsBreaksRate[length(labelsBreaksRate)-1]
                   #                                  ,"+"), TRUE ~ as.character(Rate_Groups))
            )
          , size = .001
          ,aes(fill = Rate_Groups)) +
  geom_sf(data = Communities %>% 
            filter(grepl("Long Beach|^Pasadena", NAME)) %>% 
            group_by(NAME) %>% 
            arrange(desc(AREA_SQMI)) %>% 
            slice(1) %>% 
            ungroup,
          fill = NA,
          color = "red", size = .001) +
  geom_sf(data = tigerroads %>% 
            filter(MTFCC == "S1100")
          ,color = "cornsilk",
          alpha = .75, size = .1) +
  scale_fill_brewer(name = "Rate"
                    ,palette = "Paired"
                    ,na.value = "grey50") +
  # scale_size_manual( "Total Cases") +
  hrbrthemes::theme_ipsum_rc(
    plot_title_size = 20,
    subtitle_size = 15,
    caption_size = 10
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray91"),
    panel.grid.minor = element_line(color = "gray94"),
    strip.background = element_rect(colour = NA,
                                    fill = "blue"),
    # element_blank(),
    strip.text.x = element_text(
      size = 10,
      colour = "white",
      angle = 00,
      hjust = 0 #, lineheight = .5
    ),axis.text.x = element_blank()
    ,axis.text.y = element_blank()
    ,axis.title = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    #legend.margin = margin(),
    legend.key.width = unit(2, "cm")
  ) +
  labs(size = "Rate of cases",
       title = paste0("Novel Coronavirus in Los Angeles County
                      as of ",
                      cnts$dataAsOf %>% unique() %>% format("%B %d, %Y (%p)")
       )
       ,caption = "Note: Pasadena and Long Beach have their own health departments (show outlined in red)
       separate from the County.
       Sources: http://publichealth.lacounty.gov/media/Coronavirus/locations.htm (counts),
       http://egis3.lacounty.gov/dataportal/wp-content/uploads/2010/10/Communities1.zip (shapefile)")

dev.off()

system(paste0('open "', 'LACountyCOVID19_2020040412PM.pdf', '"'))
