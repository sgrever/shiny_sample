# Could prob clean this: 
# https://data.chhs.ca.gov/dataset/healthcare-facility-locations/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0
# https://data.chhs.ca.gov/dataset/3b5b80e8-6b8d-4715-b3c0-2699af6e72e5/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0/download/health_facility_locations.xlsx
# similar dataset here: https://datavisualization.cdph.ca.gov/t/LNC/views/OpenDataFacility-FacilitiesAcrossTime/PercentChange?iframeSizedToWindow=true&%3Aembed=y&%3AshowAppBanner=false&%3Adisplay_count=no&%3AshowVizHome=no
# good viz ideas: https://public.tableau.com/app/profile/naledi.hollbruegge
# another pretty example: https://gerinberg.com/

library(readxl)
library(dplyr)
library(stringr)
library(glue)
library(janitor)
library(ggplot2)
# library(rio)
library(lubridate)
library(sf)
library(leaflet)
library(tibble)
library(DT)
# library(tigris)
# options(tigris_use_cache = TRUE)


# url <- "https://data.chhs.ca.gov/dataset/3b5b80e8-6b8d-4715-b3c0-2699af6e72e5/resource/098bbc36-044d-441f-9442-1f4db4d8aaa0/download/health_facility_locations.xlsx"
# dat <- rio::import(file = url, which = 1) %>% 
#   clean_names()

dat <- read_excel("health_facility_locations.xlsx") %>% 
  clean_names()

colnames(dat)[str_detect(colnames(dat), "date")]
# [1] "participation_date"      "approval_date"          
# [3] "start_date"              "initial_license_date"   
# [5] "license_effective_date"  "license_expiration_date"
# [7] "data_date" 

head(dat[ , which(str_detect(colnames(dat), "date"))])
# prob use data_date - initial_license_date

lapply(dat, function(x) {
  sum(is.na(x))
}) %>% 
  unlist() %>% 
  sort()


sum(!is.na(dat$start_date)) # 1927 

# choropleth of number of facilities in each county
# identify longest running facility 
# practice splitting names into Last/First/Middle
# clean up licensed_certified column 
# license_status_description
# entity_description_type
# capacity
# fac_type_code
# fac_fdr
# fac_status_type_code 

dat %>% 
  select(licensed_certified,
         entity_type_description,
         fac_type_code,
         fac_fdr,
         fac_status_type_code) %>% 
  lapply(table)


summary(dat$capacity) # no NA!! yass
# so we could map and size by capacity 


# maps ----


# county_sf <- tigris::counties(state = "CA") %>% 
#   clean_names() %>% 
#   st_transform(crs = 4326) %>% 
#   select(geoid, name)
# 
# st_write(obj = county_sf, dsn = "data/county_borders.shp")

county_sf <- st_read(dsn = "data", layer = "county_borders")
st_crs(county_sf) # 4326

msg <- paste("<b>County:</b>", county_sf$name)

# msg for facilities
msg <- paste("<b>Location:</b>", dat$facname,
             "<br>",
             # "<b>Capacity:</b>", dat$capacity,
             # "<br>",
             "<b>Type:</b>", dat$fac_fdr,
             "<br>",
             "<b>Entity:</b>", dat$entity_type_description)

blue <- "#4f75a3"
orange <- '#e89505' #'#c47916'

leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addCircleMarkers(data = dat,
                   lng = ~longitude, 
                   lat = ~latitude,
                   #radius = ~sqrt(sqrt(capacity + 1)),
                   radius = 2,
                   fillColor = orange, 
                   stroke = F,
                   fillOpacity = 0.3) %>% 
  addPolygons(data = county_sf,
              color = blue,
              fillColor = "white", 
              fillOpacity = 0.15,
              weight = 1,
              popup = msg) 


# district name ----

dat %>% 
  group_by(county_name, district_name) %>% 
  count() %>% 
  print(n = 164)

unique(dat$district_name)
# [1] "SANTA ROSA"               "EAST BAY"                
# [3] "SACRAMENTO"               "SAN JOSE"                
# [5] "STOCKTON"                 "FRESNO"                  
# [7] "VENTURA"                  "LA HHA/HOSPICE"          
# [9] "ORANGE"                   "LA ACUTE/ANCILLARY"      
# [11] "SAN BERNARDINO"           "SAN DIEGO"               
# [13] "RIVERSIDE"                "BAKERSFIELD"             
# [15] "SAN FRANCISCO"            "CHICO"                   
# [17] "STATE FACILITIES SECTION" "LA ICF/DD/CLINICS"       
# [19] "LA REGION 2"              "LA REGION 3"             
# [21] "LA REGION 1" 

dat %>% 
  mutate(district_collapsed = case_when(str_detect(district_name, "^LA\\s") ~ "LA",
                                       T ~ district_name)) %>% 
  group_by(district_collapsed) %>% 
  count() %>% 
  arrange(n)

# district_collapsed           n
# <chr>                    <int>
# 1 STATE FACILITIES SECTION    55
# 2 STOCKTON                    58
# 3 SAN FRANCISCO              360
# 4 CHICO                      366
# 5 BAKERSFIELD                402
# 6 SAN JOSE                   530
# 7 SANTA ROSA                 530
# 8 RIVERSIDE                  560
# 9 VENTURA                    622
# 10 SACRAMENTO                 693
# 11 FRESNO                     695
# 12 SAN BERNARDINO             782
# 13 EAST BAY                   858
# 14 SAN DIEGO                  901
# 15 ORANGE                    1116
# 16 LA                        6506

# too many categories to use as coloring... maybe use as a filter



# barcharts ----  


# ex: GENERAL ACUTE CARE HOSPITAL
ggplot(dat) +
  geom_bar(aes(x = fac_fdr)) +
  coord_flip()

# ex: LIMITED LIABILITY COMPANY
ggplot(dat) +
  geom_bar(aes(x = entity_type_description)) +
  coord_flip()
# collapse categories 
# then, convert to factor
# order by frequency if possible :/




# county summary ---- 


county_summary <- function(county) {
  
  county_upper <- str_to_upper(county)
  df <- dat %>% filter(county_name == county_upper)
  
  tibble::tibble(
    "County" = str_to_title(county),
    "Facilities" = nrow(df),
    "Avg Capacity" = round(mean(df$capacity, na.rm = T)),
    # could add count of facilities with capacity > 0
    "Birthing Facilities" = sum(!is.na(df$birthing_facility_flag))
    )
  
}

county_summary('kings')
lapply(c('kings', 'fresno'), county_summary)

# currently wide format, I'd prefer long tho 

DT::datatable(county_summary('kings'))


# 7/19/23 ---- 


dat %>% 
  # pulling out non-expired facilities
  filter(license_expiration_date > date("2023-06-15")) %>% 
  group_by(license_expiration_date) %>% 
  count() %>% 
  ggplot() +
  # this is fugly
  geom_line(aes(x = license_expiration_date, y = n))


# 1435 expired licenses
dat %>% 
  filter(license_expiration_date < date("2023-06-15")) %>% 
  nrow()



# 7/23/23 ---- 



dat$quarter_year <- quarter(dat$license_expiration_date, with_year = T)

dat %>% 
  filter(quarter_year >= 2022.1) %>% 
  mutate(quarter_string = paste0(
    str_extract(quarter_year, "^\\d{4}"), 
    " Q", 
    str_extract(quarter_year, "(?<=\\.)\\d")
    )) %>% 
  ggplot() +
  geom_bar(aes(x = quarter_string)) +
  theme_minimal() +
  labs(y = "Number of Facilities",
       title = "License Expiration Date of Health Facilites") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank()) 


class(dat$quarter_year) # "numeric"



