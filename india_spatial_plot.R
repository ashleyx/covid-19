
sapply( c("raster","magrittr","dplyr","ggplot2","jsonlite","rayshader","rgdal"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

# setting up map repository -----------------------------------

if(!dir.exists("../datameet_maps")){
  system("cd .. && git clone https://github.com/datameet/maps.git datameet_maps")
}
system("cd ../datameet_maps && git pull ")

# data import and processing ----------------------------------------------


gadm_india_states <- rgdal::readOGR("../datameet_maps/States/Admin2.dbf")

gadm_india_districts <- rgdal::readOGR("../datameet_maps/Districts/Census_2011/2011_Dist.dbf")

data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>% 
  mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>% 
  na.omit()

data_india_raw$detectedstate[data_india_raw$detectedstate == "Andaman and Nicobar Islands"] <- "Andaman and Nicobar Island"
data_india_raw$detectedstate[data_india_raw$detectedstate == "Delhi"] <- "NCT of Delhi"
data_india_raw$detectedstate[data_india_raw$detectedstate == "Ladakh"] <- "Jammu and Kashmir"

data_india_state <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
  data_india_raw %>% filter(dateannounced <= d) %>%
    group_by(detectedstate) %>% 
    summarise(confirmed = length(dateannounced)) %>% 
    mutate(date = d)
}) %>%  Reduce(f = rbind)

data_india_district <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
  data_india_raw %>% filter(dateannounced <= d) %>%
    group_by(detecteddistrict) %>% 
    summarise(confirmed = length(dateannounced)) %>% 
    mutate(date = d)
}) %>%  Reduce(f = rbind)

# state level -----------------------------------------------------


gadm_india_df <- fortify(gadm_india_states)

gadm_india_states@data %<>% mutate(id = rownames(.)) %>% left_join(data_india_state %>%
                                                                     group_by(detectedstate) %>% filter(date == max(date)),
                                                                   by = c("ST_NM" = "detectedstate"))
gadm_india_df %<>% left_join(gadm_india_states@data , by = "id")


plot_state <- gadm_india_df %>% mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill =
                     confirmed), color = "grey", size = 0.25) +
  scale_fill_viridis_c(option = "A")

plot_gg(plot_state, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)


# district level ----------------------------------------------------------

gadm_india_df <- fortify(gadm_india_districts)

gadm_india_districts@data %<>% mutate(id = rownames(.))%>% 
  left_join(data_india_district %>%
              group_by(detecteddistrict) %>% 
              filter(date == max(date)),
            by = c("DISTRICT" = "detecteddistrict"))
gadm_india_df %<>% left_join(gadm_india_districts@data , by = "id")


plot_district <- gadm_india_df %>% mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group,
                   fill = confirmed),
               color = "grey", size = 0.25) +
  scale_fill_viridis_c()

plot_gg(plot_district, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)


