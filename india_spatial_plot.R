
sapply( c("raster","magrittr","dplyr","ggplot2","jsonlite","rayshader"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

# data import and processing ----------------------------------------------


gadm_india_states <- getData("GADM", country = "India" , level = 1)
  # gadm_india_states <- rgdal::readOGR("IND_adm/IND_adm1.dbf") 

gadm_india_districts <- getData("GADM", country = "India" , level = 2)

data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>% 
  mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>% 
  na.omit()

data_india_raw$detectedstate[data_india_raw$detectedstate == "Andaman and Nicobar Islands"] <- "Andaman and Nicobar"
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
                                                                   by = c("NAME_1" = "detectedstate"))
gadm_india_df %<>% left_join(gadm_india_states@data , by = "id")


plot_state <- gadm_india_df %>% mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill =
                     confirmed), color = "black", size = 0.25) +
  scale_fill_viridis_c(option = "A")

plot_gg(plot_state, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)


# district level ----------------------------------------------------------

gadm_india_df <- fortify(gadm_india_districts)

gadm_india_districts@data %<>% mutate(id = rownames(.))%>% 
  left_join(data_india_district %>%
              group_by(detecteddistrict) %>% 
              filter(date == max(date)),
            by = c("NAME_2" = "detecteddistrict"))
gadm_india_df %<>% left_join(gadm_india_districts@data , by = "id")


plot_district <- gadm_india_df %>% mutate(confirmed = ifelse(is.na(confirmed),0,confirmed)) %>% 
  ggplot(aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group,
                   fill = confirmed),
               color = "grey", size = 0.25) +
  scale_fill_viridis_c()

plot_gg(plot_district, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)


