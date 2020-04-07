# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)


# data import -------------------------------------------------------------


# Data updates throughout the day. The total per day tally only refects post midnight
data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>% 
  mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>% 
  na.omit()

data_india_district <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
  data_india_raw %>% filter(dateannounced <= d) %>%
    group_by(detecteddistrict) %>% 
    summarise(confirmed = length(dateannounced)) %>% 
    mutate(date = d)
}) %>%  Reduce(f = rbind)
    
data_india_state <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
  data_india_raw %>% filter(dateannounced <= d) %>%
    group_by(detectedstate) %>% 
    summarise(confirmed = length(dateannounced)) %>% 
    mutate(date = d)
}) %>%  Reduce(f = rbind)

data_india_raw %>% skim()



# plotting ----------------------------------------------------------------


# daily new cases ---------------------------------------------------------
data_india_raw %>% group_by(dateannounced) %>% 
  summarise(count = length(dateannounced)) %>% 
  ggplot(aes(x=dateannounced, y = count))+
  geom_histogram(stat="identity")+
  geom_text(aes(label = count, y= (5 + count)), size =2)+
  theme_bw() +
  scale_x_date(date_breaks = "1 day" , date_labels = "%d/%m/'%y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("New Cases:India")

# Total cases ---------------------------------------------------------
data_india_raw %>% 
  group_by(dateannounced,currentstatus) %>% 
  summarise(new_cases = length(dateannounced) ) %>% 
  group_by(currentstatus) %>% 
  mutate(total = cumsum(new_cases)) %>%
  ggplot(aes(x=dateannounced, y = total, color = currentstatus))+
  geom_line(aes(group = currentstatus))+
  gghighlight(label_key = total,use_direct_label = TRUE) +
  guides(colour = guide_legend("legend" ,override.aes = aes(label = "|")))+
  theme_bw() +
  scale_x_date(date_breaks = "1 day" , date_labels = "%d/%m/'%y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Cases:India")

# State-wise confirmed cases ---------------------------------------------------------
data_india_state %>% ggplot(aes(x= date , y = confirmed, group = detectedstate))+geom_line() +
  facet_wrap(~detectedstate)

# trens of cumulative for top states ---------------------------------------------------------
data_india_state %>% group_by(detectedstate) %>%
  filter(max(confirmed) > 200, date > "2020-03-15") %>%  
  ggplot(aes(x= date , y = confirmed,color = detectedstate, group = detectedstate))+
  geom_line(alpha = 0.6) +
  gghighlight(max_highlight = 20) + theme_bw() 

# state-wise percent new cases ---------------------------------------------------------
data_india_state %>% group_by(detectedstate) %>% 
  mutate(total = cumsum(confirmed),
         percent_increase = round(100*confirmed/total, digits = 3)) %>% 
  ggplot(aes(x= date , y = percent_increase)) + geom_histogram(stat = "identity") +
  facet_wrap(~detectedstate)

# National percent new cases ---------------------------------------------------------
data_india_state %>% group_by(date) %>% 
  summarize(daily = sum(confirmed)) %>% 
  mutate(total = cumsum(daily),
         percent_increase = round(100*daily/lag(total), digits = 0)) %>% 
  filter(date > "2020-03-01") %>% 
  ggplot(aes(x= date , y = percent_increase)) + geom_histogram(stat = "identity") +
  geom_text(aes(x= date, y = percent_increase + 10, label = percent_increase)) +
  theme_bw() + ggtitle("% increase over previous day")

