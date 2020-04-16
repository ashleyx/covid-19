# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite","ggtext"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)


# data import -------------------------------------------------------------

# Data updates throughout the day. The total tally for current day only refects post midnight
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

# plots ----------------------------------------------------------------


# daily new cases ---------------------------------------------------------
data_india_raw %>% group_by(dateannounced) %>% 
  summarise(count = length(dateannounced)) %>% 
  ggplot(aes(x=dateannounced, y = count))+
  geom_histogram(stat="identity")+
  geom_text(aes(label = count, y= (5 + count)), size =2)+
  theme_bw() +
  scale_x_date(date_breaks = "1 day" , date_labels = "%d/%m/'%y")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "New Cases:India",
       caption = "Data: covid19india.org API")

# State-wise confirmed cases ---------------------------------------------------------
data_india_state %>% ggplot(aes(x= date , y = confirmed, group = detectedstate))+geom_line() +
  facet_wrap(~detectedstate)

# trends of cumulative for top states ---------------------------------------------------------
data_india_state %>% group_by(detectedstate) %>%
  filter(max(confirmed) > 200, date > "2020-04-01") %>%  
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
         percent_increase = round(100*daily/lag(total), digits = 0),
         doubling_time = round(log(x = 2,base = (1+percent_increase/100)), digits = 1)) %>% 
  filter(date > "2020-03-15") %>% 
  ggplot(aes(x= date , y = percent_increase)) + geom_histogram(stat = "identity") +
  geom_text(aes(x= date, y = percent_increase + 4, label = paste0(percent_increase)), color = "#850c0c") +
  geom_text(aes(x= date, y = percent_increase + 2, label = paste0("(",doubling_time,")")), color = "#30119e") +  theme_bw() +
  labs(title = "<span style='color:#850c0c'>Percentage increase over previous day total (%)</span> ",
       subtitle = "<span style='color:#30119e'>Effective Doubling Time (days)</span>",
       caption = "Data: covid19india.org API") +
  theme(plot.subtitle = element_markdown(),
    plot.title = element_markdown())


# National doubling time ---------------------------------------------------------
data_india_state %>% group_by(date) %>% 
  summarize(daily = sum(confirmed)) %>% 
  mutate(total = cumsum(daily),
         percent_increase = round(100*daily/lag(total), digits = 0),
         doubling_time = round(log(x = 2,base = (1+percent_increase/100)), digits = 1)) %>% 
  filter(date > "2020-03-01") %>% 
  ggplot(aes(x= date , y = doubling_time)) + 
  geom_path() +
  gghighlight(label_key = doubling_time)+
  annotate("rect", xmin = as.Date("2020-03-22"), xmax = as.Date("2020-04-16"), ymin = -Inf, ymax = Inf, 
           alpha = .5)+
  annotate("text",x = as.Date("2020-03-26"),y = 1 , label = "Start of Lockdown") +
  theme_bw()+ scale_x_date(expand = c(0,0))+
  labs(title = "<span style='color:#30119e'>Effective Doubling Time (days)</span>" ,
       caption = "Data: covid19india.org API") +
  theme(plot.title = element_markdown())
