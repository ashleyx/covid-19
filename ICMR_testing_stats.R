sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

# setting up icmr stats repository -----------------------------------

if(!dir.exists("../datameet_covid19")){
  system("cd .. && git clone https://github.com/datameet/covid19.git datameet_covid19")
}
system("cd ../datameet_covid19 && git pull ")

#  analysis ---------------------------------------------------------------

data <- read_json("../datameet_covid19/data/icmr_testing_status.json", simplifyVector = TRUE)

data$rows$value %>% transmute(report_time,samples, individuals,
                              difference = samples - individuals)

data$rows$value %>%
  mutate(percent_positive = (100*confirmed_positive/samples) %>% round(digits = 1),
         date = as.Date(report_time,"%Y-%m-%d")) %>% 
  filter(!duplicated(date)) %>% 
  ggplot(aes(x= date , y= percent_positive)) + geom_histogram(stat = "identity") +
  geom_text(aes(x= date, y = percent_positive + 1, label = percent_positive)) +
  labs(title = "% of tests positive, cumulative",
       caption = "Data: ICMR notifications; maintained at github:datameet/covid19") +
  theme_bw()

data$rows$value %>%
  mutate(date = as.Date(report_time,"%Y-%m-%d"),
         new_tests = samples - lag(samples)) %>% 
  ggplot(aes(x=date, y = new_tests)) + geom_histogram(stat = "identity") +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x")+
  labs(title = "Number of new tests done at each ICMR notification",
       subtitle = "blue line is trend of increase in testing capacity",
       caption = "Data: ICMR notifications; maintained at github:datameet/covid19") +
  theme_bw()
