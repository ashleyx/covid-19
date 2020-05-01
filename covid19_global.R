
# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

# setting up repository and/or updating -----------------------------------

if(!dir.exists("../COVID-19")){
  system("cd .. && git clone https://github.com/CSSEGISandData/COVID-19.git")
}
system("cd ../COVID-19 && git pull ")


# data import -------------------------------------------------------------

data <- list.files("../COVID-19/csse_covid_19_data/csse_covid_19_time_series",full.names = TRUE) %>%
  grep("_global.csv$", x = ., value = TRUE, perl = TRUE) %T>%
  print() %>% 
  lapply(function(x){
    read_csv(x) %>% 
      melt(data = . , id.vars = colnames(.)[1:4]) %>%
      transmute(State = `Province/State`,
                Country = `Country/Region`,
                Lat , Long,
                Date = as.Date(variable, "%m/%d/%y"),
                Status = gsub(".*covid19_|_global.csv","",x),
                Count = value)
  }) %>% Reduce(f = rbind, x = .)

# function definitions ----------------------------------------------------

plot_country_total <- function(country, scale = NA, skim = TRUE){
  if(is.na(scale)){
    scale <- ifelse(max(data$Count[data$Country == country]) < 1000,"linear","log")
  }
  if(skim){
    data %>% 
      filter(Country == country) %>% skim() %>% print()
  }
  plot <- data %>% 
    filter(Country == country) %>% group_by(Status, Date) %>% summarise(Count = sum(Count)) %>%
    ggplot(aes(x= Date , y = Count , color = Status )) + 
    geom_line() +
    gghighlight(label_key = Count,use_direct_label = TRUE) +
    guides(colour = guide_legend("legend" ,override.aes = aes(label = "|")))+
    theme_bw() + 
    scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/'%y")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),) +
    labs(title = country,
         subtitle = "Total Cases",
         caption = "Data: JHU CSSE github.com/CSSEGISandData/COVID-19")
  if(scale == "log"){
    return(plot + scale_y_log10())
  }else{
    return(plot)
  }
}

plot_country_new <- function(country, skim = TRUE){

  if(skim){
    data %>% 
      filter(Country == country) %>% skim() %>% print()
  }
  plot <-  data %>% 
    filter(Country == country, Status == "confirmed") %>%
    group_by(Status, Date) %>% 
    summarise(Count = sum(Count)) %>%
      group_by(Status) %>% 
      mutate(new_cases = Count - lag(Count, default = 0, order_by = Date)) %>%
    ggplot(aes(x= Date , y = new_cases)) + 
    geom_histogram(stat = "identity") + 
    geom_text(aes(label = new_cases, y= (max(new_cases)/10 + new_cases)))+
    theme_bw() +
    scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/'%y")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title = country,
         subtitle = "New Cases",
         caption = "Data: JHU CSSE github.com/CSSEGISandData/COVID-19")
    return(plot)
  
}


# testing  ----------------------------------------------------------------


unique(data$Country) %>% sort() %>% print()

plot_country_total("Oman", scale = "log")

plot_country_new("India")

data %>% filter(Country == "India" , Date == max(Date)) %>%
  group_by(Status, Date) %>% 
  summarise(Count = sum(Count))
