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
