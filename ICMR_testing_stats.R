sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

data <- read_json("../datameet_covid19/data/icmr_testing_status.json", simplifyVector = TRUE)

tmp %>% transmute(report_time,samples, individuals,
                  difference = samples - individuals)
