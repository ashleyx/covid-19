chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) # stolen from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r

# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite","ggtext"),
        function(x){
            if(!x %in% rownames(installed.packages())){
                install.packages(x)
            }
            suppressPackageStartupMessages(library(x, character.only = TRUE))

            x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------------
# DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

# This is the file used for testing numbers  ------------------------------
data_testing_raw <- read_json("https://api.covid19india.org/data.json",simplifyVector = TRUE)
data_testing_national <- data_testing_raw$tested  %>%
                     transmute(total = as.integer(totalsamplestested),
                               date = as.Date(updatetimestamp,"%d/%m/%Y"),
                               new_tested = total - lag(total,order_by = date))
cat("https://api.covid19india.org/data.json\n")

data_testing_state <- read_csv("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv") %>%
    mutate("Updated On" = as.Date(`Updated On`,"%d/%m/%Y")) %>%
    filter(`Updated On` > "2020-03-01")

cat("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv\n")
# This file is used for semi-log plot -------------------------------------

data_india <- data_testing_raw$cases_time_series %>% mutate(date = as.Date(date,"%d %b")) %>%
    mutate_if(is.character,as.integer)

data_state <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    filter(state != "tt") # removing total tally
data_state$count[is.na(data_state$count)] <- 0

state_mapping <- data.frame(abbrv = strsplit("an,ap,ar,as,br,ch,ct,dd,dn,dl,ga,gj,hr,hp,jk,jh,ka,kl,la,ld,mp,mh,ml,mn,mz,nl,or,py,pb,rj,sk,tn,tg,tr,up,ut,wb,un", split = ",")[[1]],
                            full = strsplit("Andaman and Nicobar Islands,Andhra Pradesh,Arunachal Pradesh,Assam,Bihar,Chandigarh,Chhattisgarh,Daman and Diu,Dadra and Nagar Haveli,Delhi,Goa,Gujarat,Haryana,Himachal Pradesh,Jammu and Kashmir,Jharkhand,Karnataka,Kerala,Lakshadweep,Ladakh,Madhya Pradesh,Maharashtra,Meghalaya,Manipur,Mizoram,Nagaland,Odisha,Puducherry,Punjab,Rajasthan,Sikkim,Tamil Nadu,Telangana,Tripura,Uttar Pradesh,Uttarakhand,West Bengal,Unknown",split =",")[[1]])

data_state$state %<>% as.character() %>%
    sapply(function(i) state_mapping$full[state_mapping$abbrv ==i])

data_state %<>% filter(state != "Unknown")

cat("https://api.covid19india.org/states_daily.json\n")


# Data updates throughout the day. The total tally for previous day only refects post midnight

# This was the file used for most plots ----------------------------------------------------------------------

data_india_raw <- data_india_raw <- lapply(1:2, function(i){
    read_json(paste0("https://api.covid19india.org/raw_data",i,".json"),simplifyVector = TRUE)$raw_data %>%
        transmute(dateannounced,
                  detectedstate,
                  detecteddistrict,
                  numcases = 1)
}) %>%
    Reduce(f = rbind, x = .) %>%
    rbind(lapply(3:4, function(i){
        read_json(paste0("https://api.covid19india.org/raw_data",i,".json"),simplifyVector = TRUE)$raw_data %>%
            filter(currentstatus == "Hospitalized") %>%
            transmute(dateannounced,
                      detectedstate,
                      detecteddistrict,
                      numcases)
    }) %>%  Reduce(f = rbind, x = .)) %>%
    mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y"),
           numcases = as.integer(numcases))  %>%
    filter(!is.na(dateannounced))

data_india_district <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
    data_india_raw %>% filter(dateannounced <= d) %>%
        group_by(detecteddistrict) %>%
        summarise(confirmed = sum(numcases)) %>%
        mutate(date = d)
}) %>%  Reduce(f = rbind)

data_india_state <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
    data_india_raw %>% filter(dateannounced <= d) %>%
        group_by(detectedstate) %>%
        summarise(confirmed = sum(numcases)) %>%
        mutate(date = d)
}) %>%  Reduce(f = rbind)

cat("https://api.covid19india.org/raw_data[1-4]json")


# # population data ---------------------------------------------------------
#
# population_data <-readHTMLTable(readLines("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"))[[2]] %>%
#     {
#         population_data <- data.frame(.[-1,])
#         colnames(population_data) <- .[1,]
#         population_data %>%
#             transmute(state = `State or union territory` %>% gsub("\\[c\\]","",x = .),
#                       pop = gsub("[^0-9]","",population_data$`Population(%)`) %>%   as.numeric())
#
#     }

cat("All done")
