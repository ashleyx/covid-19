# library imports ----------------------------------------------------------

options(stringsAsFactors = FALSE)
sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)


# data import -------------------------------------------------------------

data_india <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>% 
  melt(id.vars = c("status","date")) %>% 
  transmute(state = variable,
            date = as.Date(date,"%d-%b-%y"),
            status,
            count = as.integer(value)) %>%
  filter(state != "tt") # removing total tally
data_india$count[is.na(data_india$count)] <- 0

state_mapping <- data.frame(abbrv = strsplit("an,ap,ar,as,br,ch,ct,dd,dn,dl,ga,gj,hr,hp,jk,jh,ka,kl,la,ld,mp,mh,ml,mn,mz,nl,or,py,pb,rj,sk,tn,tg,tr,up,ut,wb", split = ",")[[1]],
           full = strsplit("Andaman and Nicobar Islands,Andhra Pradesh,Arunachal Pradesh,Assam,Bihar,Chandigarh,Chhattisgarh,Daman and Diu,Dadra and Nagar Haveli,Delhi,Goa,Gujarat,Haryana,Himachal Pradesh,Jammu and Kashmir,Jharkhand,Karnataka,Kerala,Lakshadweep,Ladakh,Madhya Pradesh,Maharashtra,Meghalaya,Manipur,Mizoram,Nagaland,Odisha,Puducherry,Punjab,Rajasthan,Sikkim,Tamil Nadu,Telangana,Tripura,Uttar Pradesh,Uttarakhand,West Bengal",split =",")[[1]])

data_india$state %<>% as.character() %>% 
  sapply(function(i) state_mapping$full[state_mapping$abbrv ==i])


# plots ----------------------------------------------------------------


data_india %>% group_by(date,status) %>% 
  summarise(count = sum(count)) %>% 
  group_by(status) %>% 
  mutate(total = cumsum(count)) %>% 
  skim_tee() %>% 
  ggplot(aes(x=date, y = total, color = status)) +
  geom_line(aes(group = status))+
  gghighlight(label_key = total,use_direct_label = TRUE) +
  guides(colour = guide_legend("legend" ,override.aes = aes(label = "|")))+
  theme_bw() +
  scale_x_date(date_breaks = "3 days" , date_labels = "%b %d")+
  scale_y_continuous(trans="log10") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Cases:India",
       caption = "Data: covid19india.org API")

data_india %>% group_by(date,status,state) %>% 
  summarise(count = sum(count)) %>% 
  group_by(status,state) %>% 
  mutate(total = cumsum(count)) %>% 
  group_by(state) %>% filter(max(total) > 100) %>% 
  ggplot(aes(x=date, y = total, color = status)) +
  geom_line(aes(group = status))+ 
  facet_wrap(~state)+
  theme_bw() +
  scale_x_date(date_breaks = "3 days" , date_labels = "%b %d")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "State-wise Cumulative Cases",
       caption = "Data: covid19india.org API")

data_india %>% filter(status == "Confirmed") %>% 
  group_by(date,state) %>% 
  summarise(count = sum(count)) %>% 
  group_by(state) %>% 
  mutate(total = cumsum(count)) %>% 
  group_by(state) %>% filter(max(total) > 300) %>% 
  ggplot(aes(x= date , y = total,color = state, group = state))+
  geom_line(alpha = 0.6) +
  gghighlight(max_highlight = 20) + theme_bw() +
  labs(title = "State-wise Cumulative Confirmed Cases",
       caption = "Data: covid19india.org API") 


  
