# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr",
          "ggplot2","gghighlight","jsonlite","tidyr","EpiEstim",
          "XML","ggtext","patchwork"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)


#  -------------------------------------------------------------
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>%
  mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>%
  na.omit()

population_data <-readHTMLTable(readLines("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population"))[[2]] %>%
  {
    population_data <- data.frame(.[-1,])
    colnames(population_data) <- .[1,]
    population_data %>%
      transmute(state = `State or union territory` %>% gsub("\\[c\\]","",x = .),
                pop = gsub("[^0-9]","",population_data$`Population(%)`) %>%   as.numeric())
    
  }

data_states_all <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily 

state_mapping <- data.frame(abbrv = strsplit("an,ap,ar,as,br,ch,ct,dd,dn,dl,ga,gj,hr,hp,jk,jh,ka,kl,la,ld,mp,mh,ml,mn,mz,nl,or,py,pb,rj,sk,tn,tg,tr,up,ut,wb", split = ",")[[1]],
                            full = strsplit("Andaman and Nicobar Islands,Andhra Pradesh,Arunachal Pradesh,Assam,Bihar,Chandigarh,Chhattisgarh,Daman and Diu,Dadra and Nagar Haveli,Delhi,Goa,Gujarat,Haryana,Himachal Pradesh,Jammu and Kashmir,Jharkhand,Karnataka,Kerala,Lakshadweep,Ladakh,Madhya Pradesh,Maharashtra,Meghalaya,Manipur,Mizoram,Nagaland,Odisha,Puducherry,Punjab,Rajasthan,Sikkim,Tamil Nadu,Telangana,Tripura,Uttar Pradesh,Uttarakhand,West Bengal",split =",")[[1]])

dir.create(paste0("statewise_plot/",Sys.Date()),recursive = TRUE)

date_start <- as.Date("2020-03-29")

# in progress -------------------------------------------------------------
# 
data_states_all[,!colnames(data_states_all )%in% c("date","status")] %>% apply(2,function(i) sum(as.integer(i),na.rm = TRUE)) %>% 
  { 
    names(.)[. > 400] 
  } %>% {
    state_mapping$full[state_mapping$abbrv %in% .]
  } %>% 
  lapply(function(state){
    print(state)
    
    data_incidence_state <- data_india_raw %>% filter(detectedstate == state) %>%
      group_by(dateannounced) %>%
      summarize(I = length(dateannounced)) %>%
      transmute(dates = dateannounced, I) %>%
      complete(dates = seq.Date(min(dates), max(dates), by="day")) %>%
      mutate(I = ifelse(is.na(I),0,I)) %>%
      filter(dates > date_start)
    
    result_state <- estimate_R(incid = as.data.frame(data_incidence_state),
                               method = "parametric_si",
                               config = make_config(list(mean_si = 7,std_si = 3)))
    
    plot_Re <- data.frame(date = data_incidence_state$dates[-1:-7],
                          Re = result_state$R$`Mean(R)`,
                          Re_ul = result_state$R$`Quantile.0.95(R)`,
                          Re_ll = result_state$R$`Quantile.0.05(R)`) %>% 
      ggplot(aes(x= date, y = Re)) +
      geom_line() +
      scale_x_date(expand = c(0,0))+
      geom_ribbon(aes(ymin = Re_ll , ymax = Re_ul), alpha = 0.2)+
      geom_hline(yintercept = 1, linetype = 2) + 
      theme_bw() + ggtitle("Re: Effective Reproduction Number") +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
    
    
    
    plot_incidence <- data_india_raw %>%
      filter(detectedstate == state) %>% 
      group_by(dateannounced) %>%
      summarise(count = length(dateannounced)) %>%
      filter(dateannounced > date_start + 8) %>%
      ggplot(aes(x=dateannounced, y = count))+
      geom_histogram(stat="identity")+
      theme_bw() +
      scale_y_continuous(expand = c(-Inf,1))+
      scale_x_date(expand = c(0,0))+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()) +
      labs(title = "Daily New Cases")
    
    
    # extrapolating -------------------------------------------------------------
    
    data_state <- data_states_all %>%
      melt(id.vars = c("status","date")) %>%
      transmute(State = variable,
                date = as.Date(date,"%d-%b-%y"),
                status,
                count = as.integer(value)) %>%
      filter(State == state_mapping$abbrv[state_mapping$full == state]) %>% {
        tmp <- .
        tmp$count[is.na(tmp$count)] <- 0
        tmp
      } %>% 
      group_by(status) %>% 
      mutate(count = cumsum(count)) 
    
    
    avg_increase <- data_state %>% 
      group_by(status) %>% 
      mutate(percent = (count- lag(count))/lag(count)) %>% 
      filter(date > Sys.Date() - 8) %>% group_by(status) %>% 
      summarise(percent = mean(percent))
    
    if(avg_increase$percent[avg_increase$status == "Recovered"] > avg_increase$percent[avg_increase$status == "Confirmed"]){
      avg_increase$percent[avg_increase$status == "Recovered"] <- avg_increase$percent[avg_increase$status == "Confirmed"]
    }
    
    extrapolate <- function(status){
      sapply(0:6, function(i){
        x <- data_state$count[(data_state$date == max(data_state$date)) & data_state$status == status ]
        x * (1 + avg_increase$percent[avg_increase$status == status])^i
      })
    }
    
    
    data_extra <- c("Confirmed","Recovered","Deceased") %>% 
      sapply(extrapolate) %>% as.data.frame() %>%  mutate(date = Sys.Date()+c(-1:5))
    
    data_extra$Recovered <- sapply(1:nrow(data_extra),function(i){
      min(data_extra$Confirmed[i] - data_extra$Deceased[i],data_extra$Recovered[i])
    })
    
    data_extra %<>%
      melt(id.vars = "date") %>% transmute(date,
                                           status = variable, 
                                           count = value)
    
    plot_cumulative <- ggplot(data_state,aes(x= date, y = count , color = status)) +
      geom_line() +
      geom_line(data = data_extra, aes(x= date, y = count , color = status), linetype = 2)+
      # scale_y_log10()+
      theme_bw()+
      labs(title = "Trajectory of Cumulative Counts",
           subtitle = paste0("<span style='color:#F8766E'>Confirmed</span>, ",
                             "<span style='color:#619CFF'>Recovered</span>, ",
                             "<span style='color:#00BA38'>Deceased</span>.",
                             "Dashed lines are extrapolation from 7 day average growth")) +
      theme(plot.subtitle = element_markdown(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none")
    
    final_plot <- plot_cumulative + (plot_incidence/ plot_Re) +
      plot_layout(widths = c(4,2)) + plot_annotation(title = state)
    
    ggsave(paste0("statewise_plot/",Sys.Date(),"/",Sys.Date(),"_",gsub(" ","-",state),".png"),
           plot = final_plot,
           width = 13 , height = 13)
  }) %>% invisible()
