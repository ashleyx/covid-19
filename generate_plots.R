source("load_data.R")

#  -------------------------------------------------------------
#  PLOTS -------------------------------------------------------------
#  -------------------------------------------------------------

# daily new cases ---------------------------------------------------------
last_plot <-  data_india %>% 
    melt(id.vars = "date")%>% 
    filter(variable == "dailyconfirmed",
                       date > Sys.Date() - 30) %>% 
    ggplot(aes(x=date, y = value))+
    geom_histogram(stat="identity")+
    theme_bw() +
    scale_y_continuous(expand = c(-Inf,1))+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = "Daily New Cases, India",
         caption = "Data: covid19india.org API")

ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_daily_new_cases.png"),
       plot = last_plot ,
       height = 8, width = 8 )

# trends of cumulative for top states ---------------------------------------------------------
# paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),
#        "_statewise_confirmed_",1:5,".png") %>%
#     sapply(function(i){
#         if(file.exists(i))file.remove(i)
#     })
#
# data_india_state %>% group_by(detectedstate) %>%
#     filter(date == max(date)) %>%  filter(detectedstate != "") %>%
#     arrange(confirmed) %>%  .$detectedstate %>%
#     chunk2(5) %>%
#     lapply(function(i){
#
#         last_plot <- data_india_state %>% group_by(detectedstate) %>%
#             filter(date > (Sys.Date()-28)) %>%
#             ggplot(aes(x= date , y = confirmed,color = detectedstate, group = detectedstate))+
#             geom_line(alpha = 0.6) +
#             gghighlight(detectedstate %in% i,
#                         use_group_by = FALSE,
#                         max_highlight = 20) +
#             theme_bw()+
#             scale_y_continuous(expand = c(-Inf,1))+
#             scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/%Y",
#                          expand = c(0,0))+
#             xlab("Date (dd/mm/yyyy)") +
#             theme(axis.text.x = element_text(angle = 90, hjust = 1,),
#                   panel.grid.major.x = element_blank(),
#                   panel.grid.minor.x = element_blank(),
#                   axis.title.y = element_blank()) +
#             labs(title = "Cumulative Confirmed Cases",
#                  caption = "Data: covid19india.org API")
#         for(j in 1:5){
#             if(!file.exists(paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),
#                                    "_statewise_confirmed_",j,".png"))){
#                 break
#             }
#         }
#         ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),
#                                  "_statewise_confirmed_",j,".png"),
#                plot = last_plot ,
#                height = 8, width = 8 )
#
#     })
#


# National percent new cases + doubling time ---------------------------------------------------------
last_plot <- data_india %>% 
    mutate(percent_increase = round(100*dailyconfirmed/lag(totalconfirmed), digits = 1),
           doubling_time = round(log(x = 2,base = (1+percent_increase/100)), digits = 1)) %>%
    filter(date > Sys.Date()-21) %>%
    ggplot(aes(x= date , y = percent_increase)) + geom_histogram(stat = "identity") +
    geom_text(aes(x= date, y = percent_increase + 1, label = paste0(percent_increase)), color = "#850c0c") +
    geom_text(aes(x= date, y = percent_increase + 2, label = paste0("(",doubling_time,")")), color = "#30119e") +
    theme_bw()+
    labs(title = "<span style='color:#30119e'>Effective Doubling Time (days)</span>",
         subtitle = "<span style='color:#850c0c'>Percentage increase over previous day total (%)</span> ",
         caption = "Data: covid19india.org API") +
    xlab("Date (dd/mm/yyyy)") +
    theme(plot.subtitle = element_markdown(),
          plot.title = element_markdown(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank())

ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_national_doubling_time.png"),
       plot = last_plot ,
       height = 8, width = 8 )


# Plot on Testing Numbers -----------------------------------------------------------

last_plot <- data_testing_national %>% 
    ggplot(aes(x = date, y = new_tested)) + geom_histogram(stat = "identity") +
    ggtitle("Daily Tested Total") + 
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())

ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_ICMR_testing.png"),
       plot = last_plot ,
       height = 10, width = 10 )



# National Semi-log plot --------------------------------------------------
last_plot <- data_india %>% 
    melt(id.vars = "date") %>% 
    filter(variable %in% c("totalconfirmed","totalrecovered","totaldeceased")) %>% 
    filter(date > (Sys.Date()- 45)) %>% 
    ggplot(aes(x= date, y = value , color = variable))+ 
    geom_line()+
    gghighlight(label_key = value)+
    guides(colour = guide_legend("legend" ,override.aes = aes(label = "|")))+
    theme_bw() +
    scale_x_date(date_breaks = "3 days" , date_labels = "%b %d")+
    scale_y_continuous(trans="log10") +
    labs(title = "Trajectory of Cumulative Counts",
         subtitle = paste0("<span style='color:#F8766E'>Confirmed</span>, ",
                           "<span style='color:#619CFF'>Recovered</span>, ",
                           "<span style='color:#00BA38'>Deceased</span>.",
                           " Dotted lines are pre-lockdown trend"),
         caption = "Data: covid19india.org API") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.subtitle = element_markdown(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")


ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_semilog_trend.png"),
       plot = last_plot ,
       height = 7, width = 7)


# trajectory plot ---------------------------------------------------------

 data_india_state %>%
    group_by(detectedstate) %>%
    summarise(date_at_100 = as.character(min(date[confirmed > 100])), na.rm = TRUE)  %>%
    left_join(data_india_state, by = "detectedstate") %>%
    mutate( days = difftime(date, date_at_100, units = "days") %>% ceiling() %>% as.integer()) %>%
    filter(days > 0 ) %>%
    ggplot(aes(x= days, y = confirmed)) + geom_line(aes(color = detectedstate))+
    scale_y_log10(limits = c(100,20000), breaks = c(100,200,400,800,seq(2000,20000,2000))) +
    scale_x_continuous(expand = c(0,5)) +
    gghighlight(label_key = detectedstate)+ xlab("Days since 100th confirmed case") +
    labs(title = "Statewise Cumulative Confirmed Counts on semi-log scale",
         caption = "Data: covid19india.org API") +
    theme_bw()+
    theme(axis.title.y = element_blank(),
          legend.position = "none")

ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_trajectory.png"),
       plot = last_plot ,
       height = 9, width = 9)


# trajectory of tests  ----------------------------------------------------

last_plot <- data_testing_state %>% group_by(State) %>% 
    filter(min(Positive, na.rm = TRUE) > 100,
           !is.na(Positive) & !is.na(Negative)) %>% 
    ggplot(aes(x = Negative, y = Positive, color = State, group = State))+ 
    # geom_point() +
    geom_line() +
    gghighlight(label_key = State,max_highlight = 30,label_params = list(size = 3)) +
    geom_point() +
    scale_x_log10() + scale_y_log10() +
    theme_bw() +
    theme(legend.position = "bottom")+
    ggtitle("Trajectory of Positive vs Negative tests")

ggsave(filename = paste0("~/git/covid19_twitter_bot/plots/",Sys.Date(),"_testing_trajectory.png"),
       plot = last_plot ,
       height = 9, width = 9)
