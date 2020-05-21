# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","reshape2","ggplot2","jsonlite","EpiEstim","patchwork"),
        function(x){
          if(!x %in% rownames(installed.packages())){
            install.packages(x)
          }
          suppressPackageStartupMessages(library(x, character.only = TRUE))

          x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------- -----
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

if(!all(c("data_india_raw","data_state") %in% ls())){
  source("load_data.R")
}

dir.create(paste0("statewise_plot/",Sys.Date()),recursive = TRUE)
date_start <- as.Date("2020-03-29")

#  -------------------------------------------------------------
#  Looping over states -------------------------------------------------------------
#  -------------------------------------------------------------

data_state %>% group_by(status,state) %>%
  mutate(tally = cumsum(count)) %>% filter(status == "Confirmed",
                                           date > date_start) %>% group_by(state) %>%
  summarise(n = max(tally)) %>% {
    .$state[.$n > 400]
  } %>% lapply(function(s){

    print(s)
    data <- data_state %>% group_by(status,state) %>%
      mutate(tally = cumsum(count)) %>%
      filter(status == "Confirmed",
             date > date_start,
             date < Sys.Date()) %>%
      filter(state == s)

    #  -------------------------------------------------------------
    #  Plotting -------------------------------------------------------------
    #  -------------------------------------------------------------

    K <- (log(data$tally[data$date == (Sys.Date() - 1)]) -
            log(data$tally[data$date == (Sys.Date() - 7)]) ) /
      (7 - 1)


    p1 <- data %>%  filter(status == "Confirmed") %>%
      ggplot(aes(x= date)) +
      geom_histogram(aes(y = count),stat = "identity")+
      scale_x_date(expand = c(0,0)) +
      labs(title = s,
           subtitle = paste0("7 day average doubling time: ", round(log(2)/K, 1), " days"))+
      ylab("daily new cases")+
      # geom_ribbon(data = predicted, aes( ymin = lower, ymax = upper), alpha = 0.3)+
      # geom_line(data = predicted,aes(x = date, y = mean), linetype = 2) +
      theme_bw()+
      theme(axis.title.x = element_blank())

    Rt <- data %>%
      dcast(data = . , formula = date ~ status, value.var = "count") %>%
      transmute(dates = date , I = Confirmed) %>%
      filter(I > 0) %>%
      estimate_R(incid = .,
                 method = "parametric_si",
                 config = make_config(list(mean_si = 5.12,std_si = 3)))
    p2 <- data.frame(date = Rt$dates[8:length(Rt$dates)],
                     R = Rt$R$`Mean(R)`,
                     R_max = Rt$R$`Quantile.0.975(R)`,
                     R_min = Rt$R$`Quantile.0.025(R)`) %>%
      ggplot(aes(x= date , y = R))+
      geom_line()+
      geom_hline(yintercept = 1, linetype = 3) +
      geom_ribbon(aes(ymax = R_max , ymin = R_min),alpha = 0.4) +
      scale_x_date(expand = c(0,0)) +
      ggtitle(label = "Effective Reproduction Number")+
      theme_bw() +
      theme(axis.title.x = element_blank())

    p3 <- data_testing_state %>% filter(State == s) %>%
      mutate(new_tests = `Total Tested` - lag(`Total Tested`)) %>%
      filter(`Updated On` > date_start) %>%
      ggplot(aes(x= `Updated On` , y = new_tests))+
      geom_histogram(stat="identity")+
      scale_x_date(expand = c(0,0)) +
      ggtitle(label = "New Tests done at each annoucement")+
      theme_bw()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())

    # case fatality ratios ----------------------------------------------------

    data_cfr <- data_state %>%
      filter(state == s,
             date < Sys.Date()) %>%
      group_by(status) %>%
      mutate(count = cumsum(count)) %>%
      filter(date > date_start)

    p4 <- data_cfr %>%
      group_by(date) %>%
      summarise(cfr = 100*count[status == "Deceased"]/(count[status == "Recovered"] + count[status == "Deceased"])) %>%
      na.omit() %>%
      ggplot(aes(x = date, y = cfr)) +
      geom_line() +
      geom_hline(yintercept = 0.75, linetype = 2)+
      scale_x_date(expand = c(0,0)) +
      labs(title = "Case Fatality Ratio %",
          subtitle = "Dotted line is best true estimate of CFR",
          caption = data_cfr %>%
            filter(date == max(date)) %>% {
              cfr <- .$count[.$status == "Deceased"]/(.$count[.$status == "Recovered"] + .$count[.$status == "Deceased"])
              cfr_true <- c(0.01,0.005)
              round(cfr/cfr_true,digits = 1) %>% paste0(.,collapse = " - ") %>%
                paste0("Estimated undercount of cases: (",.,")x")
            }) +
      theme_bw()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())

    p5 <- data_testing_state %>% filter(State == s) %>%
      filter(`Updated On` > date_start) %>%
      transmute(date = as.Date(`Updated On`),
                positive = Positive,
                negative = Negative) %>% na.omit() %>%
      mutate(new_positive = positive - lag(positive, default = 0),
             new_negative = negative - lag(negative, default = 0),
             percent_new_positive = 100 * new_positive/(new_positive + new_negative)) %>%
      ggplot(aes(x= date , y = percent_new_positive))+
      geom_histogram(stat = "identity")+
      ggtitle("Prcentage positive in new tests ") +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank())


    final_plot <- p1 /((p3/p5)|(p2/p4) + plot_layout(widths = c(1,1))) +
      plot_layout(heights = c(1,2))
    final_plot
    ggsave(paste0("statewise_plot/",Sys.Date(),"/",Sys.Date(),"_",gsub(" ","-",s),".png"),
           plot = final_plot,
           width = 8 , height = 8)

  }) %>%  invisible()


