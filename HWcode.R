#-----------------------HW-------------------------------------
air_id = "air_8e4360a64dbd4c50"
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

n<-39
s <- as.Date("2017-04-23")
dates<-seq(from=s, by=1, length.out=n) 
t<-data.frame(dates)
t$dates = as.character(t$dates)

plot_hw_air_id <- function(air_id){
  # max_date <- max(air_visits$visit_date)
  # split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    replace_na(list(air_store_id = air_id)) %>%
    rownames_to_column()
  
  # visits_train <- visits %>% filter(visit_date <= split_date)
  # visits_valid <- visits %>% filter(visit_date > split_date)
  
  hw.fit <- HoltWinters(tsclean(ts(visits$visitors, frequency = 14)))
  
  hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>%
    as.tibble() 
  # %>%bind_cols(visits_valid)
  return(hw_visits)
}

mydatahw <- data.frame(fit = numeric(0),date = character(0), id = character(0), pred = numeric(0) )

for (i in storelist$store){
  fcast = plot_hw_air_id(i) 
  fcast39=fcast %>%select(fit)%>%mutate(date = t$dates, id = i, pred = exp(fit)-1)
  mydatahw<-rbind(mydatahw,fcast39)
}

save(mydatahw, file = "HWresult.RData")
