setwd("/Users/fanfangege/Desktop/Kaggle_restaurant/datasets")
air_reserve <- read.csv("air_reserve.csv")
air_store <- read.csv("air_store_info.csv")
air_visits <- read.csv("air_visit_data.csv")
holidays <- read.csv("date_info.csv")
hpg_reserve <- read.csv("hpg_reserve.csv")
hpg_store <- read.csv("hpg_store_info.csv")
test<- read.csv("sample_submission.csv")
store_ids <- read.csv("store_id_relation.csv")

#We use the first air_store_id ("air_ba937bf13d40fb24") as an example.
air_id = "air_ba937bf13d40fb24"

#Apr 23th - May 31st
#automatically extract these 39 days from the length of the test prediction range 
#and define it as our "prediction length".
library(dplyr)
library(tidyr)
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

#predict for the last 39 days of our training sample
#we compute the upper end of our training dates and subtract our "prediction length" 
#from this value to define the start of our validation sample on Mar 14th.
#We also create a data set of all visit_dates in preparation 
#for many time series having gaps.
max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))


#extract the time series for the specific air_store_id
#transform the visitors counts by log1p and join the data set of all visit_dates
#This gives us a number of NA which we fill in with the overall median
library(tibble)
foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
  rownames_to_column()

#Using this new time series, we now split the data into training and validation sets.
visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)

#Now comes the fitting part. 
library(forecast)
hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))

#Using the fitted ARIMA model we will forecast for our "prediction length". We include confidence intervals.
hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>%
  as.tibble() %>%
  bind_cols(visits_valid)


#Finally, we plot our prediction
library(ggplot2)
visits_train %>%
  ggplot(aes(visit_date, visitors)) +
  geom_line() +
  geom_ribbon(data = hw_visits, aes(x = visit_date, ymin = lwr, ymax = upr), fill = "light blue") +
  geom_line(data = hw_visits, aes(visit_date, visitors), color = "grey60") +
  geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
  geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
  labs(x = "Time [weeks]", y = "log1p visitors vs predictions") +
  ggtitle("HoltWinters")
}

plot_hw_air_id("air_ba937bf13d40fb24")
p1 <- plot_hw_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_hw_air_id("air_8e4360a64dbd4c50")
p3 <- plot_hw_air_id("air_1c0b150f9e696a5f")
p4 <- plot_hw_air_id("air_820d1919cbecaa0a")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
#Now we turn this procedure into a function, including the plotting part.
plot_hw_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))
  
  hw_visits_predict <- hw.fit %>% predict(n.ahead = pred_len, prediction.interval = T, level = 0.95)
  return(hw_visits_predict)
}
