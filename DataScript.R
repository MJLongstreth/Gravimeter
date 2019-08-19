library(readr)
library(dplyr)

users <- read_csv("users.csv")
edds <- read_csv("edds.csv")
processing <- read_csv("processing.csv")
promotion <- read_csv("promotion.csv")

processing$date_submitted <- mdy(processing$date_submitted)
processing$date_complete <- mdy(processing$date_complete)
processing <- processing %>%
  mutate(year_month = substr(date_complete,0,7)) %>%
  mutate(cost_per_GB = processing_cost) %>%
  mutate(processing_cost = round((data_volume_post_exp_gb * processing_cost), 2)) %>%
  arrange(desc(year_month))

promotion$date_submitted <- mdy(promotion$date_submitted)
promotion$date_complete <- mdy(promotion$date_complete)
promotion <- promotion %>%
  mutate(year_month = substr(date_complete,0,7)) %>%
  mutate(cost_per_GB = promotion_cost) %>%
  mutate(promotion_cost = round((total_of_natives_gb * promotion_cost), 2)) %>%
  arrange(desc(year_month))

inv_processing <- processing %>%
  select(year_month, processing_cost) %>%
  group_by(year_month) %>%
  summarize(sum(processing_cost))

inv_promotion <- promotion %>% 
  select(year_month, promotion_cost) %>%
  group_by(year_month) %>%
  summarize(sum(promotion_cost))

summary(users)
str(users)

# users$Date <- mdy(users$Date)

users <- users %>%
  mutate(Year_Month = substr(mdy(users$Date),0,7)) %>%
  select(attorney_name, Year_Month) %>%
  group_by(attorney_name, Year_Month) %>%
  summarize(access_count=n()) %>%
  mutate(monthly_user_fee=75)
  
glimpse(edds)

edds <- edds %>%
  mutate(total_volume = apply(edds[-1], 1, sum)) %>%
  mutate(month_hosting_cost = total_volume * 15) %>%
  arrange(desc(year_month))


processing <- read_csv("processing.csv")
processing$date_submitted <- mdy(processing$date_submitted)
processing$date_complete <- mdy(processing$date_complete)
processing <- processing %>%
  mutate(year_month = substr(date_complete,0,7)) %>%
  mutate(ticket_cost = round((data_volume_post_exp_gb * 35), 2)) %>%
  arrange(desc(year_month))

sum(promotion$promotion_cost)

users <- read_csv("users.csv")
users <- users %>%
  mutate(year_month = substr(mdy(users$access_date),0,7)) %>%
  select(attorney_name, year_month) %>%
  group_by(attorney_name, year_month) %>%
  summarize(access_count = n()) %>%
  mutate(monthly_user_fee = user_fee) %>%
  arrange(desc(year_month), attorney_name)

users_overview <- users %>%
  select(year_month, monthly_user_fee) %>%
  group_by(year_month) %>%
  summarize(sum(monthly_user_fee))

inv_allocation <- gather(invoices, key='cost_type', value='amount', -year_month, -total) %>%
  select(cost_type, amount) %>%
  drop_na() %>%
  group_by(cost_type) %>%
  summarize(sum(amount))
inv_allocation <- inv_allocation %>%
  group_by()