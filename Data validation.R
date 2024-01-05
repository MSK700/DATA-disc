# Data Validation between GA4 and BQ

library(ggplot2)
library(ggplot)
library(corrplot)
library(data.table)
library(readr)
library(dplyr) # Important
library(tidyquant)
library(mltools)
library(lubridate)
library(googleAnalyticsR) # Important
library(bigrquery) #Important



# Lets first get the basic metrics aggregated on date level from BQ#
bq_auth()
sql_saad <- "select
PARSE_DATE('%Y%m%d', event_date)date,
count(distinct user_pseudo_id ) userpsudos,
count(distinct user_id) userids, 
count(distinct ecommerce.transaction_id) transactions, 
count(distinct (select value.int_value from unnest(event_params) where key = 'ga_session_id')) sessions,
sum(ecommerce.purchase_revenue_in_usd) revenue
FROM `project.dataset.events_*`
where 0=0
and _table_suffix between '20231106' and '20231110'
group by 1"
projectid = "PROJECT NAME"
# Run the query
project_query <- bq_project_query(projectid, sql_saad, use_legacy_sql = FALSE)
# Download result to dataframe
BQ <- bq_table_download(project_query)
str(BQ)
summary(BQ)
View(BQ)

# Now lets get the same data from GA4 to compare#
ga_auth()
GA4 <- ga_data(
  '405364247',
  # '361662151',
  metrics = c("purchaseRevenue","totalUsers","sessions", "transactions"),
  dimensions = c("date"),
  date_range = c("2023-11-30", "2023-12-05")
  # , max = 1000000
)%>% unique()
# as.factor(GA4$eventName) %>% summary()
GA4%>% View()




#Now lets compare the 2
BQ$date <- as.Date(BQ$date)
GA4$date <- as.Date(GA4$date)
joined_data <- full_join(BQ, GA4, by = "date")
joined_data = joined_data %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
comparison <- joined_data %>%
  mutate(
    transactions_diff = transactions.x - transactions.y,
    revenue_diff = revenue - purchaseRevenue,
    users_diff = userpsudos - totalUsers,
    sessions_diff = sessions.x - sessions.y
  )

comparision_View()