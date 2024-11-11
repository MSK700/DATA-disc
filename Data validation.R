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
install.packages("DataExplorer")
library(DataExplorer)
create_report(BQ_EDA)


# Lets first get the basic metrics aggregated on date level from BQ#
bq_auth()
sql_saad <- "select
PARSE_DATE('%Y%m%d', event_date)date,
count(distinct user_pseudo_id ) userpsudos,
count(distinct user_id) userids, 
count(distinct ecommerce.transaction_id) transactions, 
count(distinct (select value.int_value from unnest(event_params) where key = 'ga_session_id')) sessions,
count(distinct concat(user_pseudo_id,(select value.int_value from unnest(event_params) where key = 'ga_session_id'))) sessions2,
sum(ecommerce.purchase_revenue_in_usd) revenue,sum(ecommerce.purchase_revenue_in_usd) revenue2
FROM `canvas-provider-295813.analytics_322138925.events_*` 
where 0=0
group by 1"
projectid = "canvas-provider-295813"
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
  '322138925',
  # '361662151',
  metrics = c("purchaseRevenue","totalUsers","sessions", "transactions"),
  dimensions = c("date"),
  date_range = c("2023-07-19", "2024-01-22"),
  # date_range = NULL , 
  # , max = 1000000
  limit = -1
)%>% unique()
# as.factor(GA4$eventName) %>% summary()
GA4%>% View()


ga_meta("data", propertyId = 314629498) %>% View()

#Now lets compare the 2
BQ$date <- as.Date(BQ$date)
GA4$date <- as.Date(GA4$date)
joined_data <- full_join(BQ, GA4, by = "date")
joined_data = joined_data %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
comparison_322138925 <- joined_data %>%
  mutate(
    transactions_diff = transactions.x - transactions.y,
    revenue_diff = revenue2 - purchaseRevenue,
    users_diff = userpsudos - totalUsers,
    sessions_diff = sessions2 - sessions.y
  )

comparison_322138925 %>% View()





#EDA on GA4 data in BQ

sql_EDA <- "
select
user_pseudo_id,traffic_source.*,geo.country,device.language,device.operating_system_version,user_ltv.revenue,
 avg(((select value.int_value from unnest(event_params) where key = 'engagement_time_msec')/1000)) avg_duration_sec,
 min(((select value.int_value from unnest(event_params) where key = 'engagement_time_msec')/1000)) min_sec,
 max(((select value.int_value from unnest(event_params) where key = 'engagement_time_msec')/60000))max_min,
count(distinct concat(user_pseudo_id,(select value.int_value from unnest(event_params) where key = 'ga_session_id'))) session2,
count(*) activity,safe_divide((COUNTIF(event_name = 'session_start') - COUNTIF(event_name = 'user_engagement')),COUNTIF(event_name = 'session_start')) 
AS bounce_rate, 
count(distinct user_id) ids,
count(distinct event_date) frequency,
count(distinct ecommerce.transaction_id) transactions,
sum(ecommerce.purchase_revenue) revenue,
sum(ecommerce.purchase_revenue_in_usd) revenue_in_usd
FROM `canvas-provider-295813.analytics_322138925.events_*` 
where 0=0
group by 1,2,3,4,5,6,7,8
"
projectid = "canvas-provider-295813"
# Run the query
project_query_EDA <- bq_project_query(projectid, sql_EDA, use_legacy_sql = FALSE)
# Download result to dataframe
BQ_EDA <- bq_table_download(project_query_EDA)
str(BQ_EDA)
summary(BQ_EDA)
BQ_EDA_2 = BQ_EDA
BQ_EDA_2 =BQ_EDA_2 %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
BQ_EDA_2 =BQ_EDA_2 %>% mutate_if(is.character, ~replace(., is.na(.), 0))
create_report(BQ_EDA_2)
