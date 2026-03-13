library(dplyr)

library(googleAnalyticsR)
??googleAnalyticsR
ga_auth()

ga_account_list("ga4") %>% View()

ga_meta("data", propertyId = 320610777) %>% View()

property_ids <- c('320610777', '322138925', '322129826')


combined_data <- data.frame()


for (property_id in property_ids) {
  
  data <- ga_data(
    property_id,
    metrics = c("sessions", "totalRevenue", "purchaseRevenue", "eventValue"),
    dimensions = c("date","transactionId","currencyCode"),
    date_range = c("2023-10-01", "2024-02-13"),
      # c("2023-10-01", "2024-01-30"),
    
    dim_filters = ga_data_filter("eventName"=="purchase"),
    limit = -1
    ) %>% unique()
  
  data <- data %>% mutate(property_id = property_id) %>% unique()
  
  # Combine the data with the existing data
  combined_data <- rbind(combined_data, data) %>% unique()
}

# Print the combined data frame
print(combined_data)
write.csv(combined_data, "fenix_GA4_transactions.csv")

combined_data %>% filter(combined_data$transactionId == 'FW7146670616') %>% View()
combined_data %>% View()

library("bigrquery")

bq_auth(use_oob = TRUE, cache = FALSE)
