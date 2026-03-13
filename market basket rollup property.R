library(dplyr)
# install.packages("arules")
library(arules)
library(googleAnalyticsR)
??googleAnalyticsR
ga_auth()

ga_account_list("ga4") %>% View() #yeh dekhna k konsy GA4 properties available hyn [To view available GA4 props]

ga_meta("data", propertyId = 320610777) %>% View() #yeh dekh raha hun k konsi fields GA4 me available hyn humary liye [To check what fields available to us in GA4 ]

property_ids <- c('320610777') # yahan p comma se separate karky aur properties dali jayen yeh usi k liye hy warna aik k liye isse asaan tareeka hota hy baghair loop banaye


fenix_rollup <- data.frame()


for (property_id in property_ids) {
  
  data <- ga_data(
    property_id,
    metrics = c("itemRevenue"),
    dimensions = c("date","transactionId","currencyCode", "itemName","itemCategory3","itemCategory4","itemCategory","itemVariant"),
    date_range = c("2023-07-01", "2024-09-29"),
    # c("2023-10-01", "2024-01-30"),
    
    dim_filters = ga_data_filter("eventName"=="purchase"), #specific condition define karna jese sql me where hota hy
    limit = -1 #-1 ka matlab hy saary rows
  ) %>% unique()
  
  data <- data %>% mutate(property_id = property_id) %>% unique()
  
  # mojuda data se join karna jese sql me union hota hy
  fenix_rollup <- rbind(fenix_rollup, data) %>% unique()
}


View(fenix_rollup)
write.csv(fenix_rollup, "fenix_rollup.csv")
write.csv(BQ, "fenix_BQ_NA.csv")
fenix_rollup = read.csv("fenix_rollup.csv")
summary(fenix_rollup) #summary stats
fenix_rollup[sapply(fenix_rollup, is.character)] <- lapply(fenix_rollup[sapply(fenix_rollup, is.character)], as.factor) #tamam character ko factor yani categorical me convert
BQ[sapply(BQ, is.character)] <- lapply(BQ[sapply(BQ, is.character)], as.factor) #tamam character ko factor yani categorical me convert
library(arules)

n_distinct(fenix_rollup$transactionId) #count distinct transactionID yani unique transactions ka count
n_distinct(fenix_rollup$itemName) # unique items

fenix_rollup %>% group_by(transactionId) %>% summarise(items = n_distinct(itemName)) %>% filter(fenix_rollup$itemRevenue>1)

item_count = fenix_rollup %>% group_by(fenix_rollup$itemName) %>% summarise(n_distinct(transactionId)) %>% View()
result <- item_counts %>% group_by(items) %>% summarise(count_transactionID = n_distinct(transactionId))
View(result)
fnt1plus = fenix_rollup %>% filter(itemRevenue > 1) %>% group_by(transactionId) %>% summarise(items = n_distinct(itemName)) %>% filter(items > 1)  
fnt1plus %>% View()

filtered_fenix_rollup = fenix_rollup %>% filter(fenix_rollup$transactionId %in% fnt1plus$transactionId & fenix_rollup$itemRevenue >1)
n_distinct(filtered_fenix_rollup$itemVariant)
summary(filtered_fenix_rollup)


# Create a transaction object
transactions <- split(filtered_fenix_rollup$itemVariant, filtered_fenix_rollup$transactionId)
trans_list <- as(transactions, "transactions")

#loop k liye zaroori hy
support_values <- seq(0.0001, 0.001, by = 0.0001)
# support_values <- seq(0.0001, 0.1, by = 0.01) #0.0001)  #not recommended tho takes alot of time
confidence_values <- seq(0.5, 0.9, by = 0.1)       

# list to store results
results_list <- list()
counter <- 1  # counter for result storage

#Loop chalao
for (support in support_values) {
  for (confidence in confidence_values) {
    # Apriori with current parameters
    fit <- apriori(trans_list, parameter = list(support = support, confidence = confidence))
    
    # #of rules generated
    num_rules <- length(fit)
    
    # \rules with lift > 1.5 
    filtered_rules <- subset(fit, lift > 1.5)
    num_filtered_rules <- length(filtered_rules)
    
    # Store the results
    results_list[[counter]] <- list(
      support = support,
      confidence = confidence,
      num_rules = num_rules,
      num_filtered_rules = num_filtered_rules
    )
    
    # Print current status
    cat("Support:", support, "Confidence:", confidence,
        "Total Rules:", num_rules, "Filtered Rules:", num_filtered_rules, "\n")
    
    # Increment counter
    counter <- counter + 1
  }
}

# Convert results to a data frame for easy analysis
results_df <- do.call(rbind, lapply(results_list, as.data.frame))


View(results_df)

# Find the best parameters based on the number of filtered rules
best_result <- results_df[which.max(results_df$num_filtered_rules),]
cat("Best Parameters - Support:", best_result$support, 
    "Confidence:", best_result$confidence, 
    "Number of Filtered Rules:", best_result$num_filtered_rules, "\n")

# Run Apriori with the best parameters
optimal_fit <- apriori(trans_list, parameter = list(support = best_result$support, confidence = best_result$confidence))

# Sort and display the top rules by count
inspect(sort(optimal_fit, by = "count")[1:20])

# Support: Identifies popular product combinations, aiding in inventory and display planning.
# Confidence: Measures the likelihood of customers buying a set of products together, useful for crafting targeted promotions.
# Coverage: Shows the percentage of all transactions that include a specific product, highlighting its influence on sales.
# Lift: Evaluates the strength of an association between products, guiding effective cross-selling or bundling strategies.
# Count: Provides the total number of transactions for specific product sets, indicating their market presence and potential profitability.

#===============30-09-24 above================================#



transactions <- split(fenix_rollup$itemName, fenix_rollup$transactionId)

# Convert to transactions object
trans_list <- as(transactions, "transactions")

# Run the Apriori algorithm
fit <- apriori(trans_list, parameter = list(support = 0.0001, confidence = 0.1))
fit <- apriori(trans_list, parameter = list(lift))

# Sort and display the top rules
inspect(sort(fit, by = "count"))

fitems_NA = read.csv('fenix_item.csv')
fitems_NA[sapply(fitems_NA, is.character)] <- lapply(fitems_NA[sapply(fitems_NA, is.character)], as.factor)
fenix_rollup %>% filter(fenix_rollup$transactionId == 'FW7146670616') %>% View()
fenix_rollup %>% View()

# Support: Identifies popular product combinations, aiding in inventory and display planning.
# Confidence: Measures the likelihood of customers buying a set of products together, useful for crafting targeted promotions.
# Coverage: Shows the percentage of all transactions that include a specific product, highlighting its influence on sales.
# Lift: Evaluates the strength of an association between products, guiding effective cross-selling or bundling strategies.
# Count: Provides the total number of transactions for specific product sets, indicating their market presence and potential profitability.

fitems_NA %>% filter(fitems_NA$price_in_usd > 0) %>% summary()
fitems_NA %>% filter(fitems_NA$price_in_usd > 0) %>% summary()
hist(subset(fitems_NA, item_category == 'men')$price_in_usd)
hist(subset(fitems_NA, item_category >= 1)$price_in_usd)
hist(fitems_NA$price_in_usd)

library("bigrquery")

bq_auth()

bq_auth()
sql_saad <- "select
* 
from `canvas-provider-295813.analysis_CRO.ga4_realtime_view_asad_saad`
where 0=0 
and event_name = 'purchase'  and event_name2= 'purchase'
and item_revenue_in_usd > 0
and item_name != 'All'
and market = 'NA'
group by all"
projectid = "canvas-provider-295813"
# Run the query
project_query <- bq_project_query(projectid, sql_saad, use_legacy_sql = FALSE)
# Download result to dataframe
BQ <- bq_table_download(project_query)
str(BQ)
summary(BQ)
View(BQ)
BQ_CRFM = BQ
(BQ$category) %>% unique(.) 
BQ_Champion = BQ %>% filter(BQ$category == 'Champion') 
BQ_Champion = BQ %>% filter(BQ$category %in% c('Loyal Customer','Cant Lose','Champion'))
BQ_Champion %>% View()

# Create a transaction object
transactions <- split(BQ$item_category3_translated, BQ$transaction_id)
trans_list <- as(transactions, "transactions")

#loop k liye zaroori hy
support_values <- seq(0.0001, 0.001, by = 0.0001)
# support_values <- seq(0.0001, 0.1, by = 0.01) #0.0001)  #not recommended tho takes alot of time
confidence_values <- seq(0.3, 0.9, by = 0.1)       

# list to store results
results_list <- list()
counter <- 1  # counter for result storage

#Loop chalao
for (support in support_values) {
  for (confidence in confidence_values) {
    # Apriori with current parameters
    fit <- apriori(trans_list, parameter = list(support = support, confidence = confidence))
    
    # #of rules generated
    num_rules <- length(fit)
    
    # \rules with lift > 1.5 
    filtered_rules <- subset(fit, lift > 1.5)
    num_filtered_rules <- length(filtered_rules)
    
    # Store the results
    results_list[[counter]] <- list(
      support = support,
      confidence = confidence,
      num_rules = num_rules,
      num_filtered_rules = num_filtered_rules
    )
    
    # Print current status
    cat("Support:", support, "Confidence:", confidence,
        "Total Rules:", num_rules, "Filtered Rules:", num_filtered_rules, "\n")
    
    # Increment counter
    counter <- counter + 1
  }
}

# Convert results to a data frame for easy analysis
results_df <- do.call(rbind, lapply(results_list, as.data.frame))


View(results_df)

# Find the best parameters based on the number of filtered rules
best_result <- results_df[which.max(results_df$num_filtered_rules),]
cat("Best Parameters - Support:", best_result$support, 
    "Confidence:", best_result$confidence, 
    "Number of Filtered Rules:", best_result$num_filtered_rules, "\n")
View(cat)
# Run Apriori with the best parameters
optimal_fit <- apriori(trans_list, parameter = list(support = best_result$support, confidence = best_result$confidence))

# Sort and display the top rules by count
inspect(sort(optimal_fit, by = "count"))
# Identify redundant rules
redundant_rules <- is.redundant(optimal_fit)

# Print the rules that are redundant (optional step to inspect)
inspect(optimal_fit[redundant_rules])

# Custom filtering example to remove similar rules based on confidence
optimal_fit_cleaned <- optimal_fit[!duplicated(as(lhs(optimal_fit), "character"))]


# Remove redundant rules
optimal_fit_cleaned <- optimal_fit[!redundant_rules]

lhs_strings <- labels(lhs(optimal_fit))  # Extract lhs
rhs_strings <- labels(rhs(optimal_fit))  # Extract rhs

# Combine lhs and rhs for comparison
combined_rules <- paste(lhs_strings, "=>", rhs_strings)

# Remove duplicates based on combined lhs + rhs strings
optimal_fit_cleaned <- optimal_fit[!duplicated(combined_rules)]

# Inspect the cleaned rules
inspect(optimal_fit_cleaned)

# Inspect the non-redundant rules
inspect(sort(optimal_fit_cleaned, by = "confidence"))

rules_df = as(inspect(sort(optimal_fit_cleaned, by = "confidence")), "data.frame")
rules_df$rank <- rank(-rules_df$confidence, ties.method = "first")
rules_df %>% View()
rules_df_ = rules_df[,-2]
n_distinct(BQ$transaction_id)
# Support: Identifies popular product combinations, aiding in inventory and display planning.
# Confidence: Measures the likelihood of customers buying a set of products together, useful for crafting targeted promotions.
# Coverage: Shows the percentage of all transactions that include a specific product, highlighting its influence on sales.
# Lift: Evaluates the strength of an association between products, guiding effective cross-selling or bundling strategies.
# Count: Provides the total number of transactions for specific product sets, indicating their market presence and potential profitability.



bq_auth()
table <- bq_table(project = "canvas-provider-295813", dataset = "analysis_CRO", table = "association_rules_NA")
#bq_table_create(table)
bq_table_create(table, fields = rules_df_)
bq_table_upload(
  x = table,           # Table reference
  values = rules_df_,       # Data you want to upload
  write_disposition = "WRITE_TRUNCATE"  # Options: "WRITE_TRUNCATE", "WRITE_APPEND", "WRITE_EMPTY"
)



bq_table_upload(
  dataset = "abre_ga4_to_bq",
  table = "abre_saad",
  values = data3,
  project = "abre-email",
  write_disposition = "WRITE_TRUNCATE"  # Options: "WRITE_TRUNCATE", "WRITE_APPEND", "WRITE_EMPTY"
)
