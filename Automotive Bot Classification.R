# 1469615819.1708667
library(dplyr)
# install.packages("arules")
library(arules)
library(googleAnalyticsR)
??googleAnalyticsR
ga_auth()

ga_account_list("ga4") %>% View() #yeh dekhna k konsy GA4 properties available hyn [To view available GA4 props]
# 213025502
ga_meta("data", propertyId = 356831735) %>% View() #yeh dekh raha hun k konsi fields GA4 me available hyn humary liye [To check what fields available to us in GA4 ]

property_id <- c('356831735') # yahan p comma se separate karky aur properties dali jayen yeh usi k liye hy warna aik k liye isse asaan tareeka hota hy baghair loop banaye

library(bigrquery)
bq_auth()
#for bots
sql_saad <- "select
`customUser:ga4_client_id`
from `automotive-superstore.analysis.Bot_traffic`
where 0=0 
group by all"
projectid = "automotive-superstore"
# Run the query
project_query <- bq_project_query(projectid, sql_saad, use_legacy_sql = FALSE)
# Download result to dataframe
BQ_automotive <- bq_table_download(project_query)
str(BQ_automotive)

#For customers
sql_saad <- "SELECT `customUser:ga4_client_id` FROM `automotive-superstore.GA4_hist.data_transaction_level_dated` 
where 0=0
and `customUser:ga4_client_id` != '(not set)'
group by all"
projectid = "automotive-superstore"
# Run the query
project_query <- bq_project_query(projectid, sql_saad, use_legacy_sql = FALSE)
# Download result to dataframe
BQ_automotive <- bq_table_download(project_query)
str(BQ_automotive)
cst  =c(BQ_automotive$`customUser:ga4_client_id`)
nft  =c(BQ_automotive$`customUser:ga4_client_id`)

f1 = ga_data_filter("customUser:ga4_client_id" == nft)
f2 = ga_data_filter("customUser:ga4_client_id" == cst)

data_bots <- ga_data(
  property_id,
  metrics = c("totalUsers","sessions","engagedSessions","bounceRate","averageSessionDuration","eventCount"),
  dimensions = c("sessionSourceMedium","region", "customUser:ga4_client_id","hostName","mobileDeviceModel","screenResolution","operatingSystemWithVersion", "browser"),
  date_range = c("2024-01-01", "2024-11-01"),
  # c("2023-10-01", "2024-01-30"),
  # dim_filters = ga_data_filter("eventName" == "page_view"),
  dim_filters = ga_data_filter(f1 & "hostName" %contains% "automotivesuperstore" & "eventName" == "page_view" ),
  limit = -1 #-1 ka matlab hy saary rows
) %>% unique()

data_customers <- ga_data(
  property_id,
  metrics = c("totalUsers","sessions","engagedSessions","bounceRate","averageSessionDuration","eventCount"),
  dimensions = c("sessionSourceMedium","region", "customUser:ga4_client_id","hostName","mobileDeviceModel","screenResolution","operatingSystemWithVersion", "browser"),
  date_range = c("2024-01-01", "2024-11-01"),
  dim_filters = ga_data_filter(f2 & "hostName" %contains% "automotivesuperstore" & "eventName" == "page_view" ),
  limit = -1 #-1 ka matlab hy saary rows
) %>% unique()

library(sqldf)
all_users = sqldf("
      select *,'bots' user_type from data_bots
      union all
      select *, 'customers' user_type from data_customers
      ")
all_users$screenResolution_og = all_users$screenResolution
all_users <- all_users %>%  separate(screenResolution, into = c("width", "height"), sep = "x")
all_users$sessionSourceMedium_og = all_users$
all_users <- all_users %>%  separate(sessionSourceMedium, into = c("SessionSource", "SessionMedium"), sep = "/")
all_users %>% View()
str(all_users)
all_users$width = as.numeric(all_users$width)
all_users$height = as.numeric(all_users$height)
all_users[sapply(all_users, is.character)] <- lapply(all_users[sapply(all_users, is.character)], as.factor)

all_users = read.csv("auto_bot_data.csv")

all_users <- all_users %>%
  mutate(
    # Extract the operating system name
    OS_Type = sub(" .*", "", operatingSystemWithVersion),
    
    # Extract the numeric part (version) and convert to numeric, setting NA or missing values to 0
    OS_Version = as.numeric(sub("^[^0-9]*", "", operatingSystemWithVersion)),
    OS_Version = ifelse(is.na(OS_Version), 0, OS_Version)  # Replace NA with 0
  )

str(all_users)
summary(all_users)
sample_size <- 50000

# Sample 50k records for each class
bots_sample <- all_users %>% filter(user_type == "bots") %>% sample_n(sample_size)
customers_sample <- all_users %>% filter(user_type == "customers") %>% sample_n(sample_size)

# Combine the two samples to create a balanced training dataset
balanced_data <- bind_rows(bots_sample, customers_sample)

# Shuffle the data before splitting
balanced_data <- balanced_data %>% sample_frac(1)

# Train-test split (80% train, 20% test)
train_size <- floor(0.8 * nrow(balanced_data))


train_data <- balanced_data[1:train_size, ]

test_data <- balanced_data[(train_size + 1):nrow(balanced_data), ]

library(rpart)
# Fit the rpart model on the training data, excluding specific columns
bot_tree_model <- rpart(user_type ~ . - customUser.ga4_client_id - hostName,
                        data = train_data,
                        method = "class",
                        control = rpart.control(cp = 0.01))  # You can adjust cp to control the tree complexity
# Fit the rpart model on the training data, INcluding specific columns
bot_tree_model_new <- rpart(user_type ~ width + height + OS_Type + OS_Version + browser ,
                        data = train_data,
                        method = "class",
                        control = rpart.control(cp = 0.0001))  # You can adjust cp to control the tree complexity
summary(bot_tree_model_new)

# Fit the logistic regression model on the training data, excluding specific columns
bot_model <- glm(user_type ~ width + height + OS_Type + OS_Version + browser,
                 data = train_data,
                 family = binomial)

# View summary of the model
summary(bot_model)

test_predictions <- predict(bot_model, newdata = test_data, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(test_predictions > 0.5, "bot", "human")), 
                as.factor(test_data$user_type))

# Optionally, use the model to predict on the test set
predicted_prob <- predict(bot_tree_model_new, newdata = test_data, type = "class")
library(caret)

# Assuming your test data has the true labels
confusion_matrix <- confusionMatrix(table(test_data$user_type, predicted_prob))
print(confusion_matrix)

#NEW THINGS

# Define the custom loss function for XGBoost
custom_loss <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")  # Get the true labels
  
  # Define the penalty: CLV penalty is 100 for customers misclassified as bots, 1 otherwise
  clv_penalty <- ifelse(labels == 0 & preds > 0.5, 100, 1)
  
  # Compute gradient and hessian
  grad <- (preds - labels) * clv_penalty
  hess <- clv_penalty  # Constant hessian
  
  return(list(grad = grad, hess = hess))
}

# Install necessary packages if not already installed
install.packages("xgboost")
install.packages("shapviz")

# Load libraries
library(xgboost)
library(shapviz)

library(rpart)

# Define a function to train and evaluate the model with custom loss
optimize_tree <- function(train_data, test_data, custom_loss_function) {
  best_cp <- NULL
  best_model <- NULL
  best_loss <- Inf  # Initialize to a very large value
  
  # Loop through different cp values
  for (cp_val in seq(0.01, 0.1, by = 0.01)) {
    model <- rpart(user_type ~ . - `customUser:ga4_client_id` - hostName,
                   data = train_data,
                   method = "class",
                   control = rpart.control(cp = cp_val))
    
    # Predict on the test set
    predictions <- predict(model, test_data, type = "prob")[, 2]  # Probabilities for "bot" class
    
    # Calculate custom loss
    loss <- custom_loss_function(predictions, test_data$user_type)
    
    # Check if this model has the lowest loss
    if (loss < best_loss) {
      best_loss <- loss
      best_cp <- cp_val
      best_model <- model
    }
  }
  
  return(list(best_cp = best_cp, best_model = best_model, best_loss = best_loss))
}

# Run optimization
result <- optimize_tree(train_data, test_data, custom_loss)
print(result$best_cp)
print(result$best_loss)

library(rpart.plot)

# Plot with improved resolution and customization
png("decision_tree_plot.png", width = 1200, height = 800, res = 150)  # Set a higher resolution
rpart.plot(result$bot_tree_model, type = 2, extra = 101, under = TRUE, tweak = 1.2,
           box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)
dev.off()


install.packages("iml")
library(iml)

features <- train_data[, !colnames(train_data) %in% c("user_type", "customUser:ga4_client_id", "hostName")]
train_data_clean <- train_data[, !colnames(train_data) %in% c("customUser:ga4_client_id", "hostName")]

# Step 2: Fit the Decision Tree Model Excluding Specific Columns
bot_tree_model <- rpart(
  user_type ~ .,
  data = train_data_clean,
  method = "class",
  control = rpart.control(cp = 0.0001)
)

# Step 3: Define the Prediction Function for SHAP
predict_function <- function(model, newdata) {
  predict(model, newdata, type = "prob")[, "bot"]  # Probability for 'bot' class
}

# Step 4: Create a Predictor Object for SHAP Analysis
predictor <- Predictor$new(
  model = bot_tree_model,
  data = features,  # Use 'features' without the excluded columns
  y = train_data$user_type,
  predict.function = predict_function
)

# Step 5: Calculate SHAP Values for a Sample Observation
shapley <- Shapley$new(predictor, x.interest = features[1, ])

# Step 6: Display SHAP Values in a Table
shap_values <- shapley$results
print(shap_values)

# Optional: Plot the SHAP values to visualize feature importance
shapley$plot()
summary(bot_tree_model)

# Plot SHAP values
shap_values$plot()
# 213025502
install.packages("sparklyr")
library(sparklyr)
#============================#
install.packages("foreach")
install.packages("doParallel")
library(foreach)
library(doParallel)
library(googleAnalyticsR)
# remove.packages()

library(bigrquery)
bq_auth()
sql_saad <- "select
`customUser:ga4_client_id`
from `automotive-superstore.analysis.Bot_traffic`
where 0=0 
group by all"
projectid = "automotive-superstore"
# Run the query
project_query <- bq_project_query(projectid, sql_saad, use_legacy_sql = FALSE)
# Download result to dataframe
BQ_automotive <- bq_table_download(project_query)
str(BQ_automotive)
summary(BQ_automotive)
View(BQ)
nft  =c(BQ_automotive$`customUser:ga4_client_id`)

# Set up parallel backend
num_cores <- detectCores() - 1  # Use one less than the total number of cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)


# Define date range and split it into smaller chunks
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-11-01")
date_ranges <- seq(start_date, end_date, by = "30 days")  # Adjust for desired chunk size

# Create pairs of start and end dates for each chunk
date_chunks <- data.frame(
  start_date = date_ranges[-length(date_ranges)],
  end_date = date_ranges[-1] - 1
)

# Fetch data in parallel using foreach
data_psudo_hostname <- foreach(i = 1:nrow(date_chunks), .combine = rbind, .packages = "googleAnalyticsR") %dopar% {
  ga_data(
    property_id,
    metrics = c("totalUsers", "sessions", "engagedSessions", "bounceRate", "averageSessionDuration", "eventCount"),
    dimensions = c("sessionSourceMedium", "region", "customUser:ga4_client_id", "hostName", "mobileDeviceModel", "screenResolution", "operatingSystemWithVersion", "browser"),
    date_range = c(as.character(date_chunks$start_date[i]), as.character(date_chunks$end_date[i])),
    dim_filters = ga_data_filter(f1 & "hostName" %contains% "automotivesuperstore" & "eventName" == "page_view" ),
    limit = -1  # Retrieve all rows for this chunk
  )
} %>% unique()



stopCluster(cl)
registerDoSEQ()  # Revert to sequential processing
