library(ggplot2)
library(ggplot)
library(corrplot)
library(data.table)
library(readr)
library(dplyr)
library(tidyquant)
library(mltools)
library(lubridate)
library(tidytext)
#install.packages("syuzhet")
library(syuzhet)
library(readxl)
require(stringi)
require(stringr)
library("bigrquery")

bq_auth(use_oob = TRUE, cache = TRUE)
#4/1AdQt8qi748AZfhMTfIhEt66IDrsXE6Ybt-wG7atNnWXcxRGMJPA1_JW4EKM
# Store the project id
projectid_kitty = "kittys-ad-tech"
# Set the query
#sql <- "select * from `marketlytics-dataware-house.analytics_260337345.DOF_staffmeup`;"
sql_kitty <- "select receipient_email, subject, count(distinct click_time) as number_of_unique_clicks, max(duration_minutes) as time_taken_for_last_click, min(duration_minutes) as time_taken_for_first_click, avg(duration_minutes) as average_time_taken_for_click, avg(subtotal) as average_revenue, sum(subtotal) as total_revenue from `kittys-ad-tech.aws_emails.types_of_times_1` group by 1,2 having count(distinct click_time) = count(click_time)  order by 8 desc"
sql_kitty_saad <- "select * from `marketlytics-dataware-house.training_affan.kitty_future_saad`"

# Run the query
project_query_kitty <- bq_project_query(projectid_kitty, sql_kitty_saad, use_legacy_sql = FALSE)
# Download result to dataframe
kitty <- bq_table_download(project_query_kitty)
str(kitty)
summary(kitty)
View(kitty)

kitty[sapply(kitty, is.integer)] <- lapply(kitty[sapply(kitty, is.integer)], 
                                           as.numeric)
kitty = kitty %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))

kitty[sapply(kitty, is.character)] <- lapply(kitty[sapply(kitty, is.character)], as.factor)

kitty %>%  select_if(is.numeric) %>% cor() %>% View()
kitty %>%  select_if(is.numeric) %>% cor() %>% corrplot()
kitty$subject_length = stri_length(kitty$subject)
kitty$subject_words = str_count(kitty$subject," ")+1
kitty$body_words = stri_count_words(kitty$text_length_without_link)
kitty$text_length_without_link = stri_length(kitty$text_part_without_link)
kitty$email_domain = as.factor(sub(".*@", "", kitty$email))
#sentiments by subject
data_set1_tokens = kitty %>% summarize(subject, subject2 = subject)
data_set1_tokens = data_set1_tokens %>% unnest_tokens(output = "word", token = "words", input = subject2) %>% anti_join(stop_words)
data_set1_tokens = data_set1_tokens %>% left_join(get_sentiments("nrc")) %>% na.omit 
data_set1_tokens = data_set1_tokens %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
data_set1_tokens$sentiment = as.factor(data_set1_tokens$sentiment)
new = data_set1_tokens %>% group_by(subject, word, sentiment) %>% summarize()
new  = one_hot(as.data.table(new))
new$subject = as.factor(new$subject)
new %>% View()
new = new %>% group_by(subject) %>% summarize(subject, 
                                              subject_sentiment_anticipation = sum(sentiment_anticipation),
                                              subject_sentiment_positive =sum(sentiment_positive),
                                              subject_sentiment_trust=sum(sentiment_trust),
                                              subject_sentiment_anger=sum(sentiment_anger),
                                              subject_sentiment_disgust=sum(sentiment_disgust),
                                              subject_sentiment_fear=sum(sentiment_fear),
                                              subject_sentiment_negative=sum(sentiment_negative),
                                              subject_sentiment_joy=sum(sentiment_joy),
                                              subject_sentiment_surprise=sum(sentiment_surprise),
                                              subject_sentiment_sadness=sum(sentiment_sadness)
) %>% unique(.)
output_data = kitty %>% summarize(subject, subject_wise_revenue) %>% unique(.)
kitty = left_join(kitty, new, by = "subject") 
result = left_join(output_data, new, by = "subject")
kitty$subject_wise_revenue = as.numeric(kitty$subject_wise_revenue)
#merge(kitty$subject_wise_revenue, new)
kitty %>% summarize(subject, subject_wise_revenue) %>% unique(.) %>% View()
result %>%  select_if(is.numeric) %>% cor() %>% corrplot()
practice = kitty
kitty = kitty %>% unique(.)


#sentiments by text
data_set1_tokens = kitty %>% summarize(text_part_without_link, text_part_without_link2 = text_part_without_link)
data_set1_tokens = data_set1_tokens %>% unnest_tokens(output = "word", token = "words", input = text_part_without_link2) %>% anti_join(stop_words)
data_set1_tokens = data_set1_tokens %>% left_join(get_sentiments("nrc")) %>% na.omit 
kitty_product = read.csv('kitty_products.csv')
kitty_product = kitty_product %>% unnest_tokens(output = "word", token = "words", input = name) %>% anti_join(stop_words) %>% unique(.)
data_set1_tokens = data_set1_tokens %>% filter(!data_set1_tokens$word %in% kitty_product$word) %>% unique(.)
data_set1_tokens %>% View()
data_set1_tokens = data_set1_tokens %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
data_set1_tokens$sentiment = as.factor(data_set1_tokens$sentiment)
new = data_set1_tokens %>% group_by(text_part_without_link, word, sentiment) %>% summarize()
new  = one_hot(as.data.table(new))
new$text_part_without_link = as.factor(new$text_part_without_link)
new = new %>% unique(.)
new %>% View()
new = new %>% group_by(text_part_without_link) %>% summarize(text_part_without_link, 
                                              text_sentiment_anticipation = sum(sentiment_anticipation),
                                              text_sentiment_positive =sum(sentiment_positive),
                                              text_sentiment_trust=sum(sentiment_trust),
                                              text_sentiment_anger=sum(sentiment_anger),
                                              text_sentiment_disgust=sum(sentiment_disgust),
                                              text_sentiment_fear=sum(sentiment_fear),
                                              text_sentiment_negative=sum(sentiment_negative),
                                              text_sentiment_joy=sum(sentiment_joy),
                                              text_sentiment_surprise=sum(sentiment_surprise),
                                              text_sentiment_sadness=sum(sentiment_sadness)
) %>% unique(.)
output_data = kitty %>% summarize(text_part_without_link, text_part_without_link_wise_revenue) %>% unique(.)
kitty = left_join(kitty, new, by = "text_part_without_link") 
result = left_join(output_data, new, by = "text_part_without_link")
kitty$text_part_without_link_wise_revenue = as.numeric(kitty$text_part_without_link_wise_revenue)
#merge(kitty$text_part_without_link_wise_revenue, new)
kitty %>% summarize(text_part_without_link, text_part_without_link_wise_revenue) %>% unique(.) %>% View()
result %>%  select_if(is.numeric) %>% cor() %>% corrplot()
kitty %>%  select_if(is.numeric) %>% cor() %>% corrplot(type = "upper")
kitty %>%
  select_if(is.numeric) %>%
  mutate_all(~ cor(., kitty$click_score, use = "pairwise.complete.obs"))%>% unique(.) %>% View()


kitty = kitty %>% unique(.)
kitty %>% View()
library(syuzhet)
get_nrc_sentiment("Ugly")