install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("randomForest")
install.packages("caret")
install.packages("janitor")
install.packages("viridis")

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(janitor)
library(RColorBrewer)
library(viridis)
library(caret)


file_path <- file.choose()

item_data <- read_excel(file_path, sheet = "PBX_MEAT SNACKS - ITEM")

brand_data <- read_excel(file_path, sheet = "PBX_MEAT SNACKS - BRAND")

item_data <- clean_names(item_data)
brand_data <- clean_names(brand_data)


head(item_data)
head(brand_data)


# Ensure all character columns are converted to factors in item_data
item_data <- item_data %>%
  mutate_if(is.character, as.factor)

# Ensure all character columns are converted to factors in brand_data
brand_data <- brand_data %>%
  mutate_if(is.character, as.factor)

item_data <- item_data %>%
  select(-geography_label)

brand_data <- brand_data %>%
  select(-geography_label)



str(item_data)
str(brand_data)

colnames(item_data)
colnames(brand_data)


# Create a pie chart for market share, grouping brands with zero share together
market_share_data <- brand_data %>%
  group_by(BRANDS) %>%
  summarise(Total_Share = sum(`$ SHR`, na.rm = TRUE)) %>%
  mutate(BRANDS = ifelse(Total_Share == 0, "Other", as.character(BRANDS)))

# Recalculate market shares after grouping
market_share_data <- market_share_data %>%
  group_by(BRANDS) %>%
  summarise(Total_Share = sum(Total_Share)) %>%
  ungroup()

# Plot the pie chart
ggplot(market_share_data, aes(x = "", y = Total_Share, fill = BRANDS)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Market Share of Big Players", x = "", y = "") +
  theme_void() +
  theme(legend.position = "right") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(Total_Share, 1))), 
            position = position_stack(vjust = 0.5), size = 4, color = "white")




# Identify declining items
# Identify top 10 declining items
top_declining_items <- item_data %>%
  filter(`$ %CHG` < 0) %>%
  arrange(`$ %CHG`) %>%
  head(15)

# Plot a bar chart for declining items
ggplot(top_declining_items, aes(x = reorder(PRODUCT, `$ %CHG`), y = `$ %CHG`, fill = BRANDS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 15 Items Contributing to Decline", x = "Products", y = "Percentage Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 8))





item_data2<- item_data


# Check the number of unique categories in each column
unique_counts <- sapply(item_data, function(x) if(is.factor(x)) length(unique(x)) else NA)
unique_counts

# Remove columns with more than 53 categories
item_data2 <- item_data2 %>%
  select(-product, -trend)




#rf itemdata
set.seed(123)
# Create the Random Forest model for item_data using sku_perf as the target variable
item_rf <- randomForest(sku_perf ~ ., data = item_data2, importance = TRUE, ntree = 500)

# Print the Random Forest model summary
print(item_rf)

# Display variable importance
importance(item_rf)

# Plot variable importance
varImpPlot(item_rf)








unique_counts_brand <- sapply(brand_data, function(x) if(is.factor(x)) length(unique(x)) else NA)
unique_counts_brand

# Handle missing values by removing rows with NA
brand_data <- na.omit(brand_data)


# Random Forest model for brand_data
set.seed(123)
# Create the Random Forest model for brand_data using sku_perf as the target variable
brand_rf <- randomForest(sku_perf ~ ., data = brand_data, importance = TRUE, ntree = 500)

# Print the Random Forest model summary
print(brand_rf)

# Display variable importance
importance(brand_rf)

# Plot variable importance
varImpPlot(brand_rf)



# Create a pie chart for market share, grouping brands with zero share together
market_share_data <- brand_data %>%
  group_by(brands) %>%
  summarise(total_share = sum(shr, na.rm = TRUE)) %>%
  mutate(brands = ifelse(total_share == 0, "Other", as.character(brands)))

# Recalculate market shares after grouping
market_share_data <- market_share_data %>%
  group_by(brands) %>%
  summarise(total_share = sum(total_share)) %>%
  ungroup() %>%
  mutate(percentage = total_share / sum(total_share) * 100)

# Define a custom color palette with at least 19 colors, including green
custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
                    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78",
                    "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d")

# Plot the pie chart with the custom color palette
ggplot(market_share_data, aes(x = "", y = total_share, fill = brands)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add borders to slices
  coord_polar(theta = "y") +
  labs(title = "Market Share of Big Players", x = "", y = "") +
  theme_void() +
  theme(legend.position = "right") +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = custom_palette)  # Apply the custom color palette


















# Split the data into training and testing sets
set.seed(123)
trainIndex_item <- createDataPartition(item_data$sku_perf, p = .8, 
                                       list = FALSE, 
                                       times = 1)
item_train <- item_data2[ trainIndex_item,]
item_test  <- item_data[-trainIndex_item,]

trainIndex_brand <- createDataPartition(brand_data$sku_perf, p = .8, 
                                        list = FALSE, 
                                        times = 1)
brand_train <- brand_data[ trainIndex_brand,]
brand_test  <- brand_data[-trainIndex_brand,]

# Train the Random Forest models
item_rf <- randomForest(sku_perf ~ ., data = item_train, importance = TRUE, ntree = 500)
brand_rf <- randomForest(sku_perf ~ ., data = brand_train, importance = TRUE, ntree = 500)

# Make predictions on the testing set
item_pred <- predict(item_rf, item_test)
brand_pred <- predict(brand_rf, brand_test)

# Calculate performance metrics for item_rf
item_mse <- mean((item_test$sku_perf - item_pred)^2)
item_rmse <- sqrt(item_mse)
item_r2 <- 1 - (sum((item_test$sku_perf - item_pred)^2) / sum((item_test$sku_perf - mean(item_test$sku_perf))^2))

# Calculate performance metrics for brand_rf
brand_mse <- mean((brand_test$sku_perf - brand_pred)^2)
brand_rmse <- sqrt(brand_mse)
brand_r2 <- 1 - (sum((brand_test$sku_perf - brand_pred)^2) / sum((brand_test$sku_perf - mean(brand_test$sku_perf))^2))

# Print the performance metrics
cat("Item Data Random Forest Model Performance:\n")
cat("MSE:", item_mse, "\n")
cat("RMSE:", item_rmse, "\n")
cat("R-squared:", item_r2, "\n\n")

cat("Brand Data Random Forest Model Performance:\n")
cat("MSE:", brand_mse, "\n")
cat("RMSE:", brand_rmse, "\n")
cat("R-squared:", brand_r2, "\n")


