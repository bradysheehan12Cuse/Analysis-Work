library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(scales)

file_path <- "Plant & Coconut Water Data_MULO + Food_WE 5.23.24.xlsx"
data <- read_excel(file_path, sheet = "MULO 52WK_5.23.24")

head(data)

print(colnames(data))


data <- data %>%
  rename(
    Brand = `SS COCONUT & PLANT WATER`, 
    Rank = `...2`,
    Rank_PP = `...3`,
    Description = `...4`, 
    UPC = `...5`,
    Sales = `...6`, 
    Sales_Change = `...7`,
    Dol_Shr_Product_Group = `...8`,
    Dol_Shr_Product_Group_Chg = `...9`,
    Units = `...10`,
    ARP = `...11`,
    ARP_Chg = `...12`,
    Max_ACV = `...13`,
    Max_ACV_Chg = `...14`,
    Dollars_SPM = `...15`,
    Dollars_SPM_Chg = `...16`,
    Dollars_Promo = `...17`,
    Dollars_Promo_Chg = `...18`,
    Dollars_SPP = `...19`,
    Dollars_SPP_Chg = `...20`
  ) %>%
  mutate(
    Sales = as.numeric(Sales),
    Sales_Change = as.numeric(Sales_Change),
    Dol_Shr_Product_Group = as.numeric(Dol_Shr_Product_Group),
    Dol_Shr_Product_Group_Chg = as.numeric(Dol_Shr_Product_Group_Chg),
    Units = as.numeric(Units),
    ARP = as.numeric(ARP),
    ARP_Chg = as.numeric(ARP_Chg),
    Max_ACV = as.numeric(Max_ACV),
    Max_ACV_Chg = as.numeric(Max_ACV_Chg),
    Dollars_SPM = as.numeric(Dollars_SPM),
    Dollars_SPM_Chg = as.numeric(Dollars_SPM_Chg),
    Dollars_Promo = as.numeric(Dollars_Promo),
    Dollars_Promo_Chg = as.numeric(Dollars_Promo_Chg),
    Dollars_SPP = as.numeric(Dollars_SPP),
    Dollars_SPP_Chg = as.numeric(Dollars_SPP_Chg)
  ) %>%
  filter(!is.na(Sales))

data_clean <- data %>%
  filter(complete.cases(.))

data



summary_stats <- data_clean %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE),
            Average_Sales = mean(Sales, na.rm = TRUE),
            Max_Sales = max(Sales, na.rm = TRUE),
            Min_Sales = min(Sales, na.rm = TRUE),
            Sales_Change = mean(Sales_Change, na.rm = TRUE))

print(summary_stats)





# Sales distribution plot with formatted x-axis
ggplot(data_clean, aes(x = Sales)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Sales") +
  xlab("Sales") +
  ylab("Frequency") +
  scale_x_continuous(labels = scales::comma)


# Sales by Brand plot with formatted x-axis
ggplot(data_clean, aes(x = reorder(Brand, Sales, FUN = sum), y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Total Sales by Brand") +
  xlab("Brand") +
  ylab("Total Sales") +
  scale_y_continuous(labels = scales::comma)

# Sales change over time
ggplot(data_clean, aes(x = Sales_Change)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Sales Change") +
  xlab("Sales Change (%)") +
  ylab("Frequency")



# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data_clean$Sales, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data_clean[trainIndex,]
test_data <- data_clean[-trainIndex,]


# Train a random forest model
model <- randomForest(Sales ~ ., data = train_data, 
                      importance = TRUE, 
                      ntree = 500)


# Evaluate the model
predictions <- predict(model, test_data)
rmse <- sqrt(mean((predictions - test_data$Sales)^2))
print(paste("RMSE:", rmse))

# Feature importance
importance <- importance(model)
varImpPlot(model)


# Sales summary by brand
brand_sales_summary <- data_clean %>%
  group_by(Brand) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Average_Sales = mean(Sales, na.rm = TRUE),
    Median_Sales = median(Sales, na.rm = TRUE)
  )
print(brand_sales_summary)


# Plot total, average, and median sales by brand
ggplot(brand_sales_summary, aes(x = reorder(Brand, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Total Sales by Brand") +
  xlab("Brand") +
  ylab("Total Sales") +
  scale_y_continuous(labels = scales::comma)

ggplot(brand_sales_summary, aes(x = reorder(Brand, Average_Sales), y = Average_Sales)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Average Sales by Brand") +
  xlab("Brand") +
  ylab("Average Sales") +
  scale_y_continuous(labels = scales::comma)

ggplot(brand_sales_summary, aes(x = reorder(Brand, Median_Sales), y = Median_Sales)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Median Sales by Brand") +
  xlab("Brand") +
  ylab("Median Sales") +
  scale_y_continuous(labels = scales::comma)

data_clean2=data_clean




# Sales change analysis by brand
sales_change_summary <- data_clean %>%
  group_by(Brand) %>%
  summarise(
    Total_Sales_Change = sum(Sales_Change, na.rm = TRUE),
    Average_Sales_Change = mean(Sales_Change, na.rm = TRUE)
  )

# Print the sales change summary
print(sales_change_summary)

# Plot total and average sales change by brand
ggplot(sales_change_summary, aes(x = reorder(Brand, Total_Sales_Change), y = Total_Sales_Change)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Total Sales Change by Brand") +
  xlab("Brand") +
  ylab("Total Sales Change") +
  scale_y_continuous(labels = scales::comma)

ggplot(sales_change_summary, aes(x = reorder(Brand, Average_Sales_Change), y = Average_Sales_Change)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Average Sales Change by Brand") +
  xlab("Brand") +
  ylab("Average Sales Change") +
  scale_y_continuous(labels = scales::comma)






library(ggcorrplot)
# Correlation matrix
correlation_matrix <- data_clean %>%
  select(Sales, Sales_Change, Dol_Shr_Product_Group, Units, ARP, Dollars_SPM, Dollars_Promo, Dollars_SPP) %>%
  cor(use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Plot the correlation matrix


ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, colors = c("blue", "white", "red"), title = "Correlation Matrix", ggtheme = theme_minimal)









# Scatter plot of sales vs. units sold
ggplot(data_clean, aes(x = Units, y = Sales)) +
  geom_point(color = "blue", alpha = 0.5) +
  theme_minimal() +
  ggtitle("Sales vs. Units Sold") +
  xlab("Units Sold") +
  ylab("Sales") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)








colnames(data_clean)


#Zico

# Filter data for Zico products
zico_data <- data_clean %>% filter(grepl("Zico", Brand, ignore.case = TRUE))
other_data <- data_clean %>% filter(!grepl("Zico", Brand, ignore.case = TRUE))

colnames(zico_data)

# Filter data for Zico products
zico_data <- data %>% filter(grepl("Zico", Brand, ignore.case = TRUE))
other_data <- data %>% filter(!grepl("Zico", Brand, ignore.case = TRUE))
combined_data <- bind_rows(zico_data, other_data)
# Summary statistics
total_sales_zico <- sum(zico_data$Sales, na.rm = TRUE)
total_sales_other <- sum(other_data$Sales, na.rm = TRUE)

avg_sales_zico <- mean(zico_data$Sales, na.rm = TRUE)
avg_sales_other <- mean(other_data$Sales, na.rm = TRUE)

median_sales_zico <- median(zico_data$Sales, na.rm = TRUE)
median_sales_other <- median(other_data$Sales, na.rm = TRUE)



# Creating a summary statistics dataframe
summary_stats <- data.frame(
  Statistic = c("Total Sales", "Average Sales", "Median Sales"),
  Zico = c(total_sales_zico, avg_sales_zico, median_sales_zico),
  Other_Brands = c(total_sales_other, avg_sales_other, median_sales_other)
)


print(summary_stats)



# Visualizations
# Total Sales by Zico vs Other Brands
total_sales_plot <- ggplot(combined_data, aes(x = Brand, y = Sales, fill = Brand)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Sales: Zico vs Other Brands") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
print(total_sales_plot)
# Average Sales by Zico vs Other Brands
avg_sales <- combined_data %>%
  group_by(Brand) %>%
  summarise(avg_sales = mean(Sales, na.rm = TRUE))

avg_sales_plot <- ggplot(avg_sales, aes(x = Brand, y = avg_sales, fill = Brand)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Sales: Zico vs Other Brands") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
print(avg_sales_plot)
# Median Sales by Zico vs Other Brands
median_sales <- combined_data %>%
  group_by(Brand) %>%
  summarise(median_sales = median(Sales, na.rm = TRUE))

median_sales_plot <- ggplot(median_sales, aes(x = Brand, y = median_sales, fill = Brand)) +
  geom_bar(stat = "identity") +
  ggtitle("Median Sales: Zico vs Other Brands") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
print(median_sales_plot)


#zico graphs

# Visualization: Sales Distribution for Zico Products
sales_distribution_plot <- ggplot(zico_data, aes(x = Sales)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  ggtitle("Sales Distribution for Zico Products") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)
print(sales_distribution_plot)
# Visualization: Sales by Product Description for Zico Products
sales_by_product_plot <- ggplot(zico_data, aes(x = reorder(Description, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  ggtitle("Sales by Product Description for Zico Products") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  xlab("Product Description") +
  ylab("Sales")
print(sales_by_product_plot)


# Visualization: Sales Change Distribution for Zico Products
sales_change_distribution_plot <- ggplot(zico_data, aes(x = Sales_Change)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  ggtitle("Sales Change Distribution for Zico Products") +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent)
print(sales_change_distribution_plot)






# Handle missing values and convert character columns to factors
zico_data <- zico_data %>%
  mutate_if(is.character, as.factor) %>%
  na.omit()


# Select relevant features for the model
features <- zico_data %>%
  select(Sales, Sales_Change, Dol_Shr_Product_Group, Dol_Shr_Product_Group_Chg, Units, ARP, ARP_Chg, Max_ACV, Max_ACV_Chg, Dollars_SPM, Dollars_SPM_Chg, Dollars_Promo, Dollars_Promo_Chg, Dollars_SPP, Dollars_SPP_Chg)


# Train a Random Forest model
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)
model <- train(Sales ~ ., data = features, method = "rf", trControl = train_control)

# Print model results
print(model)

# Variable Importance
importance <- varImp(model, scale = FALSE)
importance_df <- importance$importance %>% as.data.frame() %>% rownames_to_column(var = "Variable")

# Plot Variable Importance
importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Variable Importance in Predicting Sales for Zico Products") +
  xlab("Variables") +
  ylab("Importance")

print(importance_plot)
