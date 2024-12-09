library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)

wfm_weekly <- read_excel("copy of Blender Bombs SE WFM Data_Assignment Due 6.13.24.xlsx", sheet = "WFM 2023 Weekly")
wfm_avg_per_store <- read_excel("copy of Blender Bombs SE WFM Data_Assignment Due 6.13.24.xlsx", sheet = "WFM2023 Average Per Store")



#fixing the dates
# Remove non-date rows and convert numeric values to dates
wfm_weekly_cleaned <- wfm_weekly %>% 
  filter(!is.na(as.numeric(`WFM REPORTING`))) %>% 
  mutate(Date = as.Date(as.numeric(`WFM REPORTING`), origin = "1899-12-30"))

# Print the cleaned data
print(head(wfm_weekly_cleaned))


# Rename columns for better readability
wfm_weekly_cleaned <- wfm_weekly_cleaned %>% 
  rename(Date = Date, 
         Sales_LW = `LAST WEEK LAST YEAR`, 
         Sales_LWLY = `...3`, 
         YoY_Change = `...4`, 
         Units_LW = `...5`, 
         Units_LWLY = `...6`, 
         Percent_Change = `...7`)

# Print the cleaned and renamed data
print(head(wfm_weekly_cleaned))


# Reformat the Date column to include only the year 2023
wfm_weekly_cleaned$Date <- as.Date(wfm_weekly_cleaned$Date, format = '%m-%d-%y')

# Extract only the year 2023 data
wfm_weekly_cleaned <- wfm_weekly_cleaned[format(wfm_weekly_cleaned$Date, '%Y') == '2023', ]

# Check the head of the dataframe to confirm the Date column is correctly formatted and filtered
print(head(wfm_weekly_cleaned))


# Ensure the columns are properly formatted for the y-axis
wfm_weekly_cleaned$Sales_LW <- as.numeric(wfm_weekly_cleaned$Sales_LW)
wfm_weekly_cleaned$Sales_LWLY <- as.numeric(wfm_weekly_cleaned$Sales_LWLY)
wfm_weekly_cleaned$YoY_Change <- as.numeric(wfm_weekly_cleaned$YoY_Change)
wfm_weekly_cleaned$Units_LW <- as.numeric(wfm_weekly_cleaned$Units_LW)
wfm_weekly_cleaned$Units_LWLY <- as.numeric(wfm_weekly_cleaned$Units_LWLY)

# Check the structure of the dataframe to confirm the changes
str(wfm_weekly_cleaned)

# Re-create the visualizations with the properly formatted y-axis
# Visualization 1: Sales Over Time
sales_plot <- ggplot(wfm_weekly_cleaned, aes(x = Date)) +
  geom_line(aes(y = Sales_LW, color = 'Sales_LW')) +
  geom_line(aes(y = Sales_LWLY, color = 'Sales_LWLY')) +
  labs(title = 'Sales Over Time', x = 'Date', y = 'Sales') +
  scale_color_manual(values = c('Sales_LW' = 'blue', 'Sales_LWLY' = 'red'))

# Visualization 2: YoY Percentage Change Over Time
yoy_plot <- ggplot(wfm_weekly_cleaned, aes(x = Date, y = YoY_Change)) +
  geom_line(color = 'green') +
  labs(title = 'YoY Percentage Change Over Time', x = 'Date', y = 'YoY % Change')

# Visualization 3: Units Sold Over Time
units_plot <- ggplot(wfm_weekly_cleaned, aes(x = Date)) +
  geom_line(aes(y = Units_LW, color = 'Units_LW')) +
  geom_line(aes(y = Units_LWLY, color = 'Units_LWLY')) +
  labs(title = 'Units Sold Over Time', x = 'Date', y = 'Units Sold') +
  scale_color_manual(values = c('Units_LW' = 'blue', 'Units_LWLY' = 'red'))

# Print the plots
print(sales_plot)
print(yoy_plot)
print(units_plot)


summary(wfm_weekly_cleaned)
        
        






#done with weekly sheet



#moving onto average per store sheet




# Convert the date column and handle NA values
wfm_avg_per_store <- wfm_avg_per_store %>%
  mutate(
    Week.Ending = as.Date(as.numeric(`WHOLE FOODS MARKET REPORTING`), origin = '1899-12-30'),
    LW.Sales = as.numeric(`...2`),
    Avg..Store = as.numeric(`...3`),
    Avg..Store.Top.10. = as.numeric(`...4`),
    Avg..Store.Bottom.90. = as.numeric(`...5`),
    Units.Store = as.numeric(`...6`),
    Store.Count = as.numeric(`...7`)
  )

# Filter the data for the year 2023
wfm_avg_per_store_2023 <- wfm_avg_per_store %>%
  filter(!is.na(Week.Ending) & year(Week.Ending) == 2023)

# Display the structure of the cleaned dataframe
str(wfm_avg_per_store_2023)

# Display the first few rows of the cleaned dataframe
print(head(wfm_avg_per_store_2023))


# Ensure the date column correlates for one year being 2023
library(ggplot2)

# Filter the data for the year 2023
wfm_avg_per_store_2023 <- wfm_avg_per_store %>%
  filter(year(Week.Ending) == 2023)

# Create visualizations
# 1. Line plot of LW Sales over time
p1 <- ggplot(wfm_avg_per_store_2023, aes(x = Week.Ending, y = LW.Sales)) +
  geom_line() +
  labs(title = 'LW Sales Over Time in 2023', x = 'Week Ending', y = 'LW Sales') +
  theme_minimal()

# 2. Line plot of Average $/Store over time
p2 <- ggplot(wfm_avg_per_store_2023, aes(x = Week.Ending, y = Avg..Store)) +
  geom_line() +
  labs(title = 'Average $/Store Over Time in 2023', x = 'Week Ending', y = 'Average $/Store') +
  theme_minimal()

# 3. Line plot of Units/Store over time
p3 <- ggplot(wfm_avg_per_store_2023, aes(x = Week.Ending, y = Units.Store)) +
  geom_line() +
  labs(title = 'Units/Store Over Time in 2023', x = 'Week Ending', y = 'Units/Store') +
  theme_minimal()

# Print the plots
print(p1)
print(p2)
print(p3)




panel.background = element_rect(fill = 'transparent', color = NA)












