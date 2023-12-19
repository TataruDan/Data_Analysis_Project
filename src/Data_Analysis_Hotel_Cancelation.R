if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
if (!require("summarytools")) install.packages("summarytools", dependencies=TRUE)
if (!require("caret")) install.packages("caret", dependencies=TRUE)
if (!require("randomForest")) install.packages("randomForest", dependencies=TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies=TRUE)
if (!require('stats')) install.packages("stats", dependencies = TRUE)
if (!require('skimr')) install.packages('skimr', dependencies = TRUE)
if (!require('caret')) install.packages('caret', dependencies = TRUE)
if (!require('naniar')) install.packages('naniar', dependencies = TRUE)
if (!require('lubridate')) install.packages('lubridate', dependencies = TRUE)
if (!require('glmnet')) install.packages('glmnet', dependencies = TRUE)
if (!require('rpart')) install.packages('rpart' , dependencies = TRUE)
if (!require('gbm')) install.packages('gbm', dependencies = TRUE)


library(stats)
library(dplyr)
library(ggplot2)
library(summarytools)
library(caret)
library(reshape2)
library(skimr)
library(caret)
library(naniar)
library(lubridate)
library(glmnet)
library(rpart)
library(lightgbm)
library(gbm)

# Use read.csv to read data from csv file
data_hotel <- read.csv('hotel_bookings.csv')


# Print some data
head(data_hotel)

#Get some data info
summary(data_hotel)

str(data_hotel)

data_hotel[data_hotel == "NULL"] <- NA

skim(data_hotel)


#### Step 1. Check on Null Values
# Calculate the count of null values in each column
null_counts <- colSums(is.na(data_hotel))

# Calculate the percentage of null values in each column
null_percentages <- (null_counts / nrow(data_hotel)) * 100

# Create a data frame to display the results
null_df <- data.frame(
  "Null Values" = null_counts,
  "Percentage Null Values" = null_percentages
)

# Print the result
null_df

### Step 2. Replace NA in my data set

# Replace NA values with zeros in the entire data frame
data_hotel[is.na(data_hotel)] <- 0

# Print the data frame with null values replaced
print(data_hotel)



## Check missing values in the data set
# Create a missing data visualization using gg_miss_upset()

missing_summary <- miss_var_summary(data_hotel)
print(missing_summary,n=32)


data_hotel <- data_hotel %>%
  filter(!(children == 0 & adults == 0 & babies == 0))

print(data_hotel)


### Step 3. EDA (Exploratory Data Analysis)

# Creează un grafic de tip bar (barchart)
ggplot(data_hotel, aes(x = hotel, fill = hotel)) +
  geom_bar() +
  labs(x = "Hotel", y = "Count") +
  ggtitle("Hotel Types") +
  theme_minimal() +
  theme(legend.position = "none")


country_wise_guests <- data_hotel %>%
  filter(is_canceled == 0) %>%
  count(country) %>%
  rename('No of guests' = n) %>%
  arrange(desc('No of guests'))

# Print the result
print(country_wise_guests)

# Folosiți funcțiile dplyr pentru a obține numărul de oaspeți pe țară
country_wise_guests <- data_hotel %>%
  filter(is_canceled == 0) %>%
  group_by(country) %>%
  summarize(`No of guests` = n()) %>%
  arrange(desc(`No of guests`))

# Afișați rezultatul
print(country_wise_guests)

# Create a bar chart using ggplot2
ggplot(country_wise_guests[1:10, ], aes(x = reorder(country, -`No of guests`), y = `No of guests`)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Country", y = "Number of Guests") +
  ggtitle("Top 10 Countries by Number of Guests") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Check on cancelations 


# Create a bar chart for 'is_canceled' column
ggplot(data_hotel, aes(x = factor(is_canceled))) +
  geom_bar(fill = 'blue') +
  coord_flip() +
  labs(x = 'Canceled or Not Canceled', y = 'Count') +
  ggtitle('Canceled Situation') +
  theme_minimal()


# Create a violin plot for 'arrival_date_year' vs 'lead_time' vs 'is_canceled'
ggplot(data_hotel, aes(x = factor(arrival_date_year), y = lead_time, fill = factor(is_canceled))) +
  geom_violin(scale = "width", trim = FALSE) +
  labs(x = "Year", y = "Lead Time") +
  ggtitle("Arrival Year vs Lead Time vs Canceled Situation") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()

# Create a new column 'arrival_date_month_numeric' with numeric values
hotel_data_mod <- data_hotel %>%
  mutate(arrival_date_month_numeric = case_when(
    arrival_date_month == 'January' ~ 1,
    arrival_date_month == 'February' ~ 2,
    arrival_date_month == 'March' ~ 3,
    arrival_date_month == 'April' ~ 4,
    arrival_date_month == 'May' ~ 5,
    arrival_date_month == 'June' ~ 6,
    arrival_date_month == 'July' ~ 7,
    arrival_date_month == 'August' ~ 8,
    arrival_date_month == 'September' ~ 9,
    arrival_date_month == 'October' ~ 10,
    arrival_date_month == 'November' ~ 11,
    arrival_date_month == 'December' ~ 12,
    TRUE ~ NA_integer_
  ))

# Create a bar chart for 'arrival_date_month'
ggplot(hotel_data_mod, aes(x = factor(arrival_date_month, levels = unique(arrival_date_month)))) +
  geom_bar(fill = 'orange') +
  labs(x = 'Month', y = 'Count') +
  ggtitle('Arrival Month') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Creează o nouă coloană 'weekend_or_weekday' pe baza datelor existente
hotel_data_mod <- hotel_data_mod %>%
  mutate(weekend_or_weekday = case_when(
    stays_in_weekend_nights > 0 & stays_in_week_nights == 0 ~ 'stay_just_weekend',
    stays_in_weekend_nights == 0 & stays_in_week_nights > 0 ~ 'stay_just_weekday',
    stays_in_weekend_nights > 0 & stays_in_week_nights > 0 ~ 'stay_both_weekday_and_weekend',
    stays_in_weekend_nights == 0 & stays_in_week_nights == 0 ~ 'undefined_data',
    TRUE ~ NA_character_
  ))

group_data <- hotel_data_mod %>%
  group_by(arrival_date_month, weekend_or_weekday) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(arrival_date_month)

# Creează un grafic de tip bar (barchart)
ggplot(group_data, aes(x = factor(arrival_date_month), y = Count, fill = weekend_or_weekday)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(x = "Arrival Month", y = "Count") +
  ggtitle("Arrival Month vs Staying Weekend or Weekday") +
  scale_fill_manual(values = c("stay_just_weekend" = "blue", "stay_just_weekday" = "green", "stay_both_weekday_and_weekend" = "red", "undefined_data" = "gray")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculați numărul de înregistrări pentru fiecare tip de masă
meal_counts <- table(hotel_data_mod$meal)

# Crearea unui cadru de date pentru grafic
meal_data <- data.frame(
  meal_labels = names(meal_counts),
  size = as.vector(meal_counts)
)

# Crearea graficului tip donut
ggplot(meal_data, aes(x = 2, y = size, fill = meal_labels)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(x = "", y = "") +
  ggtitle("Meal Types") +
  theme_void() +
  scale_fill_manual(values = c("BB" = "red", "HB" = "green", "SC" = "blue", "Undefined" = "gray", "FB" = "purple")) +
  theme(plot.title = element_text(hjust = 0.5))


# Creează un grafic de tip bar (barchart)
ggplot(hotel_data_mod, aes(x = market_segment, fill = market_segment)) +
  geom_bar() +
  labs(x = "Market Segment", y = "Count") +
  ggtitle("Market Segment Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Filter the data for is_canceled == 0
data <- subset(data_hotel, is_canceled == 0)

# Create a boxplot using ggplot2
ggplot(data, aes(x = adr, y = reserved_room_type, fill = hotel)) +
  geom_boxplot() +
  labs(x = "ADR", y = "Reserved Room Type") +
  ggtitle("Box Plot for ADR by Reserved Room Type") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Resort Hotel" = "blue", "City Hotel" = "red")) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
  coord_flip()


# Filter data for 'Resort Hotel' and 'City Hotel' with 'is_canceled' == 0
data_resort <- data_hotel %>%
  filter(hotel == 'Resort Hotel', is_canceled == 0)

data_city <- data_hotel %>%
  filter(hotel == 'City Hotel', is_canceled == 0)


# Calculate the mean of 'adr' grouped by 'arrival_date_month'
resort_hotel <- data_resort %>%
  group_by(arrival_date_month) %>%
  summarize(mean_adr = mean(adr))

resort_hotel <- as.data.frame(resort_hotel) 
print(resort_hotel)


# Calculate the average adr per month for City Hotel
city_hotel <- data_city %>%
  group_by(arrival_date_month) %>%
  summarize(avg_adr = mean(adr))

city_hotel <- as.data.frame(city_hotel)
city_hotel

# Merge the data frames by 'arrival_date_month'
final_hotel <- merge(resort_hotel, city_hotel, by = 'arrival_date_month')

# Rename columns
colnames(final_hotel) <- c('month', 'price_for_resort', 'price_for_city_hotel')

# Print the final data frame
print(final_hotel)

# Sort the data frame by 'month'
final_prices <- final_hotel %>%
  arrange(factor(month, levels = month.name))

# Print the sorted data frame
print(final_prices)

# Convert month to a factor in the desired order
final_prices$month <- factor(final_prices$month, levels = month.name)

# Create a line plot using ggplot2 with the 'group' aesthetic
ggplot(final_prices, aes(x = month, group = 1)) +
  geom_line(aes(y = price_for_resort, color = "Resort Hotel"), size = 1) +
  geom_line(aes(y = price_for_city_hotel, color = "City Hotel"), size = 1) +
  labs(x = "Month", y = "Price") +
  ggtitle("Room Price per Night Over the Months") +
  scale_color_manual(
    values = c("Resort Hotel" = "blue", "City Hotel" = "red"),
    labels = c("Resort Hotel", "City Hotel")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))



# Count the number of guests for the resort hotel over each month
resort_guests <- data_resort %>%
  filter(is_canceled == 0) %>%
  group_by(arrival_date_month) %>%
  summarise(no_of_guests = n()) %>%
  ungroup() %>%
  arrange(match(arrival_date_month, month.name))

# Rename the columns
colnames(resort_guests) <- c("month", "no_of_guests")

resort_guests <- resort_guests %>%
  arrange(desc(no_of_guests))

# Print the result
print(resort_guests)


# Count the number of guests for the city hotel over each month
city_guests <- data_city %>%
  filter(is_canceled == 0) %>%
  group_by(arrival_date_month) %>%
  summarise(no_of_guests = n()) %>%
  ungroup() %>%
  arrange(match(arrival_date_month, month.name))

# Rename the columns
colnames(city_guests) <- c("month", "no_of_guests")

city_guests <- city_guests %>%
  arrange(desc(no_of_guests))

# Print the result
print(city_guests)

# Merge the data frames on the 'month' column
final_guests <- merge(resort_guests, city_guests, by = 'month')

# Rename the columns
colnames(final_guests) <- c("month", "no_of_guests_in_resort", "no_of_guests_in_city_hotel")

final_guests <- final_guests %>%
  arrange(desc(no_of_guests_in_resort))

# Print the result
print(final_guests)

# Sort the data frame by 'month'
final_guests <- final_guests %>%
  arrange(match(month, month.name))

# Print the sorted result
print(final_guests)

# Specify the order of months
month_order <- factor(final_guests$month, levels = month.name)

# Create a ggplot line plot
ggplot(final_guests, aes(x = month_order,group = 1)) +
  geom_line(aes(y = no_of_guests_in_resort, color = "Resort Hotel"), size = 1) +
  geom_line(aes(y = no_of_guests_in_city_hotel, color = "City Hotel"), size = 1) +
  labs(x = "Month", y = "Number of Guests") +
  ggtitle("Number of Guests Over the Months") +
  scale_color_manual(
    values = c("Resort Hotel" = "blue", "City Hotel" = "red"),
    labels = c("Resort Hotel", "City Hotel")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))

##################################################3

# Filter the data frame to include only rows where 'is_canceled' is 0
filtered_data <- data_hotel %>%
  filter(is_canceled == 0)

# Print the resulting data frame
print(filtered_data)

# Create a new column 'total_nights' by calculating the sum of two columns
filtered_data$total_nights <- filtered_data$stays_in_weekend_nights + filtered_data$stays_in_week_nights

# Print the resulting data frame
print(filtered_data)

stay <- filtered_data %>%
  group_by(total_nights, hotel) %>%
  summarize("Number of stays" = n()) %>%
  ungroup()

# Print the resulting data frame
print(stay)

ggplot(stay, aes(x = as.factor(total_nights), y = `Number of stays`, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Total Nights", y = "Number of Stays", fill = "Hotel") +
  ggtitle("Number of Stays by Total Nights and Hotel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Resort Hotel" = "blue", "City Hotel" = "red"))



###############################################
# Heat Map
# Selectați coloanele dorite pentru datele categorice
hotel_data_categorical <- hotel_data_mod %>%
  select(hotel, is_canceled, arrival_date_month, meal, country, market_segment,
         distribution_channel, is_repeated_guest, reserved_room_type, assigned_room_type,
         deposit_type, agent, customer_type, reservation_status, weekend_or_weekday)

# Afișați informații despre noul cadru de date
str(hotel_data_categorical)

# Selectați doar coloanele numerice
hotel_data_numerical <- hotel_data_mod %>%
  select_if(is.numeric)

# Afișați informații despre noul cadru de date
str(hotel_data_numerical)

# Calculate the correlation matrix
corr_matrix <- cor(hotel_data_numerical)

# Create a heatmap of the correlation matrix
ggplot(data = melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "Variables", y = "Variables") +
  theme(plot.title = element_text(size = 15))


###############################################
# Assuming your dataframe is named filtered_data_hotel

numeric_columns <- sapply(hotel_data_mod, is.numeric)
correlation_results <- cor(hotel_data_mod[, numeric_columns])
sorted_correlation <- sort(abs(correlation_results["is_canceled", ]), decreasing = TRUE)
print(sorted_correlation)

#########
useless_col <- c(
  "days_in_waiting_list", "arrival_date_year", "assigned_room_type",
  "booking_changes", "reservation_status", "country", "days_in_waiting_list"
)

# Drop the specified columns
data_hotel <- data_hotel %>%
  select(-one_of(useless_col))

head(data_hotel)

# Assuming your dataframe is named hotel_data_dropped
data_hotel$hotel <- factor(data_hotel$hotel)
data_hotel$arrival_date_month <- factor(data_hotel$arrival_date_month)
data_hotel$meal <- factor(data_hotel$meal)
data_hotel$market_segment <- factor(data_hotel$market_segment)
data_hotel$distribution_channel <- factor(data_hotel$distribution_channel)
data_hotel$reserved_room_type <- factor(data_hotel$reserved_room_type)
data_hotel$deposit_type <- factor(data_hotel$deposit_type)
data_hotel$customer_type <- factor(data_hotel$customer_type)
data_hotel$reservation_status_date <- factor(data_hotel$reservation_status_date)

head(data_hotel)


####### CATEGORIAL COLUMNS ########
# Find categorical columns
cat_cols <- sapply(data_hotel, is.factor)

# Extract column names
cat_col_names <- names(cat_cols[cat_cols == TRUE])

# Print the names of categorical columns
cat_col_names


cat_data <- data_hotel %>%
  select(all_of(cat_col_names))

# Print the first few rows of the categorical data frame
head(cat_data)

# Convert 'reservation_status_date' to a POSIXct (datetime) object
cat_data$reservation_status_date <- as.POSIXct(cat_data$reservation_status_date)

# Extract year, month, and day
cat_data$year <- year(cat_data$reservation_status_date)
cat_data$month <- month(cat_data$reservation_status_date)
cat_data$day <- day(cat_data$reservation_status_date)

# Assuming 'cat_data' is your data frame
cat_data <- cat_data %>%
  select(-c(reservation_status_date, arrival_date_month))

# Print the updated data frame
head(cat_data)


# Assuming 'cat_data' is your data frame
for (col in names(cat_data)) {
  cat(paste(col, ":\n", unique(cat_data[[col]]), "\n\n"))
}



# Encoding 'hotel' column
cat_data$hotel <- recode(cat_data$hotel,
                         'Resort Hotel' = 0,
                         'City Hotel' = 1)

# Encoding 'meal' column
cat_data$meal <- recode(cat_data$meal,
                        'BB' = 0,
                        'FB' = 1,
                        'HB' = 2,
                        'SC' = 3,
                        'Undefined' = 4)

# Encoding 'market_segment' column
cat_data$market_segment <- recode(cat_data$market_segment,
                                  'Direct' = 0,
                                  'Corporate' = 1,
                                  'Online TA' = 2,
                                  'Offline TA/TO' = 3,
                                  'Complementary' = 4,
                                  'Groups' = 5,
                                  'Undefined' = 6,
                                  'Aviation' = 7)

# Encoding 'distribution_channel' column
cat_data$distribution_channel <- recode(cat_data$distribution_channel,
                                        'Direct' = 0,
                                        'Corporate' = 1,
                                        'TA/TO' = 2,
                                        'Undefined' = 3,
                                        'GDS' = 4)

# Encoding 'reserved_room_type' column
cat_data$reserved_room_type <- recode(cat_data$reserved_room_type,
                                      'C' = 0,
                                      'A' = 1,
                                      'D' = 2,
                                      'E' = 3,
                                      'G' = 4,
                                      'F' = 5,
                                      'H' = 6,
                                      'L' = 7,
                                      'B' = 8)

# Encoding 'deposit_type' column
cat_data$deposit_type <- recode(cat_data$deposit_type,
                                'No Deposit' = 0,
                                'Refundable' = 1,
                                'Non Refund' = 3)

# Encoding 'customer_type' column
cat_data$customer_type <- recode(cat_data$customer_type,
                                 'Transient' = 0,
                                 'Contract' = 1,
                                 'Transient-Party' = 2,
                                 'Group' = 3)

# Encoding 'year' column
cat_data$year <- recode(cat_data$year,
                        '2015' = 0,
                        '2014' = 1,
                        '2016' = 2,
                        '2017' = 3)

# Print the updated data frame
head(cat_data)


########## NUMERIC ###########
# Create a new data frame with only numerical columns (excluding 'is_canceled')
num_data <- data_hotel[, !(names(data_hotel) %in% c(cat_cols, 'is_canceled'))]

# Convert 'agent' and 'company' columns to numeric (assuming they contain numeric values)
num_data$agent <- as.numeric(num_data$agent)
num_data$company <- as.numeric(num_data$company)

numerical_data <- num_data[sapply(num_data, is.numeric)]
names(numerical_data)
##############################


######### Variance ###########
# Calculate the variance of numerical columns
variance_values <- sapply(numerical_data, var)

# Print the variance values
variance_values

# Apply logarithm transformation to selected columns
numerical_data$lead_time <- log(numerical_data$lead_time + 1)
numerical_data$arrival_date_week_number <- log(numerical_data$arrival_date_week_number + 1)
numerical_data$arrival_date_day_of_month <- log(numerical_data$arrival_date_day_of_month + 1)
numerical_data$agent <- log(numerical_data$agent + 1)
numerical_data$company <- log(numerical_data$company + 1)
numerical_data$adr <- log(numerical_data$adr + 1)

# Print the updated numerical data frame
head(numerical_data)

# Calculate the variance of numerical columns
variance_values <- apply(numerical_data, 2, var)

# Print the variance values
variance_values



# Calculate the mean of the 'adr' column
adr_mean <- mean(numerical_data$adr, na.rm = TRUE)

# Replace missing values in the 'adr' column with the mean
numerical_data$adr <- ifelse(is.na(numerical_data$adr), adr_mean, numerical_data$adr)

# Print the updated numerical data frame
head(numerical_data)
##############################
####### Data Extracting ######

data_hotel <- na.omit(data_hotel)

# 2. Regularization (L1 or L2)
# Use glmnet with L1 regularization to mitigate convergence issues
data_hotel$agent <- as.numeric(data_hotel$agent)
data_hotel$company <- as.numeric(data_hotel$company)
# Separate the target variable from predictors
X <- data_hotel[, -which(names(data_hotel) == 'is_canceled')]  # Exclude the target column
y <- data_hotel$is_canceled  # Select the target column

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

###### Gradient Boosting model ########


# Create the Gradient Boosting model
gb_model <- gbm(y_train ~ ., data = data.frame(X_train, y_train), n.trees = 100, distribution = "bernoulli")

# Make predictions on the test set
y_pred_gb <- predict(gb_model, newdata = data.frame(X_test), n.trees = 100, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
y_pred_binary <- ifelse(y_pred_gb > 0.5, 1, 0)


# Calculate accuracy
accuracy_gb <- sum(y_pred_binary == y_test) / length(y_test)

# Calculate the confusion matrix
confusion_matrix <- table(Actual = y_test, Predicted = y_pred_binary)

# Print results
cat("Accuracy Score of Gradient Boosting Classifier is:", accuracy_gb, "\n")
cat("Confusion Matrix:\n", confusion_matrix, "\n")

classification_report <- confusionMatrix(data = as.factor(y_pred_binary), reference = as.factor(y_test))
print(classification_report)

###############################
##### LightGBM model ########

# Define the LightGBM model with learning rate = 1
lgbm <- lightgbm(data = as.matrix(X_train), label = y_train, objective = "binary")

# Make predictions on the test data
y_pred_lgbm <- predict(lgbm, newdata = as.matrix(X_test))
# Convert predicted probabilities to binary predictions
y_pred_lgbm <- ifelse(y_pred_lgbm > 0.5, 1, 0)

# Calculate accuracy
acc_lgbm <- sum(y_pred_lgbm == y_test) / length(y_test)

# Calculate confusion matrix
conf <- table(y_test, y_pred_lgbm)


# Convert y_test and y_pred_lgbm to factors
y_test <- as.factor(y_test)
y_pred_lgbm <- as.factor(y_pred_lgbm)

# Create a confusion matrix object
confusion_matrix <- confusionMatrix(y_pred_lgbm, y_test)

# Print accuracy, confusion matrix, and classification report
cat("Accuracy Score of LightGBM Classifier is:", acc_lgbm, "\n")
cat("Confusion Matrix:\n", as.table(conf), "\n")
cat("Classification Report:\n")
print(confusion_matrix)

########## XXXX #############
# Assuming you've already loaded and preprocessed your data, and split it into X_train, y_train, X_test, and y_test

# 3. Apply L1 regularization using glmnet
alpha_value <- 1  # L1 regularization (Lasso)
lambda_values <- 10^seq(-3, 3, by = 0.1)  # Range of lambda values

# Fit the glmnet model with cross-validation
cv_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = alpha_value, lambda = lambda_values)

# Find the best lambda value
best_lambda <- cv_model$lambda.min

# Fit the final model with the best lambda
final_model <- glmnet(as.matrix(X_train), y_train, alpha = alpha_value, lambda = best_lambda)

# Make predictions on the test set
y_pred <- predict(final_model, newx = as.matrix(X_test), s = best_lambda, type = "response")

# Convert probabilities to binary predictions (0 or 1)
y_pred_binary <- ifelse(y_pred > 0.5, 1, 0)

# 4. Evaluate the model
accuracy_n <- sum(y_pred_binary == y_test) / length(y_test)
confusion_matrix <- table(Actual = y_test, Predicted = y_pred_binary)

# Print results
cat("Accuracy Score:", accuracy, "\n")
cat("Confusion Matrix:\n", confusion_matrix, "\n")

classification_report <- confusionMatrix(data = as.factor(y_pred_binary), reference = as.factor(y_test))
print(classification_report)

######### Decision Tree ############

agent_levels <- unique(X_train$agent)
company_levels <- unique(X_train$company)

# Convert 'agent' and 'company' columns to factors with the same levels
X_train$agent <- factor(X_train$agent, levels = agent_levels)
X_train$company <- factor(X_train$company, levels = company_levels)
X_test$agent <- factor(X_test$agent, levels = agent_levels)
X_test$company <- factor(X_test$company, levels = company_levels)

# Create a Decision Tree classifier
dtc_model <- rpart(y_train ~ ., data = X_train, method = "class")

# Make predictions on the test set
y_pred_dtc <- predict(dtc_model, X_test, type = "class")

# Create a confusion matrix object
confusion_matrix <- confusionMatrix(as.factor(y_pred_dtc), as.factor(y_test))

# Calculate precision, recall, and F1-score
precision <- confusion_matrix$byClass["Pos Pred Value"]
recall <- confusion_matrix$byClass["Sensitivity"]
f1_score <- confusion_matrix$byClass["F1"]

# Print the classification report
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")
dec_prec <- confusion_matrix$overall["Accuracy"]

# Print the accuracy
cat("Accuracy Score of Decision Tree is:", confusion_matrix$overall["Accuracy"], "\n")

# Print the confusion matrix
cat("Confusion Matrix:\n", confusion_matrix$table, "\n")

######### Logistic Regression ##########

# Set a seed for reproducibility
set.seed(123)

# Subset the data to 5000 rows
data_subset <- data_hotel %>%
  sample_n(5000)

# Split the data into training and testing sets
set.seed(456)  # Set another seed for data splitting
train_index <- createDataPartition(data_subset$is_canceled, p = 0.8, list = FALSE)
X_train <- data_subset[train_index, -which(names(data_subset) == 'is_canceled')]
y_train <- data_subset[train_index, 'is_canceled']
X_test <- data_subset[-train_index, -which(names(data_subset) == 'is_canceled')]
y_test <- data_subset[-train_index, 'is_canceled']

# Fit a logistic regression model
lr_model <- glm(is_canceled ~ ., data = data_subset, family = binomial(link = 'logit'))

# Make predictions on the test set
y_pred_lr <- predict(lr_model, newdata = data.frame(X_test), type = 'response')

# Convert predicted probabilities to binary predictions
y_pred_lr <- ifelse(y_pred_lr >= 0.5, 1, 0)

accuracy_log_reg <- sum(y_pred_lr == y_test) / length(y_test)
conf <- table(Actual = y_test, Predicted = y_pred_lr)
precision <- conf[2, 2] / sum(conf[, 2])  # TP / (TP + FP)
recall <- conf[2, 2] / sum(conf[2, ])     # TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print results
cat("Accuracy Score of Logistic Regression is :", round(accuracy_log_reg, 4), "\n")
cat("Confusion Matrix :\n", conf, "\n")
cat("Precision: ", round(precision, 2), "\n")
cat("Recall: ", round(recall, 2), "\n")
cat("F1 Score: ", round(f1_score, 2), "\n")


# Create a data frame with model names and scores
models <- data.frame(
  Model = c('Gradient Boosting', 'LightGBM', 'XXXX','Decision Tree', 'Logistic Regression'),
  Score = c(accuracy_gb, acc_lgbm, accuracy_n, dec_prec, accuracy_log_reg)
)
print(models)
# Sort the data frame by score in descending order
sorted_models <- models[order(models$Score, decreasing = TRUE),]

# Print the sorted data frame
print(sorted_models)
