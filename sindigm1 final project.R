# Set working directory
setwd("/courses/STA145/sindigm1")

# Upload data
library(readr)

#Load data
raw_data <- read_delim("FINAL PROJECT - data (1).csv")
data <- raw_data %>%
  filter(complete.cases(.))

# Descriptive Statistics
#qual var
mean(data$year_of_song)
sd(data$year_of_song)
describe(data$year_of_song)
summary(data$year_of_song)

# Descriptive Statistics
#qual var
mean(data$monthly_listeners)
sd(data$monthly_listeners)
describe(data$monthly_listeners)
summary(data$monthly_listeners)

##### STEP 1: Examine the scatter plot
# showing the relationship between year the song came out and monthly streams 

linear_plot <- plot (data$year_of_song, data$monthly_listeners)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$monthly_listeners)
meanx <- mean(data$year_of_song)

abline(h = meany, col = "black")
abline(v = meanx, col = "black")

##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(monthly_listeners ~ year_of_song, data = data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(data$year_of_song, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

