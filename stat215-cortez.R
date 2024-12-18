## Project:  STA 215, Spring 2024, Final Project
# Located:   Posit Cloud
# File Name: lorde songs
# Date:      2024_2_29
# Who:       Kevin Janas



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
raw_data <- read_delim("raw_data.csv")
data <- na.omit(raw_data)



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################


# Length of the song
mean(data$poverty_2020)
sd(data$poverty_2020)
hist(data$poverty_2020)
summary(data$poverty_2020)
min(data$poverty_2020)
max(data$poverty_2020)


# Streams per song
mean(data$violent_crime_rate_2019)
sd(data$violent_crime_rate_2019)
hist(data$violent_crime_rate_2019)
summary(data$violent_crime_rate_2019)
min(data$violent_crime_rate_2019)
max(data$violent_crime_rate_2019)


# Statistic on release name
table(data$pop_over_500k_2021)

# Statistic on emotion felt during the song
table(data$imgrate10)




##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
boxplot(data$births_2020 ~ data$pop_over_500k_2021)
anova <- aov(data$births_2020 ~ data$pop_over_500k_2021)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
plot(data$poverty_2020, data$violent_crime_rate_2019)

# add x line and y line for means
meanx <- mean(data$poverty_2020)
meany <- mean(data$violent_crime_rate_2019)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(data$violent_crime_rate_2019 ~ poverty_2020, data = data)
summary(linear_relationship)


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$poverty_2020, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$pop_over_500k_2021, data$imgrate10)
chisq.test(data$pop_over_500k_2021, data$imgrate10)