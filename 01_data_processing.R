# Script: data_preprocessing.R
# Purpose: Clean and preprocess the raw data for analysis.
# Author: JATO
# Date: 2024-07-23
# Description: This script loads raw data, cleans it, and transforms it for further analysis.
# Usage: Run this script before starting the analysis to prepare the data.

# Load necessary libraries
# library(tidyverse)
# library(janitor)

# Set working directory (if not using R projects)
setwd("C:/Users/Sergio/Downloads/R Projects/analyzing-support-performance")

# Read raw data
df <- read.csv("./data/raw/2023-08-01 2024-08-01 All Channels Report.csv")

#--------------------Data Exploration-----------------
# str(df)
# summary(df)
# head(df)
# colnames(df)

#--------------------Data Cleaning-----------------
# Update column names and filter records by mailbox and closed_by
clean_df <- df %>% 
  janitor::clean_names() %>%
  filter(mailbox == 'lightningstepsupport' & closed_by != 'Lightning Step Support') %>%
  filter(assignee %in% c("Enrique Carreno", "Jose Teran", "David Benalcazar", "Nicolas Jaramillo","Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach"))

# Remove Zen and Form columns, and columns: mailbox, bcc
clean_df <- clean_df %>%
  select(-matches("zen|forms_team"), -mailbox, -bcc)

# Remove "_lightningstepsupport" from all column names
colnames(clean_df) <- gsub("_lightningstepsupport", "", colnames(clean_df))

#--------------------Data Transformation and Feature Engineering-----------------
# Convert data types
# created_at date columns
clean_df <- clean_df %>%
  mutate(
    created_at_datetime = ymd_hms(created_at),
    created_at_date = as.Date(created_at_datetime),
    created_at_year = year(created_at_date),
    created_at_quarter = quarter(created_at_date),
    created_at_month = month(created_at_date),
    created_at_month_name = month(created_at_date, label = TRUE),
    created_at_year_month = format(created_at_date, "%Y-%m"), # Create year-month column
    created_at_year_quarter = paste0(year(created_at_date), "-Q", quarter(created_at_date)),  # New line to create year-quarter column
    created_at_week = week(created_at_date),
    created_at_weekday = wday(created_at_date), # Sunday is 1 and Saturday 7
    created_at_weekday_name = weekdays(created_at_date),
    created_at_day = day(created_at_date),
    created_at_day_of_year = yday(created_at_date),  # Add day of year column
    created_at_isweekend = ifelse(created_at_date %in% c(1, 7), 1, 0),  # Check if the day is a weekend
    created_at_hour = hour(created_at_datetime)
  )

# remove text created_at column
clean_df <- clean_df %>%
  select(-created_at)

# closed_at date columns
clean_df <- clean_df %>%
  mutate(
    closed_at_datetime = ymd_hms(closed_at),
    closed_at_date = as.Date(closed_at_datetime),
    closed_at_year = year(closed_at_date),
    closed_at_quarter = quarter(closed_at_date),
    closed_at_month = month(closed_at_date),
    closed_at_month_name = month(closed_at_date, label = TRUE),
    closed_at_year_month = format(closed_at_date, "%Y-%m"), # Create year-month column
    closed_at_year_quarter = paste0(year(closed_at_date), "-Q", quarter(closed_at_date)),  # New line to create year-quarter column
    closed_at_week = week(closed_at_date),
    closed_at_weekday = wday(closed_at_date), # Sunday is 1 and Saturday 7
    closed_at_weekday_name = weekdays(closed_at_date),
    closed_at_day = day(closed_at_date),
    closed_at_day_of_year = yday(closed_at_date),  # Add day of year column
    closed_at_isweekend = ifelse(closed_at_date %in% c(1, 7), 1, 0),  # Check if the day is a weekend
    closed_at_hour = hour(closed_at_datetime)
  )

# remove text created_at column
clean_df <- clean_df %>%
  select(-closed_at)

# Enrich cleaned_df
clean_df <- clean_df %>%
  mutate(
    rating_comments = as.integer(rating_comments == 'y'),
    resolved = as.integer(resolved == 'y'),
    resolved_on_first_reply = as.integer(resolved_on_first_reply == 'y'),
    isjira = ifelse(str_detect(tags, "jira"), 1, 0), # Add isjira column
    support_team_assignee = case_when(
      assignee %in% c("Jose Teran", "David Benalcazar", "Nicolas Jaramillo") ~ "Financial",
      assignee %in% c("Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach") ~ "Clinical/Medical",
      TRUE ~ "Other"
    ),
    support_team_closed_by = case_when(
      closed_by %in% c("Jose Teran", "David Benalcazar", "Nicolas Jaramillo") ~ "Financial",
      closed_by %in% c("Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach") ~ "Clinical/Medical",
      TRUE ~ "Other"
    )
  )

# #--------------------Data Export-----------------
# # Export the cleaned data
# write_csv(clean_df, "./data/processed/clean_data.csv")

#--------------------Filter Cleaned Data -----------------
# Filter tickets created in 2024
data_2024 <- clean_df %>%
  filter(created_at_date >= "2024-01-01")
