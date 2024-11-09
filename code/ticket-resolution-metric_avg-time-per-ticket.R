# average resolution time per ticket

financial_average_resolution_time <- financial_tickets %>%
  filter(isjira == 0) %>%
  summarise(avg_resolution_time_hours = mean(resolution_time_office_hours_seconds, na.rm = TRUE) / 3600)  # Convert seconds to hours

print(financial_average_resolution_time)

# Calculate average resolution time by month in hours
monthly_avg_resolution <- financial_tickets %>%
  filter(!is.na(closed_at_month)) %>%
  group_by(closed_at_month) %>%
  summarise(avg_resolution_time_hours = mean(resolution_time_office_hours_seconds, na.rm = TRUE) / 3600) # Convert seconds to hours


# Visualize average resolution time by month -----------
ggplot(monthly_avg_resolution, aes(x = closed_at_month, y = avg_resolution_time_hours)) +
  geom_line(color = "#0073C2FF", size = 1) +  # Line for trend
  geom_point(color = "#E69F00", size = 2) +  # Points on the line for emphasis
  labs(title = "Average Ticket Resolution Time by Month",
       x = "Month",
       y = "Average Resolution Time (Hours)") +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Set labels for months
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity


# Visualize average resolution time by month and assignee --------------
# Summarize the average resolution time by month and assignee
monthly_avg_resolution_by_assignee <- financial_tickets %>%
  group_by(assignee, closed_at_month) %>%
  summarise(avg_resolution_time_hours = mean(resolution_time_office_hours_seconds, na.rm = TRUE) / 3600)  # Convert seconds to hours

# View the summarized data
head(monthly_avg_resolution_by_assignee)

# Visualize average resolution time by month grouped by assignee
ggplot(monthly_avg_resolution_by_assignee, aes(x = closed_at_month, y = avg_resolution_time_hours, color = assignee, group = assignee)) +
  geom_line(size = 1) +  # Line for trend
  geom_point(size = 2) +  # Points on the line for emphasis
  labs(title = "Average Ticket Resolution Time by Month and Assignee",
       x = "Month",
       y = "Average Resolution Time (Hours)",
       color = "Assignee") +  # Add color legend for assignees
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Set labels for months
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity


# Visualize average resolution time by month and assignee for only the Financial team --------------
# Summarize the average resolution time by month and assignee
monthly_avg_resolution_by_financial_assignee <- financial_tickets %>%
  filter(support_team_assignee == "Financial") %>%
  group_by(assignee, closed_at_month) %>%
  summarise(avg_resolution_time_hours = mean(resolution_time_office_hours_seconds, na.rm = TRUE) / 3600)  # Convert seconds to hours

# View the summarized data
head(monthly_avg_resolution_by_financial_assignee)

# Visualize average resolution time by month grouped by assignee
ggplot(monthly_avg_resolution_by_financial_assignee, aes(x = closed_at_month, y = avg_resolution_time_hours, color = assignee, group = assignee)) +
  geom_line(size = 1) +  # Line for trend
  geom_point(size = 2) +  # Points on the line for emphasis
  labs(title = "Average Ticket Resolution Time by Month and Assignee",
       x = "Month",
       y = "Average Resolution Time (Hours)",
       color = "Assignee") +  # Add color legend for assignees
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Set labels for months
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity


# Avg Resolution Tier 1 --------------
# Summarize the average resolution time by month and assignee
monthly_avg_resolution_financial_tier1 <- financial_tickets %>%
  filter(support_team_assignee == "Financial",
         assignee != "Jose Teran") %>%
  group_by(assignee, closed_at_month) %>%
  summarise(avg_resolution_time_hours = mean(resolution_time_office_hours_seconds, na.rm = TRUE) / 3600)  # Convert seconds to hours

# View the summarized data
head(monthly_avg_resolution_financial_tier1)

# Visualize average resolution time by month grouped by assignee
ggplot(monthly_avg_resolution_financial_tier1, aes(x = closed_at_month, y = avg_resolution_time_hours, color = assignee, group = assignee)) +
  geom_line(size = 1) +  # Line for trend
  geom_point(size = 2) +  # Points on the line for emphasis
  labs(title = "Average Ticket Resolution Time by Month and Assignee",
       x = "Month",
       y = "Average Resolution Time (Hours)",
       color = "Assignee") +  # Add color legend for assignees
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +  # Set labels for months
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity
