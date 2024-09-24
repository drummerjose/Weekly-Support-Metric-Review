# trends over time

# weekly
weekly_summary <- financial_tickets_2024 %>%
  group_by(created_at_week) %>%
  summarise(weekly_tickets_created = n())

# Visualizations -------------
# Visualize weekly ticket volume
ggplot(weekly_summary, aes(x = created_at_week, y = weekly_tickets_created)) +
  geom_line(color = "#0073C2FF", size = 1) +  # Line for trend
  geom_point(color = "#E69F00", size = 2) +  # Points on the line for emphasis
  labs(title = "Weekly Ticket Creation Trend in 2024",
       x = "Week of the Year",
       y = "Number of Tickets Created") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 52, 1)) +  # Customize x-axis to show all weeks
  theme(axis.text.x = element_text(hjust = 1))  # Rotate x-axis labels

# monthly
monthly_summary <- financial_tickets_2024 %>%
  group_by(created_at_month) %>%
  summarise(monthly_tickets_created = n())

# Visualize monthly ticket volume
ggplot(monthly_summary, aes(x = created_at_month, y = monthly_tickets_created)) +
  geom_line(color = "#0073C2FF", size = 1) +  # Line for trend
  geom_point(color = "#E69F00", size = 2) +  # Points on the line for emphasis
  labs(title = "Monthly Ticket Creation Trend in 2024",
       x = "Month of the Year",
       y = "Number of Tickets Created") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 12, 1)) +  # Customize x-axis to show all weeks
  theme(axis.text.x = element_text(hjust = 1))  # Rotate x-axis labels