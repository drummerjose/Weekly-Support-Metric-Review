# Financial Inflow pattern

# Step 1: Create columns for day of week and hour
ticket_inflow_patterns <- financial_tickets

# Step 2: Summarize the number of tickets by day of week and hour
ticket_inflow_summary <- ticket_inflow_patterns %>%
  group_by(created_at_weekday, created_at_hour) %>%
  summarise(total_tickets = n()) %>%
  ungroup()

# View the summarized inflow pattern data
print(ticket_inflow_summary)

# Step 3: Visualize the ticket inflow patterns by day of week and hour
ggplot(ticket_inflow_summary, aes(x = created_at_hour, y = total_tickets, fill = created_at_weekday)) +
  geom_col(position = "dodge") +
  labs(title = "Ticket Inflow Patterns by Day of Week and Time of Day",
       x = "Hour of Day",
       y = "Total Tickets") +
  theme_minimal()
