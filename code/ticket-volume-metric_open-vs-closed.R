# Financial Ticket Volume

# Create a summary of tickets created by month
financial_tickets_created <- financial_tickets %>%
  group_by(created_at_year_month) %>%
  summarise(tickets_created = n()) %>%
  ungroup()

# Create a summary of tickets closed by month
financial_tickets_closed <- financial_tickets %>%
  filter(status == "closed") %>%
  group_by(closed_at_year_month) %>%
  summarise(tickets_closed = n()) %>%
  ungroup()

# Combine the two summaries into a single table
financial_ticket_summary_by_month <- full_join(financial_tickets_created, financial_tickets_closed, by = c("created_at_year_month" = "closed_at_year_month")) %>% filter(!is.na(created_at_year_month))

# View the result
print(financial_ticket_summary_by_month)


# Plot the table --------------------
# Convert to long format for ggplot
financial_ticket_summary_by_month_long <- financial_ticket_summary_by_month %>%
  pivot_longer(cols = c(tickets_created, tickets_closed), 
               names_to = "status", 
               values_to = "total_tickets")

# Create the plot
ggplot(financial_ticket_summary_by_month_long, aes(x = created_at_year_month, y = total_tickets, fill = status)) +
  geom_col(position = "dodge") +  # Group bars side by side
  labs(title = "Monthly Tickets Created vs. Closed",
       x = "Month",
       y = "Number of Tickets",
       fill = "Ticket Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
