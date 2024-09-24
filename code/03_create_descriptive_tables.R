last_week_tickets <- clean_df %>%
  filter(created_at_date >= Sys.Date() - 7)

last_week_tickets

last_30_days_tickets <- clean_df %>%
  filter(created_at_date >= Sys.Date() - 30)

last_30_days_tickets

# Count of assigned tickets by assignee with margins
table(last_week_tickets$assignee, last_week_tickets$created_at_date) %>% addmargins()


# 2024 tickets
tickets_2024 <- clean_df %>%
  filter(created_at_date >= "2024-01-01")

# Create a summary of tickets created by month
tickets_created <- tickets_2024 %>%
  group_by(created_at_month) %>%
  summarise(tickets_created = n()) %>%
  ungroup()

# Create a summary of tickets closed by month
tickets_closed <- tickets_2024 %>%
  group_by(closed_at_month) %>%
  summarise(tickets_closed = n()) %>%
  ungroup()

# Combine the two summaries into a single table
ticket_summary_by_month <- full_join(tickets_created, tickets_closed, by = c("created_at_month" = "closed_at_month"))

# View the result
print(ticket_summary_by_month)
