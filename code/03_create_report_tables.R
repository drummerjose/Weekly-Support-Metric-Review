# Prepare dataset -------------------
ticket_2024 <- clean_df %>% 
  filter(closed_at_year == 2024,
         support_team_closed_by == "Financial")

last_7_days_tickets <- clean_df %>%
  filter(created_at_date >= Sys.Date() - 7)

# tables -------------------
# * T1: Last 7 days received financial tickets -------------------
last_7_days_created_financial_tickets <- clean_df %>%
  filter(created_at_date >= Sys.Date() - 7,
         support_team_closed_by == "Financial")

t1 <- table(last_7_days_created_financial_tickets$assignee, last_7_days_created_financial_tickets$created_at_date)

t1

addmargins(t1)

# * T2: Last 7 days closed financial tickets -------------------
last_7_days_closed_financial_tickets <- clean_df %>%
  filter(closed_at_date >= Sys.Date() - 7,
         support_team_closed_by == "Financial",
         status == "closed")

t2 <- table(last_7_days_closed_financial_tickets$assignee, last_7_days_closed_financial_tickets$closed_at_date)

t2

addmargins(t2)

# * T3: Last 7 days closed same day financial tickets -------------------
last_7_days_closed_same_day_financial_tickets <- clean_df %>%
  filter(created_at_date >= Sys.Date() - 7,
         support_team_closed_by == "Financial",
         status == "closed",
         current_status != "Awaiting Development",
         created_at_date == closed_at_date)

t3 <- table(last_7_days_closed_same_day_financial_tickets$assignee, last_7_days_closed_same_day_financial_tickets$closed_at_date)

t3

addmargins(t3)

last_7_days_closed_same_day_financial_tickets_percent <- t3

last_7_days_closed_same_day_financial_tickets_percent <- t3[ ,-1] / t1[ , -1]
