# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming clean_df is already available
tickets_data <- clean_df %>%
  filter(created_at_date >= "2024-10-31")

# 1. Count tickets created by day
created_counts <- tickets_data %>%
  group_by(created_at_date) %>%
  summarise(tickets_created = n(), .groups = 'drop')

# 2. Count tickets resolved by day
resolved_counts <- tickets_data %>%
  group_by(closed_at_date) %>%
  summarise(tickets_resolved = n(), .groups = 'drop')

# 3. Merge created_counts and resolved_counts using merge()
result <- merge(created_counts, resolved_counts, 
                by.x = "created_at_date", by.y = "closed_at_date", 
                all = TRUE)

# 4. Replace NA values with 0 in the created and resolved counts
result$tickets_created[is.na(result$tickets_created)] <- 0
result$tickets_resolved[is.na(result$tickets_resolved)] <- 0

# 5. Calculate cumulative created tickets
result <- result %>%
  arrange(created_at_date) %>%
  mutate(cumulative_created = cumsum(tickets_created))

# 6. Calculate cumulative resolved tickets
result <- result %>%
  mutate(cumulative_resolved = cumsum(tickets_resolved))

# 7. Calculate the difference between created and resolved tickets
result <- result %>%
  mutate(ticket_diff = cumulative_created - cumulative_resolved)

# 8. Plot the three lines
ggplot(result, aes(x = created_at_date)) +
  # Blue line for cumulative created tickets
  geom_line(aes(y = cumulative_created), color = "blue", size = 1.5) +
  
  # Green line for cumulative resolved tickets
  geom_line(aes(y = cumulative_resolved), color = "green", size = 1.5) +
  
  # Red dashed line for the difference between created and resolved tickets
  geom_line(aes(y = ticket_diff), color = "red", linetype = "dashed", size = 1) +
  
  # Customize title and axes
  labs(title = "Cumulative Created vs Resolved Tickets and Difference", 
       x = "Date", 
       y = "Ticket Count") +
  
  # Add a legend to describe the lines
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Cumulative Created", "Cumulative Resolved", "Difference")) +
  
  # Minimal theme for clarity
  theme_minimal() +
  
  # Optionally adjust axis and legend position
  theme(legend.title = element_blank(),
        legend.position = "bottom")
