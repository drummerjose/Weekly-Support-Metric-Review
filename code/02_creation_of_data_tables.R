# CREATION OF DATA TABLES FOR VISUALIZATIONS

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

tickets_data <- clean_df %>% filter(created_at_date >= "2024-10-31")

# 1. Count tickets created by day
created_counts <- tickets_data %>%
  group_by(created_at_date) %>%
  summarise(tickets_created = n(), .groups = 'drop')

# 2. Count tickets resolved by day
resolved_counts <- tickets_data %>%
  group_by(closed_at_date) %>%
  summarise(tickets_resolved = n(), .groups = 'drop')

# 3. Merge created_counts and resolved_counts using merge()
result <- merge(created_counts, resolved_counts, by.x = "created_at_date", by.y = "closed_at_date", all = TRUE)

# 4. Replace NA values with 0 in the created and resolved counts
result$tickets_created[is.na(result$tickets_created)] <- 0
result$tickets_resolved[is.na(result$tickets_resolved)] <- 0

# # 5. Reshape data to long format for easier plotting (created vs. resolved)
# result_long <- result %>%
#   pivot_longer(cols = c(tickets_created, tickets_resolved),
#                names_to = "ticket_status",
#                values_to = "ticket_count")

# 5. Calculate cumulative created tickets
result <- result %>%
  arrange(created_at_date) %>%
  mutate(cumulative_created = cumsum(tickets_created))

# 6. Calculate the difference between created and resolved tickets
result <- result %>%
  mutate(ticket_diff = tickets_created - tickets_resolved)

# 7. Plot the line graph with the area above or below the line for the difference
ggplot(result, aes(x = created_at_date)) +
  # Plot the line for cumulative created tickets
  geom_line(aes(y = cumulative_created), color = "blue", size = 1.5) +
  
  # Fill the area above the line when ticket_diff is positive (created > resolved)
  geom_area(aes(y = cumulative_created + ticket_diff, fill = ticket_diff >= 0), 
            alpha = 0.3, show.legend = TRUE) +
  
  # Fill the area below the line when ticket_diff is negative (resolved > created)
  geom_area(aes(y = cumulative_created + ticket_diff, fill = ticket_diff < 0), 
            alpha = 0.3, show.legend = TRUE) +
  
  # Customize title and axes
  labs(title = "Cumulative Created vs Resolved Tickets", 
       x = "Date", 
       y = "Ticket Count") +
  
  # Define colors for the areas (green for positive, red for negative)
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), 
                    labels = c("Created > Resolved", "Resolved > Created")) +
  
  # Minimal theme for clarity
  theme_minimal() +
  
  # Optionally adjust axis and legend position
  theme(legend.title = element_blank(),
        legend.position = "bottom")
  