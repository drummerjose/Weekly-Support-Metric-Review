# Build star schema tables

# #--------------------Create the Fact Table-----------------
fact_support_ticket <- clean_df

# #--------------------Create the Dimension Tables-----------------

# * Orgcode Dimension table-----------------
dim_orgcode <- clean_df %>%
  select(orgcode) %>%
  distinct() %>%
  arrange(orgcode) %>%
  mutate(orgcode_id = row_number())

# Update fact table with the org code data
fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_orgcode, by = "orgcode") %>%
  select(-orgcode)

# * Support Area Dimension table-----------------
fact_support_ticket$area <- str_to_title(fact_support_ticket$area) # Format text as title

dim_support_area <- fact_support_ticket %>%
  select(area) %>%
  distinct() %>%
  mutate(area_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_support_area, by = "area") %>%
  select(-area)

# * Ticket Resolution Dimension table-----------------
fact_support_ticket$resolution <- str_to_title(fact_support_ticket$resolution) # Format text as title

dim_ticket_resolution <- fact_support_ticket %>%
  select(resolution) %>%
  distinct() %>%
  mutate(resolution_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_ticket_resolution, by = "resolution") %>%
  select(-resolution)

# * Ticket Priority Dimension table-----------------
fact_support_ticket$priority <- str_to_title(fact_support_ticket$priority) # Format text as title

dim_ticket_priority <- fact_support_ticket %>%
  select(priority) %>%
  distinct() %>%
  mutate(priority_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_ticket_priority, by = "priority") %>%
  select(-priority)

# * Ticket Issue Type Dimension table-----------------
fact_support_ticket$issue_type <- str_to_title(fact_support_ticket$issue_type) # Format text as title

dim_ticket_issue <- fact_support_ticket %>%
  select(issue_type) %>%
  distinct() %>%
  mutate(issue_type_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_ticket_issue, by = "issue_type") %>%
  select(-issue_type)

# * Ticket Rating Dimension table-----------------
fact_support_ticket$ratings <- str_to_title(fact_support_ticket$ratings) # Format text as title

dim_ticket_rating <- fact_support_ticket %>%
  select(ratings) %>%
  distinct() %>%
  mutate(ratings_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_ticket_rating, by = "ratings") %>%
  select(-ratings)

# * Ticket Status Dimension table-----------------
fact_support_ticket$status <- str_to_title(fact_support_ticket$status) # Format text as title

dim_ticket_status <- fact_support_ticket %>%
  select(status) %>%
  distinct() %>%
  mutate(status_id = row_number()) # create dim table

fact_support_ticket <- fact_support_ticket %>%
  left_join(dim_ticket_status, by = "status") %>%
  select(-status)