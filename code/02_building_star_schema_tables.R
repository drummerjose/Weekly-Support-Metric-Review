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
