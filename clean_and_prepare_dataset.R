# Reading the flat file with support data
ticket_data <- read.csv("data/raw_2022-01-01_2023-12-31_Helpscout_Ticket_Data.csv", stringsAsFactors = FALSE)

# Remove blank org codes field
## Remove Notification Rule Error Tickets
## MISSING ADD IF CONDITION AT BEGINNING. IF ORGCODE IS MISSING THEN FILTER
df %>% filter(Subject == 'Notification Rule Has Failed') #Filter by Subject column

### FILTER ONLY lightningstepsupport MAILBOX
### FILTER OUT CUSTOMER EMAIL ADDRESSES flash@lightningstep.com
