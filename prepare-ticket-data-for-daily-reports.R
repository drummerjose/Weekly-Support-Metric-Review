# Install and load packages
library('dplyr')
library('stringr')
library('tidyr')
library('lubridate')

# Set workspace
setwd("C:/Users/Sergio/Downloads/R Projects/Support Dashboard v1")

daily_tickets <- read.csv("data/2024-01-23 2024-01-24 All Channels Report.csv", stringsAsFactors = FALSE)

daily_tickets <- daily_tickets %>%
  filter(Mailbox == 'lightningstepsupport') # Keep only LS support tickets

daily_tickets <- daily_tickets %>%
  select(-contains("zen")) # Exclude columns containing the word 'zen'

daily_tickets <- daily_tickets %>%
  rename(
    id = ID,
    conversation_id = Conversation..,
    conversation_url = Conversation.URL,
    type = Type,
    assignee = Assignee,
    status = Status,
    mailbox = Mailbox,
    customer_name = Customer.name,
    customer_email = Customer.email.addresses,
    customer_phone = Customer.phone.numbers,
    customer_profile_url = Customer.profile.URL,
    thread.count = Thread.count,
    subject = Subject,
    created_at = Created.at,
    created_by = Created.by,
    last_modified = Last.modified,
    closed_at = Closed.at,
    closed_by = Closed.by,
    tags = Tags,
    saved_replies = Saved.replies,
    workflows = Workflows,
    cc = CC,
    bcc = BCC,
    first_response_time_seconds = First.response.time..seconds.,
    first_response_time_office_hours_seconds = First.response.time..office.hours..seconds.,
    avg_response_time_seconds = Avg..response.time..seconds.,
    avg_response_time_seconds_office_hours_seconds = Avg..response.time..office.hours..seconds.,
    replies_sent = Replies.sent,
    handle_time_seconds = Handle.time..seconds.,
    resolved = Resolved,
    resolved_on_first_reply = Resolved.on.first.reply,
    resolution_time_seconds = Resolution.time..seconds.,
    resolution_time_office_hours_seconds = Resolution.time..office.hours..seconds.,
    ratings = Ratings,
    rating_comments = Rating.comments,
    area = Area.lightningstepsupport,
    current_status = Current.Status.lightningstepsupport,
    due_date = Due.Date.lightningstepsupport,
    issue_type = Issue.Type.lightningstepsupport,
    orgcode = Orgcode.lightningstepsupport,
    origin = Origin.lightningstepsupport,
    other = Other.lightningstepsupport,
    priority = Priority.lightningstepsupport,
    resolution = Resolution.lightningstepsupport
  )

daily_tickets <- daily_tickets %>%
  filter(closed_by != 'Lightning Step Support') # Filter out tickets closed by Lightning Step Support

daily_tickets <- daily_tickets %>%
  select(-mailbox, -bcc) # remove mailbox and bcc columns

# Convert 'y' and 'n' to 1 and 0
daily_tickets <- daily_tickets %>%
  mutate(rating_comments = ifelse(rating_comments == 'y', 1, 0))

daily_tickets <- daily_tickets %>%
  mutate(resolved = ifelse(resolved == 'y', 1, 0))

daily_tickets <- daily_tickets %>%
  mutate(resolved_on_first_reply = ifelse(resolved_on_first_reply == 'y', 1, 0))

# * Convert Date columns to the correct format---------------------------------------
# Define the time zone (replace with your desired time zone)
timezone <- "America/Chicago"

# Convert the string to a POSIXct datetime object
daily_tickets$created_at <- daily_tickets$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

daily_tickets$last_modified <- daily_tickets$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

daily_tickets$closed_at <- daily_tickets$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

write.csv(daily_tickets, "C:/Users/Sergio/Downloads/Daily Support/data/2024-01-23_Daily_Tickets.csv", row.names = TRUE)
