# Ctrl + Shift + R to Create a Section

# Install and load packages
library('dplyr')
library('stringr')
library('tidyr')
library('lubridate')

# Set workspace
setwd("C:/Users/Sergio/Downloads/R Projects/Support Dashboard v1")

# Preprocess Date to only data that will be used in Analysis ----------------------------------
# Reading the flat file with support data
tickets_fact <- read.csv("data/raw_2022-01-01_2023-12-31_Helpscout_Ticket_Data.csv", stringsAsFactors = FALSE)

tickets_fact <- tickets_fact %>%
  filter(Mailbox == 'lightningstepsupport') # Keep only LS support tickets
  
tickets_fact <- tickets_fact %>%
  select(-contains("zen")) # Exclude columns containing the word 'zen'

tickets_fact <- tickets_fact %>%
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

tickets_fact <- tickets_fact %>%
  filter(closed_by != 'Lightning Step Support') # Filter out tickets closed by Lightning Step Support

tickets_fact <- tickets_fact %>%
  select(-mailbox, -bcc) # remove mailbox and bcc columns

# Convert 'y' and 'n' to 1 and 0
tickets_fact <- tickets_fact %>%
  mutate(rating_comments = ifelse(rating_comments == 'y', 1, 0))

tickets_fact <- tickets_fact %>%
  mutate(resolved = ifelse(resolved == 'y', 1, 0))

tickets_fact <- tickets_fact %>%
  mutate(resolved_on_first_reply = ifelse(resolved_on_first_reply == 'y', 1, 0))

# * Convert Date columns to the correct format---------------------------------------
# Define the time zone (replace with your desired time zone)
timezone <- "America/Chicago"

# Convert the string to a POSIXct datetime object
tickets_fact$created_at <- tickets_fact$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

tickets_fact$last_modified <- tickets_fact$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

tickets_fact$closed_at <- tickets_fact$created_at %>% 
  ymd_hms(tz = "UTC") %>%
  with_tz(tzone = "America/Bogota") %>%
  floor_date(unit = "hour") %>%
  format(format = "%Y-%m-%d %H:00:00") %>%
  as.POSIXct(format = "%Y-%m-%d %H:%M:%S")

# Dimension Table 1: Org Dimension -------------------------------------------------------------
# * Convert all values to lowercase --------------
tickets_fact$orgcode <- tolower(tickets_fact$orgcode)
# * Mark excluded orgcodes that aren't LS Support tickets  -------------------------------------------------------------
tickets_fact <- tickets_fact %>%
  mutate(orgcode = case_when(
    orgcode %in% c("?", 
                                        "??",
                                        "all",
                                        "all servers",
                                        "ctc",
                                        "ctc.zencharts.com",
                                        "ctm", #zen
                                        "dominion",
                                        "donovan",
                                        "ds", #dosespot
                                        "dup",
                                        "duplicate",
                                        "eedc",
                                        "error",
                                        "etc",
                                        "globalpay",
                                        "gmail",
                                        "ignore",
                                        "infinite", #ls server not found in list
                                        "internal",
                                        "lab", #dominion
                                        "labs", #dominion
                                        "lovett",
                                        "millenium", #labs
                                        "millennium", #labs
                                        "mjr", #zen
                                        "mycp", #zen
                                        "n/a",
                                        "na",
                                        "nlw", #zen incorrect code
                                        "none", #non LS Users
                                        "other", #request from old servers
                                        "ps", #paysimple
                                        "qualitox laboratories", #lab
                                        "real deal", #zen
                                        "scientific lab", #lab
                                        "source", #zen
                                        "spam", #spam
                                        "stewardsofrecovery.zencharts.com", #zen
                                        "stonegate", #zen
                                        "tco - zen", #zen
                                        "unknown", #non LS user
                                        "waystar", 
                                        "zen", #zen
                                        "zencharts") ~ "99999",
    TRUE ~ orgcode  # Keep other values unchanged
  ))


# * Identify all org code variations ----------------------------------------

# * * Define replacement vectors -----------------------------------------
replacement_orgcodes <- c("achievebhs" = "achievebh",
                          "asc" = "acs",
                          "adorc" = "ado",
                          "al" = "alpas",
                          "alpha crc hr njbh sana rrt" = "alpha",
                          "aplha" = "alpha",
                          "altior rcm" = "altiorrcm",
                          "altior.lightningrcm.com/" = "altiorrcm",
                          "alitum" = "altium",
                          "amanaha" = "amanah",
                          "amaqtus" = "amatus",
                          "amati’s" = "amatus",
                          "apex" = "apexrecovery",
                          "anchor" = "apw",
                          "awp" = "apw",
                          "arow" = "arrow",
                          "ascent.lightningrcm.com" = "ascent",
                          "ascentrcm" = "ascent",
                          "aspen" = "aspentc",
                          "ava" = "avarcm",
                          "all ava" = "avarcm",
                          "avabilling" = "avarcm",
                          "ava.lightningrcm" = "avarcm",
                          "ava sites" = "avarcm",
                          "ava rcm" = "avarcm",
                          "ava rcm - momentum" = "avarcm",
                          "medusin" = "avarcm",
                          "oakvine rcm ava" = "avarcm",
                          "rcm" = "avarcm",
                          "rc" = "avarcm",
                          "awaken acs" = "awaken",
                          "bbh" = "bah",
                          "buyareaps" = "bayareaps",
                          "bsc" = "bcs",
                          "beond" = "beod",
                          "brf" = "bfr",
                          "bhp" = "bhpmgmt",
                          "bhpmgmt / frc" = "bhpmgmt",
                          "boulder" = "bih",
                          "bluehero" = "blueheron",
                          "brazos" = "brazosrecovery",
                          "brazos recovery" = "brazosrecovery",
                          "bcr" = "brc",
                          "blue tiger" = "btr",
                          "bluetiger" = "btr",
                          "btrr" = "btr",
                          "buen" = "bv",
                          "buenavista" = "bv",
                          "buena vista recovery" = "bv",
                          "bhc" = "cbh",
                          "ccbh" = "ccbhp",
                          "cchbp" = "ccbhp",
                          "ccpbh" = "ccbhp",
                          "ccphp" = "ccbhp",
                          "ctrl" = "ccbhp",
                          "ctrlcare" = "ccbhp",
                          "casacolina" = "cct",
                          "cdt" = "cct",
                          "cp" = "cpcc",
                          "cpr" = "cpcc",
                          "compasion" = "crc",
                          "compassion" = "crc",
                          "dennovo" = "denovo",
                          "ect" = "ecr",
                          "ehg, empowered, njbh" = "ehg",
                          "elevate" = "elev8",
                          "empower" = "empowered",
                          "empo" = "empowered",
                          "empowerrecovery" = "empowered",
                          "empowored" = "empowered",
                          "exists" = "exist",
                          "flhv" = "fhlv",
                          "fcr" = "frc",
                          "frcsf" = "frc",
                          "fredom" = "frc",
                          "freedom" = "frc",
                          "fsfs" = "fsf",
                          "grace" = "gc",
                          "grgcce" = "gc",
                          "aim" = "ghr",
                          "hgr" = "ghr",
                          "fhr" = "ghr",
                          "glendora" = "glendorarc",
                          "hamm" = "hammocks",
                          "hammocks, tribe" = "hammocks",
                          "hammoks" = "hammocks",
                          "hill" = "hr", 
                          "hl" = "hr",
                          "hcr" = "hrc",
                          "hdc" = "hrc",
                          "hrc, cpcc, pc, ncwc, njbh, bcs" = "hrc",
                          "harbor" = "hwr",
                          "iarc" = "ia",
                          "intoaction" = "ia",
                          "ai" = "ia",
                          "bcr" = "ilc",
                          "integrative" = "ilc",
                          "llc" = "ilc",
                          "jhr" = "jrh",
                          "kpr,ircv" = "kpr",
                          "ktr" = "kpr",
                          "lg" = "lb",
                          "longbranch" = "lb",
                          "lh, moon" = "lh", 
                          "lht" = "lh",
                          "lighthouse" = "lh",
                          "ls" = "lightning",
                          "admin" = "lightning",
                          "kirk" = "lightning",
                          "ltr" = "lrt",
                          "lake wellness" = "lw",
                          "mrc" = "mcr",
                          "multi" = "mcr",
                          "mtc" = "mcr",
                          "multiple" = "mcr",
                          "midland" = "midlandjd",
                          "momenta" = "mo",
                          "mfv" = "mvf",
                          "newbeginnings" = "nb",
                          "ndwf", "ncfw" = "ndfw",
                          "ndts", "ntdc" = "ndtc",
                          "ntdc" = "ndtc",
                          "new day" = "newday",
                          "nhorizon" = "nhorizons",
                          "nh - rcm" = "nhtc",
                          "njbh ghr" = "njbh",
                          "nola, rr" = "nola",
                          "new port" = "np",
                          "newpo" = "np",
                          "newport" = "np",
                          "nre port" = "np",
                          "mp" = "np",
                          "np test" = "nptest",
                          "newroads" = "nr",
                          "nvhv" = "nvh",
                          "mwr" = "nwr",
                          "nwrc" = "nwr",
                          "mycfl" = "nycfl",
                          "nycl" = "nycfl",
                          "nyfcl" = "nycfl",
                          "outcomes" = "odc",
                          "onsi" = "onsite",
                          "peace" = "pc",
                          "peace club" = "pc",
                          "peaceclub" = "pc",
                          "embrace" = "pmc",
                          "pcr" = "prc",
                          "positive" = "prc",
                          "pr" = "prc",
                          "pro" = "prc",
                          "praeaum" = "praesum",
                          "preasum" = "praesum",
                          "praseum" = "praesum",
                          "renewed hope rcm" = "rhr",
                          "rhr rcm" = "rhr",
                          "redriver" = "rr",
                          "rcr" = "rrc",
                          "resolutionrecovery" = "rrc",
                          "riversource" = "rs",
                          "riseup" = "ru", 
                          "ur" = "ru",
                          "sarc" = "sac",
                          "thesafefundation" = "safe",
                          "sagwe" = "sage",
                          "sbr" = "sbrc",
                          "sge" = "sage",
                          "sim" = "samh",
                          "simco" = "samh",
                          "simcoe" = "samh",
                          "sana, njbh" = "sana",
                          "see, mcr" = "see",
                          "see, mcr, sto, mhrc, nwr" = "see",
                          "site4" = "site14",
                          "skyway" = "skywaybh",
                          "serenity" = "slr",
                          "sor.zencharts.com" = "sor",
                          "srs, lw" = "srs",
                          "stage" = "staging",
                          "stating" = "staging",
                          "steward" = "stewards",
                          "sto, see, mcr, mhrc, nwr" = "sto",
                          "ttbh" = "tbh",
                          "tf" = "tfc",
                          "fbc" = "tfc",
                          "tbc" = "tfc",
                          "thrivercm" = "thrive",
                          "tmh" = "tmc",
                          "trp" = "tpr",
                          "tp" = "tpr",
                          "tprc" = "tpr",
                          "trauma" = "traumari",
                          "trib" = "tribe",
                          "tribr" = "tribe",
                          "rrbh" = "ttbh",
                          "vct" = "vtc",
                          "warrior" = "warriors",
                          "Warriors RCM" = "warriors",
                          "warrriors" = "warriors",
                          "warrriors rcm" = "warriors",
                          "westbridge" = "wb",
                          "wasatch" = "wct",
                          "wtc" = "wct",
                          "wellness" = "wellnesscenter",
                          "wellnessrecovery" = "wellnesscenter",
                          "wkl" = "wlk",
                          "wl" = "wlk",
                          "wlc" = "wlk",
                          "woodl ake" = "wlk",
                          "woodlak" = "wlk",
                          "woodlake" = "wlk",
                          "woodlands" = "wrc",
                          "wwrr" = "wwr",
                          "yellowwood" = "yellowwoodrecovery",
                          "yellowood" = "yellowwoodrecovery",
                          "zia" = "zrc",
                          "zcr" = "zrc")


# * * Apply replacement vector to org codes column ---------------------------------------------

tickets_fact <- tickets_fact %>%
  mutate(orgcode = case_when(
    orgcode %in% names(replacement_orgcodes) ~ replacement_orgcodes[orgcode],
    TRUE ~ orgcode  # Keep other values unchanged
  ))

# * Remaining rows to correct -----------------------------------------------
# Count number of tickets by org code
# View(tickets_fact %>%
#   group_by(orgcode) %>%
#   summarise(count = n()))
# Dimension Table 1: Orgcodes -----------------
org_dim <- tickets_fact %>%
  select(orgcode) %>%
  distinct() %>%
  mutate(orgcode_id = row_number())

tickets_fact <- tickets_fact %>%
  left_join(org_dim, by = "orgcode") %>%
  select(-orgcode)

# Dimension Table 2: Area  ------------------------
tickets_fact$area <- str_to_title(tickets_fact$area) # Format text as title

area_dim <- tickets_fact %>%
  select(area) %>%
  distinct() %>%
  mutate(area_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(area_dim, by = "area") %>%
  select(-area)


# Dimension Table 3: Resolution -------------------------------------
tickets_fact$resolution <- str_to_title(tickets_fact$resolution) # Format text as title

resolution_dim <- tickets_fact %>%
  select(resolution) %>%
  distinct() %>%
  mutate(resolution_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(resolution_dim, by = "resolution") %>%
  select(-resolution)

# Dimension Table 4: Priority -------------------------------------
tickets_fact$priority <- str_to_title(tickets_fact$priority) # Format text as title

priority_dim <- tickets_fact %>%
  select(priority) %>%
  distinct() %>%
  mutate(priority_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(priority_dim, by = "priority") %>%
  select(-priority)

# Dimension Table 5: Issue Type -------------------------------------
tickets_fact$issue_type <- str_to_title(tickets_fact$issue_type) # Format text as title

issue_type_dim <- tickets_fact %>%
  select(issue_type) %>%
  distinct() %>%
  mutate(issue_type_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(issue_type_dim, by = "issue_type") %>%
  select(-issue_type)

# Dimension Table 6: Rating  -------------------------------------
tickets_fact$ratings <- str_to_title(tickets_fact$ratings) # Format text as title

ratings_dim <- tickets_fact %>%
  select(ratings) %>%
  distinct() %>%
  mutate(ratings_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(ratings_dim, by = "ratings") %>%
  select(-ratings)

# Dimension Table 7: Status  -------------------------------------
tickets_fact$status <- str_to_title(tickets_fact$status) # Format text as title

status_dim <- tickets_fact %>%
  select(status) %>%
  distinct() %>%
  mutate(status_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(status_dim, by = "status") %>%
  select(-status)

# Dimension Table 8: Ticket Attributes  -------------------------------------
# Format text as title
tickets_fact$type <- str_to_title(tickets_fact$type)

ticket_attributes_dim <- tickets_fact %>% # create dim table
  select(conversation_id, conversation_url, type, assignee, subject, tags, saved_replies, workflows, cc, current_status, due_date, origin, other) %>%
  rename(a_conversation_id = conversation_id, 
         a_conversation_url = conversation_url,
         a_type = type, 
         a_assignee = assignee,
         a_subject = subject, 
         a_tags = tags,
         a_saved_replies = saved_replies,
         a_workflows = workflows,
         a_cc = cc,
         a_current_status = current_status,
         a_due_date = due_date,
         a_origin = origin,
         a_other = other)

tickets_fact <- tickets_fact %>%
  left_join(ticket_attributes_dim, by = c("conversation_id" = 'a_conversation_id')) %>%
  select(-conversation_url, -type, -assignee, -subject, -tags, -saved_replies, -workflows, -cc, -current_status, -due_date, -origin, -other, 
         -a_conversation_url, -a_type, -a_assignee, -a_subject, -a_tags, -a_saved_replies, -a_workflows, -a_cc, -a_current_status, -a_due_date, -a_origin, -a_other)

# Dimension Table 9: Customer  -------------------------------------
customer_dim <- tickets_fact %>%
  select(customer_name, customer_email, customer_phone, customer_profile_url) %>%
  distinct(customer_email) %>%
  mutate(customer_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(customer_dim, by = "customer_email") %>%
  select(-customer_name, -customer_email, -customer_phone, -customer_profile_url)

# Dimension Table 10: Created By  -------------------------------------
created_by_dim <- tickets_fact %>%
  select(created_by) %>%
  distinct() %>%
  mutate(created_by_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(created_by_dim, by = "created_by") %>%
  select(-created_by)

# Dimension Table 11: Closed By  -------------------------------------
closed_by_dim <- tickets_fact %>%
  select(closed_by) %>%
  distinct() %>%
  mutate(closed_by_id = row_number()) # create dim table

tickets_fact <- tickets_fact %>%
  left_join(closed_by_dim, by = "closed_by") %>%
  select(-closed_by)

# Dimension Table 12: Date Table  ----------------------------------------------------------

# Create a sequence of dates by hour from 2021-01-01 00:00:00 to 2021-01-02 00:00:00 in the specified time zone
date_sequence <- seq(
  with_tz(as.POSIXct("2021-01-01 00:00:00", tz = timezone), tzone = timezone),
  with_tz(as.POSIXct("2025-12-31 11:00:00", tz = timezone), tzone = timezone),
  by = "hour"
)

# Create a data frame with the date sequence
date_dim <- data.frame(DateTime = date_sequence)

# Extract individual components of the date and time
date_dim <- date_dim %>%
  mutate(
    Year = year(DateTime),
    Month = month(DateTime),
    Day = day(DateTime),
    Hour = hour(DateTime),
    Weekday = weekdays(DateTime),
    Quarter = quarter(DateTime),
    YearMonth = format(DateTime, "%Y-%m"),
    YearQuarter = paste0(Year, " Q", Quarter),
    TimeZone = timezone
  )

# Create Visualization -----------------------------------------------------
# Install and load required packages
# library(ggplot2)
# 
# # Merge fact and dimension data frames
# merged_df <- merge(tickets_fact, closed_by_dim, by = "closed_by_id")
# 
# # Create a bar plot using ggplot2
# ggplot(merged_df, aes(x = closed_by, y = resolution_time_seconds, fill = closed_by)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Bar Plot of Fact Data by Category", x = "Category", y = "Value") +
#   theme_minimal()


# Create Function to improve code ---------------------------------
# Function to create a dimension table
# create_dim_table <- function(data, column) {
#   column <- enquo(column)
#   data %>%
#     select(!!column) %>%
#     distinct() %>%
#     mutate(!!paste0(quo_name(column), "_id") := row_number())
# }
# 
# # Function to left join and select columns
# left_join_and_select <- function(data, dim_table, column) {
#   column <- enquo(column)
#   data %>%
#     left_join(dim_table, by = !!column) %>%
#     select(-!!column)
# }
# 
# # Issue Type Dimension Table
# tickets_fact$issue_type <- str_to_title(tickets_fact$issue_type)
# issue_type_dim <- create_dim_table(tickets_fact, issue_type)
# tickets_fact <- left_join_and_select(tickets_fact, issue_type_dim, issue_type)
# 
# # Rating Dimension Table
# tickets_fact$ratings <- str_to_title(tickets_fact$ratings)
# ratings_dim <- create_dim_table(tickets_fact, ratings)
# tickets_fact <- left_join_and_select(tickets_fact, ratings_dim, ratings)
# 
# # Status Dimension Table
# tickets_fact$status <- str_to_title(tickets_fact$status)
# status_dim <- create_dim_table(tickets_fact, status)
# tickets_fact <- left_join_and_select(tickets_fact, status_dim, status)