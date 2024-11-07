# Script: 01_data_preprocessing.R
# Purpose: Filter out unnecessary records and columns and add new data points. Then it can be exported to CSV.
# Author: Jose Teran
# Date: 2024-09-17
# Description: This script loads raw data, cleans it, and prepares it for analysis.
# Usage: Run this script before any analysis.

# Load necessary libraries
library(tidyverse)
library(janitor)

# Set working directory (if not using R projects)
# setwd("C:/Users/Sergio/Downloads/R Projects/Weekly-Support-Metric-Review/") # LS Laptop

setwd("C:/Users/joe/Desktop/Weekly Support Metric Review/") # Dell Laptop


# Read raw data
df <- read.csv("./data/raw/2024-01-01 2024-09-22 All Channels Report.csv")

#--------------------Initial Data Exploration-----------------
# str(df)
# summary(df)
# head(df)
# colnames(df)

#--------------------Data Filtering-----------------
# Update column names and filter records by mailbox and closed_by
clean_df <- df %>% 
  janitor::clean_names() %>%
  filter(mailbox == 'lightningstepsupport' & closed_by != 'Lightning Step Support') %>%
  filter(assignee %in% c("Enrique Carreno", "Jose Teran", "David Benalcazar", "Nicolas Jaramillo","Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach"))

# Remove Zen and Form columns, and columns: mailbox, bcc
clean_df <- clean_df %>%
  select(-matches("zen|forms_team"), -mailbox, -bcc)

# Rename colnames by removing "_lightningstepsupport" from all column names for easier reading
colnames(clean_df) <- gsub("_lightningstepsupport", "", colnames(clean_df))

#--------------------Data Transformation and Feature Engineering-----------------
# Convert data types
# created_at date columns
clean_df <- clean_df %>%
  mutate(
    created_at_datetime = ymd_hms(created_at),
    created_at_date = as.Date(created_at_datetime),
    created_at_year = year(created_at_date),
    created_at_quarter = quarter(created_at_date),
    created_at_month = month(created_at_date),
    created_at_month_name = month(created_at_date, label = TRUE),
    created_at_year_month = format(created_at_date, "%Y-%m"), # Create year-month column
    created_at_year_quarter = paste0(year(created_at_date), "-Q", quarter(created_at_date)),  # New line to create year-quarter column
    created_at_week = week(created_at_date),
    created_at_weekday = wday(created_at_date), # Sunday is 1 and Saturday 7
    created_at_weekday_name = weekdays(created_at_date),
    created_at_day = day(created_at_date),
    created_at_day_of_year = yday(created_at_date),  # Add day of year column
    created_at_isweekend = ifelse(created_at_date %in% c(1, 7), 1, 0),  # Check if the day is a weekend
    created_at_hour = hour(created_at_datetime)
  )

# remove created_at column from dataset
clean_df <- clean_df %>%
  select(-created_at)

# closed_at date columns
clean_df <- clean_df %>%
  mutate(
    closed_at_datetime = ymd_hms(closed_at),
    closed_at_date = as.Date(closed_at_datetime),
    closed_at_year = year(closed_at_date),
    closed_at_quarter = quarter(closed_at_date),
    closed_at_month = month(closed_at_date),
    closed_at_month_name = month(closed_at_date, label = TRUE),
    closed_at_year_month = format(closed_at_date, "%Y-%m"), # Create year-month column
    closed_at_year_quarter = paste0(year(closed_at_date), "-Q", quarter(closed_at_date)),  # New line to create year-quarter column
    closed_at_week = week(closed_at_date),
    closed_at_weekday = wday(closed_at_date), # Sunday is 1 and Saturday 7
    closed_at_weekday_name = weekdays(closed_at_date),
    closed_at_day = day(closed_at_date),
    closed_at_day_of_year = yday(closed_at_date),  # Add day of year column
    closed_at_isweekend = ifelse(closed_at_date %in% c(1, 7), 1, 0),  # Check if the day is a weekend
    closed_at_hour = hour(closed_at_datetime)
  )

# remove created_at column from dataset
clean_df <- clean_df %>%
  select(-closed_at)

# Enrich cleaned_df
clean_df <- clean_df %>%
  mutate(
    rating_comments = as.integer(rating_comments == 'y'),
    resolved = as.integer(resolved == 'y'),
    resolved_on_first_reply = as.integer(resolved_on_first_reply == 'y'),
    isjira = ifelse(str_detect(tags, "jira"), 1, 0), # Add isjira column
    support_team_assignee = case_when(
      assignee %in% c("Jose Teran", "David Benalcazar", "Nicolas Jaramillo") ~ "Financial",
      assignee %in% c("Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach") ~ "Clinical/Medical",
      TRUE ~ "Other"
    ),
    support_team_closed_by = case_when(
      closed_by %in% c("Jose Teran", "David Benalcazar", "Nicolas Jaramillo") ~ "Financial",
      closed_by %in% c("Daniel Freire", "Felipe Chiriboga", "Javier Galarza", "Thomas Hansen", "Kristofer Gerlach") ~ "Clinical/Medical",
      TRUE ~ "Other"
    )
  )

# #--------------------Standardize Values-----------------
# standaridize the org codes

# * * Org code values to lowercase
clean_df$orgcode <- tolower(clean_df$orgcode)

# * * Search for orgcode values with count occurrences equal to one
# one_count_occurrences_orgcode <- clean_df %>%
#   group_by(orgcode) %>%
#   summarise(count = n()) %>%
#   filter(count == 1)

# * * Create a replacement vector
replacement_orgcodes <- c("abh" = "abhaya",
                          "acheivebh" = "achievebh",
                          "achievebhs" = "achievebh",
                          "accs" = "acs",
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
                          "amatustraining" = "amatus",
                          "apex" = "apexrecovery",
                          "anchor" = "apw",
                          "awp" = "apw",
                          "arow" = "arrow",
                          "asa" = "asap",
                          "ascent.lightningrcm.com" = "ascent",
                          "ascentrcm" = "ascent",
                          "aspen" = "aspentc",
                          "astara midlandjd" = "astara",
                          "all ava" = "ava",
                          "avabilling" = "ava",
                          "ava.lightningrcm" = "ava",
                          "ava sites" = "ava",
                          "ava rcm" = "ava",
                          "avarcm" = "ava",
                          "ava rcm - momentum" = "ava",
                          "medusin" = "ava",
                          "oakvine rcm ava" = "ava",
                          "rcm" = "ava",
                          "rc" = "ava",
                          "awaken acs" = "awaken",
                          "bbh" = "bah",
                          "buyareaps" = "bayareaps",
                          "bsc" = "bcs",
                          "beond" = "beod",
                          "brc" = "bfr",
                          "brf" = "bfr",
                          "bhp" = "bhpmgmt",
                          "bhpmgmt / frc" = "bhpmgmt",
                          "boulder" = "bih",
                          "blu" = "blueheron",
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
                          "bvtest" = "bv",
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
                          "freedom" = "fhlv",
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
                          "ilctest" = "ilc",
                          "llc" = "ilc",
                          "icrv" = "ircv",
                          "icv" = "ircv",
                          "jhr" = "jrh",
                          "kpr,ircv" = "kpr",
                          "ktr" = "kpr",
                          "lg" = "lb",
                          "longbranch" = "lb",
                          "lh, moon" = "lh",
                          "lhc" = "lh",
                          "lht" = "lh",
                          "lhtest" = "lh",
                          "lighthouse" = "lh",
                          "admin" = "ls",
                          "clone" = "ls",
                          "demo" = "ls",
                          "demorcm" = "ls",
                          "forms" = "ls",
                          "internal" = "ls",
                          "kirk" = "ls",
                          "ls sales" = "ls",
                          "ls staff" = "ls",
                          "ls demo" = "ls",
                          "site14" = "ls",
                          "site4" = "ls",
                          "stage" = "ls",
                          "stating" = "ls",
                          "ltr" = "lrt",
                          "lake wellness" = "lw",
                          "nlw" = "lw",
                          "mrc" = "mcr",
                          "multi" = "mcr",
                          "mtc" = "mcr",
                          "multiple" = "mcr",
                          "midland" = "midlandjd",
                          "momenta" = "mo",
                          "momenutous" = "momentous",
                          "mfv" = "mvf",
                          "?" = "n/a",
                          "??" = "n/a",
                          "lab" = "n/a",
                          "labs" = "n/a",
                          "mako" = "n/a",
                          "millenium" = "n/a",
                          "na" = "n/a",
                          "other" = "n/a",
                          "qualitox laboratories" = "n/a",
                          "real deal" = "n/a",
                          "salesforce" = "n/a",
                          "spam" = "n/a",
                          "newbeginnings" = "nb",
                          "ndwf" = "ndfw", 
                          "ncfw" = "ndfw",
                          "ndts" = "ndtc", 
                          "ntdc" = "ndtc",
                          "ntdc" = "ndtc",
                          "new day" = "newday",
                          "nex" = "nextsteptoday",
                          "nextstep" = "nextsteptoday",
                          "nhorizon" = "nhorizons",
                          "nh - rcm" = "nhtc",
                          "njbh ghr" = "njbh",
                          "nola, rr" = "nola",
                          "new port" = "np",
                          "newpo" = "np",
                          "newport" = "np",
                          "nre port" = "np",
                          "mp" = "np",
                          "np test" = "np",
                          "nptest" = "np",
                          "nptraining" = "np",
                          "newroads" = "nr",
                          "nvhv" = "nvh",
                          "mwr" = "nwr",
                          "nwrc" = "nwr",
                          "mycfl" = "nycfl",
                          "nycl" = "nycfl",
                          "nyfcl" = "nycfl",
                          "outcomes" = "odc",
                          "onsi" = "onsite",
                          "onsitetest" = "onsite",
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
                          "praseumtest" = "praesum",
                          "prasesum" = "praesum",
                          "praseum" = "praesum",
                          "priemererc" = "premiererc",
                          "renewed hope rcm" = "rhr",
                          "rhr rcm" = "rhr",
                          "rootmentalhealth" = "rootedmentalhealth",
                          "roots" = "rootedmentalhealth",
                          "redriver" = "rr",
                          "rcr" = "rrc",
                          "resolutionrecovery" = "rrc",
                          "rrr" = "rrt",
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
                          "sekkingintegrity" = "seekingintegrity",
                          "skpypointrecovery" = "skypointrecovery",
                          "skypoint" = "skypointrecovery",
                          "skypoint" = "skypointrecovery",
                          "skywaa" = "skywaybh",
                          "skywah" = "skywaybh",
                          "skyway" = "skywaybh",
                          "serenity" = "slr",
                          "sor.zencharts.com" = "sor",
                          "srs, lw" = "srs",
                          "stephen" = "stephencenter",
                          "stephenscenter" = "stephencenter",
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
                          "tprtest" = "tpr",
                          "trauma" = "traumari",
                          "traumarecoveryinst" = "traumari",
                          "truamari" = "traumari",
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
                          "ctc" = "zencharts",
                          "ctc.zencharts.com" = "zencharts",
                          "ctct" = "zencharts",
                          "lrrc.zen cw.ls" = "zencharts",
                          "ror" = "zencharts",
                          "ror.zen" = "zencharts",
                          "tco - zen" = "zencharts",
                          "zen" = "zencharts",
                          "zia" = "zrc",
                          "zcr" = "zrc")

# * * Apply replacement vector to org codes column ---------------------------------------------
clean_df <- clean_df %>%
  mutate(orgcode = case_when(
    orgcode %in% names(replacement_orgcodes) ~ replacement_orgcodes[orgcode],
    TRUE ~ orgcode  # Keep other values unchanged
  ))

# * Mark excluded orgcodes that aren't LS Support tickets  -------------------------------------------------------------
clean_df <- clean_df %>%
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

# 2024 financial tickets
financial_tickets_2024 <- clean_df %>%
  filter(created_at_date >= "2024-01-01",
         area == "FINANCE")

# #--------------------Export Data-----------------
# # Export the cleaned data
# write_csv(clean_df, "./data/processed/clean_data.csv")
