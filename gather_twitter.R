library(rtweet)
library(dplyr)
library(lubridate)

today_txt <- as.character(today(tz="Pacific/Auckland"))
# since_id seems to generate an unsupported API error when it is to: tweets not of your
# timeline so I am setting the more accurate status ID lower bound for from: tweets
# and using the date (since) for the lower bound of the to:tweets
# rather than working through why, I am working around the issue

before_threshold <- "1106322000000000000" # find a value for the time period that works4u
reply_minima <- "2019-03-15"

# all the OS calls assume the working directory is the directory the script is in.

current_folders <- list.dirs(getwd(), full.names=FALSE)

# the seed accounts text file are the initial accounts being checked
# one account per line
who <- readLines("starting_files/seed_accounts.txt")

# the keyword list file is the list of keywords being searched for
what <- readLines("starting_files/keyword_list.txt")

all_accounts <- data.frame(screen_name = who, stringsAsFactors = FALSE)
all_accounts$default_lower_bound <- before_threshold
all_accounts$lower_threshold <- before_threshold

folders_in_wd <- list.dirs(getwd(), full.names = FALSE)
existing_seed_folders <- grep("seed_tweets", folders_in_wd, value=T)
existing_seed_csvs <- combine(sapply(existing_seed_folders, list.files, 
                                     pattern="csv$", full.names=T))

# working out, for each account, the highest ID seed tweet
# so we don't need to download more of heavy duty tweets
# takes advantage that if the vector is empty it is apparently classed as logical
if(!is.logical(existing_seed_csvs)){
  csv_contents <- lapply(existing_seed_csvs, function(x){
  tryCatch(read_twitter_csv(x), error = function(e) NULL)})
  tweet_details <- bind_rows(csv_contents) %>% 
    select(screen_name, status_id) %>%
    distinct() %>%
    group_by(screen_name) %>%
    summarise(lower_border = as.character(max(as.numeric(status_id)))) %>%
    ungroup()
  all_accounts <- all_accounts %>% 
    left_join(tweet_details, by="screen_name") %>% 
    mutate(lower_threshold = ifelse(
      is.na(lower_border), default_lower_bound, lower_border))
}

seeds_vol <- paste0("seed_tweets_", today_txt)
dir.create(seeds_vol)
reply_vol <- paste0("reply_tweets_", today_txt)
dir.create(reply_vol)

### Gather new seed tweets

find_seed_tweets <- function(x,y, keyword_list=what, sdpth=seeds_vol){
  #x is a username I am searching last 7 to 10 days in conjunction with each keyword
  # I could write nest functions here, but given the limited keywords, lets go for
  # something clearer
  resultlist <- lapply(keyword_list, function(kw, acc, mnid){
    suppressMessages(search_tweets(paste0("from:", acc, " ",kw), n=1000, 
                                   retryonratelimit = TRUE, since_id=mnid))
  }, acc=x, mnid=y)
  bundledresults <- bind_rows(resultlist) 
  write_as_csv(bundledresults, file_name = paste0(sdpth, "/",x,".csv"))
  #If I have to kill this because it gets stuck, each accounts tweets are recorded on disk
  # as csvs by this step
  return(x)
}

mapply(find_seed_tweets, all_accounts$screen_name, all_accounts$lower_threshold)

## now find accounts that have said something the influence campaign might
# want to respond to, so combine all the collective seed tweets, including the new ones
# this is a reuse of earlier code, but the number of seed tweet folders has increased 
# initially there will be none, now there can be assumed to be 1 or more

folders_in_wd <- list.dirs(getwd(), full.names = FALSE)
existing_seed_folders <- grep("seed_tweets", folders_in_wd, value=T)
existing_seed_csvs <- combine(sapply(existing_seed_folders, list.files, 
                                     pattern="csv$", full.names=T))

csv_contents <- lapply(existing_seed_csvs, function(x){
  tryCatch(read_twitter_csv(x), error = function(e) NULL)})
seed_accounts <- bind_rows(csv_contents) %>% 
  select(screen_name) %>%
  distinct() %>%
  mutate(before_threshold = reply_minima)
# in the interest of manageable API calls, this is making the assumptions that
# accounts that have tweeted thing relevent to the campaign we are focused on 
# are the ones to collect replies for.

# to avoid double downloading replies to those accounts, get the maximum id number of 
# alread collected replies

existing_reply_folders <- grep("reply_tweets", folders_in_wd, value=T)
existing_reply_csvs <- combine(sapply(existing_reply_folders, list.files, 
                                     pattern="csv$", full.names=T))

read_and_add_replied_to <- function(x) {
  df <- tryCatch(read_twitter_csv(x), error = function(e) NULL)
  if(is.null(df)) {return(NULL)}
  csv_only <- gsub(".*/","",x)
  df$seed_is <- gsub("\\.csv","",csv_only)
  return(df)
}
if(!is.logical(existing_reply_csvs)){
  csv_contents <- lapply(existing_reply_csvs, read_and_add_replied_to)
  tweet_details <- bind_rows(csv_contents) %>% 
    select(seed_is, created_at) %>%
    distinct() %>%
    mutate(as_dt = ymd_hms(created_at)) %>%
    group_by(screen_name = seed_is) %>%
    summarise(lower_border = as.character(max(as_dt), format="%Y-%m-%d")) %>%
    ungroup()
  seed_accounts <- seed_accounts %>% 
    left_join(tweet_details, by="screen_name") %>% 
    mutate(lower_threshold = ifelse(
      is.na(lower_border), before_threshold, lower_border)) %>%
    select(screen_name, lower_threshold)
}

# now we get the replies in a simplified manner to the seed gathering

find_reply_tweets <- function(x,y, rpypth=reply_vol){
  #x is a username I am searching last 7 to 10 days of to: tweets
  results <- suppressMessages(search_tweets(paste0("to:", x), 
                                            retryonratelimit = TRUE, since=y))
  write_as_csv(results, file_name = paste0(rpypth, "/",x,".csv"))
  #If I have to kill this because it gets stuck, each accounts tweets are recorded on disk
  # as csvs by this step
  return(x)
}
mapply(find_reply_tweets, seed_accounts$screen_name, seed_accounts$lower_threshold)

