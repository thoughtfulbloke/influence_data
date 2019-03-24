# influence_data

Code for gathering influence campaign data from Twitter in #rstats. 

Specifically for campaigns where you are looking for a coordinated campaign to influence a set of accounts and those following them. 

Put the initial accounts in starting_files/seed_accounts.txt one per a line

Put keywords you are focused on from those accounts in starting_files/keyword_list.txt one per a line

The script collects relevant tweets from those accounts and all replies to those accounts, which can be matched up at analysis stage (no analysis code in this, just data gathering)
