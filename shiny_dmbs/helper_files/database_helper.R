# let's first load the required libraries
library(DBI)
library(RSQLite)
library(dplyr)

# let's create the database
con <- dbConnect(RSQLite::SQLite(), "database/my_database.db")

