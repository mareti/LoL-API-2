library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

challengerSnapshot <- function() {
    # using a static file because I've already downloaded it...
    df <- fromJSON("../LoL Data/challenger snapshots/2016-03-03 NA.json")
    
    write.csv(df$entries, "../LoL Extracts/Challenger Players 2016-03-03 NA.csv")
    tbl_df(df$entries)
}