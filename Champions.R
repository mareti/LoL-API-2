library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

getChampionInfo <- function() {
    BASE <- paste("http://ddragon.leagueoflegends.com/cdn"
                  , "/6.3.1/data/en_US/champion.json"
                  , sep=""
                  )
    
    requestURL <- BASE
    responseBody <- fromJSON(requestURL)
    
    champions <- data.frame(stringsAsFactors=FALSE)
    stats <- data.frame(stringsAsFactors=FALSE)
    info <- data.frame(stringsAsFactors=FALSE)
    
    for (i in 1:length(responseBody$data)) {
        key <- responseBody$data[[i]]$key
        name <- responseBody$data[[i]]$name
        title <- responseBody$data[[i]]$title
        info <- responseBody$data[[i]]$info
        tag0 <- responseBody$data[[i]]$tags[1]
        tag1 <- responseBody$data[[i]]$tags[2]
        stats <- responseBody$data[[i]]$stats
        
        df <- data.frame(key, name, title, info, tag0, tag1, stats
                         , stringsAsFactors=FALSE)
        champions <- rbind(champions, df)
    }
    
    tbl_df(champions)
}

ChampionExtract <- function() {
    df <- getChampionInfo()
    
    write.csv(df, file="../LoL Extracts/Champions (Full Stats).csv")
    write.csv(df %>%
                  select(key, name, title, tag0, tag1)
              , file="../LoL Extracts/Champions (Summary).csv"
              )
}