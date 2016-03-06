library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

challengerTimelines <- function() {
    path <- "../LoL Data/challenger matches"
    file <- dir(path, pattern=".json", full.names=TRUE, recursive=FALSE)
    
    for (i in 1:2) { #length(file)) {
        df1 <- fromJSON(file[i])
        df2 <- fromJSON(file[i], flatten=TRUE)
        matchId <- df1$matchId
        
        if (df1$matchMode != "CLASSIC") {
            print(paste(i, "-", file[i], "is not CLASSIC"))
            next
        }
        
        if (df1$matchType != "MATCHED_GAME") {
            print(paste(i, "-", file[i], "is not MATCHED_GAME"))
            next
        }
        
        if (df1$queueType != "TEAM_BUILDER_DRAFT_RANKED_5x5") {
            print(paste(i, "-", file[i], "is not TEAM_BUILDER_DRAFT_RANKED_5x5"))
            next
        }
        
        teamData <- getTeamData(df1, matchId)
        participantData <- getParticipantData(df1, matchId)
        positionData <- getPositions(df2, matchId)
        
        if (i==1) {
            teamDataExtract <- teamData
            participantDataExtract <- participantData
            positionDataExtract <- positionData
        } 
        else {
            teamDataExtract <- rbind(teamDataExtract, teamData)
            participantDataExtract <- rbind(participantDataExtract, participantData)
            positionDataExtract <- rbind(positionDataExtract, positionData)
        }
        
        print(paste(i, "-", basename(file[i])))
    }
    print(teamDataExtract)
    print(participantDataExtract)
    print(positionDataExtract)
}

getTeamData <- function(df, matchId) {
    teams <- df$teams %>%
        select(teamId, winner, firstBlood, firstTower
               , firstInhibitor, firstBaron, firstDragon, firstRiftHerald
               , towerKills, inhibitorKills, baronKills, dragonKills
               , riftHeraldKills)
    
    teamSummary <- data.frame(matchId, teams, stringsAsFactors=FALSE)
    
    tbl_df(teamSummary)
}

getParticipantData <- function(df, matchId) {
    participants <- df$participants %>%
        select(teamId, spell1Id, spell2Id, championId, participantId)
    
    roles <- data.frame(df$participants$timeline$role
                        , stringsAsFactors=FALSE)
    
    lanes <- data.frame(df$participants$timeline$lane
                        , stringsAsFactors=FALSE)
    
    participantSummary <- data.frame(matchId, participants, roles, lanes
                                     , stringsAsFactors=FALSE)
    
    tbl_df(participantSummary)
}

getPositions <- function(dat, matchId) {
    pos1 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.1.participantId
               , level = participantFrames.1.level
               , x = participantFrames.1.position.x
               , y = participantFrames.1.position.y
               , currentGold = participantFrames.1.currentGold
               , totalGold = participantFrames.1.totalGold
               , xp = participantFrames.1.xp
               , minionsKilled = participantFrames.1.minionsKilled
               , jungleMinionsKilled = participantFrames.1.jungleMinionsKilled
        ) 
    
    pos2 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.2.participantId
               , level = participantFrames.2.level
               , x = participantFrames.2.position.x
               , y = participantFrames.2.position.y
               , currentGold = participantFrames.2.currentGold
               , totalGold = participantFrames.2.totalGold
               , xp = participantFrames.2.xp
               , minionsKilled = participantFrames.2.minionsKilled
               , jungleMinionsKilled = participantFrames.2.jungleMinionsKilled
        ) 
    
    pos3 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.3.participantId
               , level = participantFrames.3.level
               , x = participantFrames.3.position.x
               , y = participantFrames.3.position.y
               , currentGold = participantFrames.3.currentGold
               , totalGold = participantFrames.3.totalGold
               , xp = participantFrames.3.xp
               , minionsKilled = participantFrames.3.minionsKilled
               , jungleMinionsKilled = participantFrames.3.jungleMinionsKilled
        )
    
    pos4 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.4.participantId
               , level = participantFrames.4.level
               , x = participantFrames.4.position.x
               , y = participantFrames.4.position.y
               , currentGold = participantFrames.4.currentGold
               , totalGold = participantFrames.4.totalGold
               , xp = participantFrames.4.xp
               , minionsKilled = participantFrames.4.minionsKilled
               , jungleMinionsKilled = participantFrames.4.jungleMinionsKilled
        )
    
    pos5 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.5.participantId
               , level = participantFrames.5.level
               , x = participantFrames.5.position.x
               , y = participantFrames.5.position.y
               , currentGold = participantFrames.5.currentGold
               , totalGold = participantFrames.5.totalGold
               , xp = participantFrames.5.xp
               , minionsKilled = participantFrames.5.minionsKilled
               , jungleMinionsKilled = participantFrames.5.jungleMinionsKilled
        )
    
    pos6 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.6.participantId
               , level = participantFrames.6.level
               , x = participantFrames.6.position.x
               , y = participantFrames.6.position.y
               , currentGold = participantFrames.6.currentGold
               , totalGold = participantFrames.6.totalGold
               , xp = participantFrames.6.xp
               , minionsKilled = participantFrames.6.minionsKilled
               , jungleMinionsKilled = participantFrames.6.jungleMinionsKilled
        )
    
    pos7 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.7.participantId
               , level = participantFrames.7.level
               , x = participantFrames.7.position.x
               , y = participantFrames.7.position.y
               , currentGold = participantFrames.7.currentGold
               , totalGold = participantFrames.7.totalGold
               , xp = participantFrames.7.xp
               , minionsKilled = participantFrames.7.minionsKilled
               , jungleMinionsKilled = participantFrames.7.jungleMinionsKilled
        )
    
    pos8 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.8.participantId
               , level = participantFrames.8.level
               , x = participantFrames.8.position.x
               , y = participantFrames.8.position.y
               , currentGold = participantFrames.8.currentGold
               , totalGold = participantFrames.8.totalGold
               , xp = participantFrames.8.xp
               , minionsKilled = participantFrames.8.minionsKilled
               , jungleMinionsKilled = participantFrames.8.jungleMinionsKilled
        )
    
    pos9 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.9.participantId
               , level = participantFrames.9.level
               , x = participantFrames.9.position.x
               , y = participantFrames.9.position.y
               , currentGold = participantFrames.9.currentGold
               , totalGold = participantFrames.9.totalGold
               , xp = participantFrames.9.xp
               , minionsKilled = participantFrames.9.minionsKilled
               , jungleMinionsKilled = participantFrames.9.jungleMinionsKilled
        )
    
    pos10 <- dat$timeline$frames %>%
        select(timestamp
               , participantId = participantFrames.10.participantId
               , level = participantFrames.10.level
               , x = participantFrames.10.position.x
               , y = participantFrames.10.position.y
               , currentGold = participantFrames.10.currentGold
               , totalGold = participantFrames.10.totalGold
               , xp = participantFrames.10.xp
               , minionsKilled = participantFrames.10.minionsKilled
               , jungleMinionsKilled = participantFrames.10.jungleMinionsKilled
        ) 
    
    pos <- pos1
    pos <- rbind(pos, pos2)
    pos <- rbind(pos, pos3)
    pos <- rbind(pos, pos4)
    pos <- rbind(pos, pos5)
    pos <- rbind(pos, pos6)
    pos <- rbind(pos, pos7)
    pos <- rbind(pos, pos8)
    pos <- rbind(pos, pos9)
    pos <- rbind(pos, pos10)
    pos <- data.frame(dat$matchId, pos)
    
    tbl_df(pos)
}