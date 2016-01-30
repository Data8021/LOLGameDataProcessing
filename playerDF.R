library(jsonlite)
library(dplyr)
## Create player df


## Load worlds schedule
load("data/scheduleGames.Rda")

## Filter for games with Realm
games <- filter(games, gameRealm != "NA")

## Get distinct tournament/match list
tournamentList <- distinct(select(games, tournamentID))

## Remove all-start tournament
tournamentList <- tournamentList[-(6:9),]

## Intialize df for gameID/gameHash/team information
playerDF <- data.frame(playerID=numeric(),
                       playerName=character(),
                       playerPosition=character(),
                       playerSlug=character(),
                       playerTeamAcro=character(),
                       playerTeamSlug=character(),
                       stringsAsFactors = FALSE)

## Loop through each match
for (i in 1:length(tournamentList)) {
  
  ## Temporarily store each tournament as a temp df
  tempDF <- fromJSON(paste0("http://api.lolesports.com/api/v2/tournamentPlayerStats?tournamentId=", tournamentList[i]))[[1]]

  ## Remove variables
  tempDF <- select(tempDF, -(gamesPlayed:minutesPlayed))
  
  ## Rename remaining variables
  tempDF <- rename(tempDF,
                   playerID = id,
                   playerName = name,
                   playerPosition = position,
                   playerTeamAcro = team,
                   playerTeamSlug = teamSlug)
  
  ## Bind temp DFs on full DFs
  playerDF <- rbind(playerDF, tempDF)
  
  print(i)
  
}

## Strip duplicate player information
playerDF <- distinct(playerDF)

## Add in combo name
playerDF <- mutate(playerDF, playerComboName = paste(playerTeamAcro, playerName, sep=" "))

playerDF <- arrange(playerDF, playerComboName)
## Save files
save(playerDF, file="data/playerDF.Rda")
