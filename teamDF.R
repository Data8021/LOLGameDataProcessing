library(jsonlite)
library(dplyr)
## Create team df


## Load worlds schedule
load("data/scheduleGames.Rda")

## Filter for games with Realm
games <- filter(games, gameRealm != "NA")

## Get distinct tournament/match list
matchList <- distinct(select(games, tournamentID, matchID))

## Intialize df for gameID/gameHash/team information
teamDF <- data.frame(teamID=numeric(),
                     teamSlug=character(),
                     teamName=character(),
                     teamAcro=character(),
                     stringsAsFactors = FALSE)

## Loop through each match
for (i in 1:nrow(matchList)) {
  
  ## Intialize tempdf
  tempTeamDF <- data.frame(teamID=numeric(),
                           teamSlug=character(),
                           teamName=character(),
                           teamAcro=character(),
                           stringsAsFactors = FALSE)
  
  ## Temporarily store each match
  tempJSON <- fromJSON(paste0("http://api.lolesports.com/api/v2/highlanderMatchDetails?tournamentId=", matchList[i, "tournamentID"], "&matchId=", matchList[i, "matchID"]))

  ## Load team information
  tempTeamDF[1, "teamID"] <- tempJSON[["teams"]][["id"]][[1]]
  tempTeamDF[1, "teamSlug"] <- tempJSON[["teams"]]["slug"][[1]][1]
  tempTeamDF[1, "teamName"] <- tempJSON[["teams"]][["name"]][[1]]
  tempTeamDF[1, "teamAcro"] <- tempJSON[["teams"]][["acronym"]][[1]]
  tempTeamDF[2, "teamID"] <- tempJSON[["teams"]][["id"]][[2]]
  tempTeamDF[2, "teamSlug"] <- tempJSON[["teams"]]["slug"][[1]][2]
  tempTeamDF[2, "teamName"] <- tempJSON[["teams"]][["name"]][[2]]
  tempTeamDF[2, "teamAcro"] <- tempJSON[["teams"]][["acronym"]][[2]]
  
  ## Bind temp DFs on full DFs
  teamDF <- rbind(teamDF, tempTeamDF)
  
  print(i)
  
  
}

## Strip duplicate player information
teamDF <- distinct(teamDF)


## Save files
save(teamDF, file="data/teamDF.Rda")
