## Code to Pull out high level game information

## Load raw game details
load("fullGameList.Rda")

## Initialize data frame
allGameData <- data.frame(gameHash = as.character(),
                          gameId = as.numeric(),
                          platformId = as.character(),
                          gameCreation = as.numeric(),
                          gameDuration = as.numeric(),
                          queueId = as.numeric(),
                          mapId = as.numeric(),
                          seasonId = as.numeric(),
                          gameVersion = as.numeric(),
                          gameMode = as.character(),
                          gameType = as.character(),
                          t1Win = as.character(),
                          t1FirstBlood = as.character(),
                          t1FirstTower = as.character(), 
                          t1FirstInhibitor = as.character(),
                          t1FirstBaron = as.character(),
                          t1FirstDragon = as.character(),
                          t1FirstRiftHerald = as.character(),
                          t1TowerKills = as.numeric(),
                          t1InhibitorKills = as.numeric(),
                          t1BaronKills = as.numeric(),
                          t1DragonKills = as.numeric(),
                          t1VilemawKills = as.numeric(),
                          t1RiftHeraldKills = as.numeric(),
                          t1DominionVictoryScore = as.numeric(),
                          t2Win = as.character(),
                          t2FirstBlood = as.character(),
                          t2FirstTower = as.character(), 
                          t2FirstInhibitor = as.character(),
                          t2FirstBaron = as.character(),
                          t2FirstDragon = as.character(),
                          t2FirstRiftHerald = as.character(),
                          t2TowerKills = as.numeric(),
                          t2InhibitorKills = as.numeric(),
                          t2BaronKills = as.numeric(),
                          t2DragonKills = as.numeric(),
                          t2VilemawKills = as.numeric(),
                          t2RiftHeraldKills = as.numeric(),
                          t2DominionVictoryScore = as.numeric(),
                          stringsAsFactors = FALSE)

## Load game data from each game
for (i in 1:1386) {
  
  ## Generic game data
  allGameData[i, "gameHash"] <- names(fullGameList)[i]
  allGameData[i, "gameId"] <- fullGameList[[i]][["gameId"]]
  allGameData[i, "platformId"] <- fullGameList[[i]][["platformId"]]
  allGameData[i, "gameCreation"] <- fullGameList[[i]][["gameCreation"]]
  allGameData[i, "gameDuration"] <- fullGameList[[i]][["gameDuration"]]
  allGameData[i, "queueId"] <- fullGameList[[i]][["queueId"]] ## Unnecesary
  allGameData[i, "mapId"] <- fullGameList[[i]][["mapId"]] ## likely not necesary
  allGameData[i, "seasonId"] <- fullGameList[[i]][["seasonId"]] ## likely not Unnecesary
  allGameData[i, "gameVersion"] <- fullGameList[[i]][["gameVersion"]] ## Not sure how to handle yet
  allGameData[i, "gameMode"] <- fullGameList[[i]][["gameMode"]] ## Unnecesary
  allGameData[i, "gameType"] <- fullGameList[[i]][["gameType"]] ## Unnecesary
  
  ## Team 1 data
  allGameData[i, "t1Win"] <- fullGameList[[i]][["teams"]][["win"]][[1]]
  allGameData[i, "t1FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[1]]
  allGameData[i, "t1FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[1]]
  allGameData[i, "t1FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[1]]
  allGameData[i, "t1FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[1]]
  allGameData[i, "t1FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[1]]
  allGameData[i, "t1TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[1]]
  allGameData[i, "t1InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[1]]
  allGameData[i, "t1BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[1]]
  allGameData[i, "t1DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[1]]
  allGameData[i, "t1VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[1]]
  allGameData[i, "t1DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[1]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[1]]
  } else {
    allGameData[i, "t1FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[1]]
  } else {
    allGameData[i, "t1RiftHeraldKills"] <- NA
  }
  
  ## Team 2 data
  allGameData[i, "t2Win"] <- fullGameList[[i]][["teams"]][["win"]][[2]]
  allGameData[i, "t2FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[2]]
  allGameData[i, "t2FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[2]]
  allGameData[i, "t2FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[2]]
  allGameData[i, "t2FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[2]]
  allGameData[i, "t2FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[2]]
  allGameData[i, "t2TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[2]]
  allGameData[i, "t2InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[2]]
  allGameData[i, "t2BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[2]]
  allGameData[i, "t2DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[2]]
  allGameData[i, "t2VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[2]]
  allGameData[i, "t2DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[2]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[2]]
  } else {
    allGameData[i, "t2FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[2]]
  } else {
    allGameData[i, "t2RiftHeraldKills"] <- NA
  }

}

## Save game data
save(allGameData, file="allGameData.Rda")
