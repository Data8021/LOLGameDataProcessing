library(dplyr)
library(lubridate)
## Load functions 
fromSeconds <- function(x){
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             if (i >= 3600) {
               y <- seconds_to_period(i)
               sprintf('%02d:%02d:%02d', y@hour, minute(y), second(y))
             } else {
               y <- seconds_to_period(i)
               sprintf('%02d:%02d', minute(y), second(y))
             }
           }  
    )  
  )  
} 
## Load game details
load("fullGameList.Rda")

## 
fullGameList[[i]][["gameId"]]
fullGameList[[i]][["platformId"]]
fullGameList[[i]][["gameCreation"]]
as.POSIXct((fullGameList[[i]][["gameCreation"]]/1000), origin = "1970-01-01", tz = "UTC")
fullGameList[[i]][["gameDuration"]]
fromSeconds(fullGameList[[i]][["gameDuration"]])
fullGameList[[i]][["queueId"]] ## Unnecesary
fullGameList[[i]][["mapId"]] ## Not sure how to handle 1 vs 11
fullGameList[[i]][["seasonId"]] ## likely not Unnecesary
fullGameList[[i]][["gameVersion"]] ## Not sure how to handle yet
fullGameList[[i]][["gameMode"]] ## Unnecesary
fullGameList[[i]][["gameType"]] ## Unnecesary

names(fullGameList[[i]])
names(fullGameList[[i]][["teams"]])
names(fullGameList[[i]][["participants"]])
names(fullGameList[[i]][["participants"]][["stats"]])
names(fullGameList[[1]][["participants"]][["timeline"]])

timelineD <- fullGameList[[i]][["participants"]][["timeline"]]

fullGameList[[i]][["participants"]][["stats"]][["participantId"]]
allGameData[[i, "masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[1]]
fullGameList[[i]][["participants"]][["runes"]][[1]]
yTest <- fullGameList[[i]][["participants"]][["timeline"]][["creepsPerMinDeltas"]][1, "10-20"]



masteriesRunesTest <- data.frame(numMasteries = as.numeric(),
                                 numRunes = as.numeric())

for (i in 1:length(fullGameList)) {
  
  masteriesRunesTemp <- data.frame(numMasteries = as.numeric(),
                                   numRunes = as.numeric())
  
  if ("masteries" %in% names(fullGameList[[i]][["participants"]])) {
  
    for (j in 1:10) {
      
      if (!is.null(fullGameList[[i]][["participants"]][["masteries"]][[j]])) {
        masteriesRunesTemp[j, 1] <- nrow(fullGameList[[i]][["participants"]][["masteries"]][[j]])
      }
      
      if (!is.null(fullGameList[[i]][["participants"]][["runes"]][[j]])) {
        masteriesRunesTemp[j, 2] <- nrow(fullGameList[[i]][["participants"]][["runes"]][[j]])
      }
      
    }  
    
  }
  
  masteriesRunesTest <- rbind(masteriesRunesTest, masteriesRunesTemp)
  
}



creepMinTest <- vector()

for (i in 1:length(fullGameList)) {
  
  print(names(fullGameList[[i]][["participants"]][["timeline"]][["creepsPerMinDeltas"]]))
  
}

timelineTest <- vector()

for (i in 1:length(fullGameList)) {
  
  timelineTest[i] <- length(names(fullGameList[[i]][["participants"]][["timeline"]]))
  
}

LanesTest <- data.frame(player1Lane = as.character(),
                             player2Lane = as.character(),
                             player3Lane = as.character(),
                             player4Lane = as.character(),
                             player5Lane = as.character(),
                             stringsAsFactors = FALSE)

for (i in 1:length(fullGameList)) {
  
  
  LanesTemp <- data.frame(player1Lane = as.character(),
                          player2Lane = as.character(),
                          player3Lane = as.character(),
                          player4Lane = as.character(),
                          player5Lane = as.character(),
                          stringsAsFactors = FALSE)
  
  LanesTemp[1, "player1Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[1]]
  LanesTemp[1, "player2Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[2]]
  LanesTemp[1, "player3Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[3]]
  LanesTemp[1, "player4Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[4]]
  LanesTemp[1, "player5Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[5]]
  LanesTemp[2, "player1Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[6]]
  LanesTemp[2, "player2Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[7]]
  LanesTemp[2, "player3Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[8]]
  LanesTemp[2, "player4Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[9]]
  LanesTemp[2, "player5Lane"] <- fullGameList[[i]][["participants"]][["timeline"]][["lane"]][[10]]

  LanesTest <- rbind(LanesTest, LanesTemp)
}