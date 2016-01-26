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


names(fullGameList[[i]][["participants"]])
names(fullGameList[[i]][["participants"]][["stats"]])

pStats <- fullGameList[[i]][["participants"]][["stats"]]

fullGameList[[i]][["participants"]][["stats"]][["participantId"]]
allGameData[[i, "masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[1]]
fullGameList[[i]][["participants"]][["runes"]][[1]]
yTest <- fullGameList[[i]][["participants"]][["runes"]][[1]]



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
                          masteries = I(list()),
                          stringsAsFactors = FALSE)