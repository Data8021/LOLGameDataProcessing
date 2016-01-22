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


teamTest <- vector()

for (i in 1:length(fullGameList)) {
  
  testTeam <- fullGameList[[i]][["participants"]][["teamId"]][1:5]
  if (all(testTeam == 100)) {
    teamTest[i] <- "Good"
  } else {
    teamTest[i] <- "Bad"
  }
  
}

