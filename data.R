library(rgdal)
library(matrixStats)
library(tidyverse)

map <- readOGR(dsn="data",
               layer="ca_vest_20",
               verbose=TRUE)

#dynamically search for variable names starting with "G" - meaning General Election
tgt <- grep("G",colnames(map@data))

map@data[,tgt] <- apply(map@data[,tgt], MARGIN=2, FUN= as.numeric)
elec <- map@data[,tgt]
elecmatrix <- as.matrix(map@data[,tgt])
map$elect_max <- rowMaxs(elecmatrix)
map$elect_max_prop <- rowMaxs(elecmatrix)/ rowSums(elecmatrix)
map$winner <- colnames(elec)[max.col(elec,ties.method="first")]
map$year <- substring(map$winner, 2,3)
map$year <- unname(sapply(map$year, FUN=function(x) paste0("20",x)))
map$election <- substring(map$winner, 4,6)
map$winner <- sapply(map$winner,
                       FUN=function(x){
                         switch(x,
                                "G20PREDBID" = "Joseph R. Biden",
                                "G20PRERTRU" = "Donald J. Trump",
                                "G20PRELJOR" = "Jo Jorgensen",
                                "G20PREGHAW" = "Howie Hawkins",
                                "G20PREAFUE" = "Roque 'Rocky' De La Fuente Guerra",
                                "G20PREPLAR" = "Gloria La Riva")
                       })
map$winner_party  <- sapply(map$winner,
                        FUN=function(x){
                          switch(x,
                                 "Joseph R. Biden" = "Democratic Party",
                                 "Donald J. Trump" = "Republican Party",
                                 "Jo Jorgensen" = "Libertarian Party",
                                 "Howie Hawkins" = "Green Party",
                                 "Roque 'Rocky' De La Fuente Guerra" = "American Independent Party",
                                 "Gloria La Riva" = "Peace and Freedom Party")
                        })
map$winner <- ifelse(map$elect_max == 0, NA, map$winner)
map$winner_party <- ifelse(map$elect_max == 0, NA, map$winner_party)
map$election <- rep("2020 Presidential", nrow(map))

map2 <- readOGR(dsn=".",
               layer="ca_vest_16",
               verbose=TRUE)

tgt <- grep("G",colnames(map2@data))

map2@data[,tgt] <- apply(map2@data[,tgt], MARGIN=2, FUN= as.numeric)
elec <- map2@data[,tgt]
elecmatrix <- as.matrix(map2@data[,tgt])
map2$elect_max <- rowMaxs(elecmatrix)
map2$elect_max_prop <- rowMaxs(elecmatrix)/ rowSums(elecmatrix)
map2$winner <- colnames(elec)[max.col(elec,ties.method="first")]
map2$year <- substring(map2$winner, 2,3)
map2$year <- unname(sapply(map2$year, FUN=function(x) paste0("20",x)))
map2$election <- substring(map2$winner, 4,6)
map2$winner <- sapply(map2$winner,
                     FUN=function(x){
                       switch(x,
                              "G16PREDCli" = "Hilary Clinton",
                              "G16PRERTru" = "Donald J. Trump",
                              "G16PREGSte" = "Jill Stein",
                              "G16PRELJoh" = "Gary Johnson",
                              "G16PREPLaR" = "Gloria La Riva")
                     })
map2$winner_party  <- sapply(map2$winner,
                            FUN=function(x){
                              switch(x,
                                     "Hilary Clinton" = "Democratic Party",
                                     "Donald J. Trump" = "Republican Party",
                                     "Gary Johnson" = "Libertarian Party",
                                     "Jill Stein" = "Green Party",
                                     "Gloria La Riva" = "Peace and Freedom Party")
                            })
map2$winner <- ifelse(map2$elect_max == 0, NA, map2$winner)
map2$winner_party <- ifelse(map2$elect_max == 0, NA, map2$winner_party)
map2$election <- rep("2016 Presidential", nrow(map2))

map@data <- map@data %>% select(COUNTY,election,SRPREC,elect_max, elect_max_prop, winner,election, winner_party)
map2@data <- map2@data %>% select(COUNTY,election,SRPREC,elect_max, elect_max_prop, winner,election, winner_party)
map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
map2 <- spTransform(map2, CRS("+proj=longlat +datum=WGS84"))

map <- rbind(map,map2, makeUniqueIDs = TRUE)

map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))

writeOGR(map, dsn = getwd(), layer="ca_vest", driver="ESRI Shapefile")
