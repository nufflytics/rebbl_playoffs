library(nufflytics)
library(tidyverse)

key = readRDS("data/api.key")

O_divs <- c("Season 8 Div 1", "Season 8 Div 2", "Season 8 Div 3", "Season 8 Div 4A", "Season 8 Div 4B")
G_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6A", "Season 8 - Division 6B", "Season 8 - Division 6C", "Season 8 - Division 6D", "Season 8 - Division 6E")
R_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6", "Season 8 - Division 7", "Season 8 - Division 8", "Season 8 - Division 9A", "Season 8 - Division 9B", "Season 8 - Division 9C", "Season 8 - Division 9D", "Season 8 - Division 9E" )

BigO <- map(O_divs, ~api_ladder(key, league = "REBBL - Big O", ., )$ranking %>% flatten %>% as.data.frame()) %>% set_names(paste0("D",c(1:3,paste0(4,LETTERS[1:2]))))
Gman <- map(G_divs, ~api_ladder(key, league = "REBBL - Gman", ., )$ranking %>% flatten %>% as.data.frame()) %>% set_names(paste0("D",c(1:5,paste0(6,LETTERS[1:5]))))
REL <- map(R_divs, ~api_ladder(key, league = "REBBL - REL", ., )$ranking %>% flatten %>% as.data.frame()) %>% set_names(paste0("D",c(1:8,paste0(9,LETTERS[1:5]))))

saveRDS(list(BigO=BigO, Gman=Gman, REL=REL), file = "data/ladders.rds")
