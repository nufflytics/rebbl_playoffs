library(nufflytics)
library(tidyverse)

key = readRDS("data/api.key")

O_divs <- c("Season 8 Div 1", "Season 8 Div 2", "Season 8 Div 3", "Season 8 Div 4A", "Season 8 Div 4B", "Season 8 Div 4A Swiss", "Season 8 Div 4B Swiss")
G_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6A", "Season 8 - Division 6B", "Season 8 - Division 6C", "Season 8 - Division 6D", "Season 8 - Division 6E", "Season 8 - 6C Swiss")
R_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6", "Season 8 - Division 7", "Season 8 - Division 8", "Season 8 - Division 9A", "Season 8 - Division 9B", "Season 8 - Division 9C", "Season 8 - Division 9D", "Season 8 - Division 9E", "Season 8 Division 8 Swiss")



BigO <- map(O_divs, ~api_ladder(key, league = "REBBL - Big O", ., )$ranking %>% flatten %>% as.data.frame())

BigO[[4]] <- bind_rows(BigO[[4]] ,BigO[[6]]) %>% group_by(name) %>% summarise(race = first(race), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))
BigO[[5]] <- bind_rows(BigO[[5]] ,BigO[[7]]) %>% group_by(name) %>% summarise(race = first(race), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))

BigO <- BigO[-c(6:7)] %>% set_names(paste0("D",c(1:3,paste0(4,LETTERS[1:2]))))

Gman <- map(G_divs, ~api_ladder(key, league = "REBBL - Gman", ., )$ranking %>% flatten %>% as.data.frame()) 

Gman[[8]] <-  bind_rows(Gman[[8]] ,Gman[[11]]) %>% group_by(name) %>% summarise(race = first(race), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))

Gman <- Gman[-c(11)] %>% set_names(paste0("D",c(1:5,paste0(6,LETTERS[1:5]))))

REL <- map(R_divs, ~api_ladder(key, league = "REBBL - REL", ., )$ranking %>% flatten %>% as.data.frame()) 

REL[[8]] <- bind_rows(REL[[8]] ,REL[[14]]) %>% group_by(name) %>% summarise(race = first(race), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))

REL <- REL[-c(14)] %>% set_names(paste0("D",c(1:8,paste0(9,LETTERS[1:5]))))

saveRDS(list(BigO=BigO, Gman=Gman, REL=REL), file = "data/ladders.rds")
