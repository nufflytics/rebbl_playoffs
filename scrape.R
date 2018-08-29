library(nufflytics)
library(tidyverse)

key = readRDS("data/api.key")

O_divs <- c("Season 9 - Division 1" , "Season 9 - Division 2" , "Season 9 - Division 3" , "Season 9 - Division 4A" , "Season 9 - Division 4B" , "Season 9 - 4A Swiss" , "Season 9 - 4B Swiss")
G_divs <- c("Season 9 - Division 1" , "Season 9 - Division 2" , "Season 9 - Division 3" , "Season 9 - Division 4" , "Season 9 - Division 5" , "Season 9 - Division 6" , "Season 9 - Division 7" , "Season 9 - Division 8A" , "Season 9 - Division 8B" , "Season 9 - Division 8C" , "Season 9 - Division 8D" , "Season 9 - Division 8E" , "Season 9 - Division 8F" , "Season 9 - Division 8G")
R_divs <- c("Season 9 - Division 1" , "Season 9 - Division 2" , "Season 9 - Division 3" , "Season 9 - Division 4" , "Season 9 - Division 5" , "Season 9 - Division 6" , "Season 9 - Division 7" , "Season 9 - Division 8" , "Season 9 - Division 9" ,"Season 9 - Division 10A" , "Season 9 - Division 10B" , "Season 9 - Division 10C" , "Season 9 - Division 10D" , "Season 9 - Division 10E")


get_ladder <- function(division, league, key) {
  data <- api_ladder(key, league, division)
  
  if("ranking" %in% names(data)) {
    data$ranking %>% flatten %>% as.data.frame(stringsAsFactors=F)
  } else {
    NULL
  }
  
}


BigO <- map(O_divs, get_ladder, league="REBBL - Big O", key)

BigO[[4]] <- bind_rows(BigO[[4]] ,BigO[[6]]) %>% group_by(name) %>% summarise(tv = last(tv), race_id = first(race_id), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))
BigO[[5]] <- bind_rows(BigO[[5]] ,BigO[[7]]) %>% group_by(name) %>% summarise(tv = last(tv), race_id = first(race_id), logo = first(logo), name.1 = first(name.1), score = sum(score), id = first(id)) %>% arrange(desc(score))

BigO <- BigO[-c(6:7)] %>% set_names(paste0("D",c(1:3,paste0(4,LETTERS[1:2]))))

Gman <- map(G_divs, get_ladder, league = "REBBL - Gman", key)

Gman <- Gman %>% set_names(paste0("D",c(1:7,paste0(8,LETTERS[1:7]))))

REL <- map(R_divs, get_ladder, league="REBBL - REL", key)

REL <- REL %>% set_names(paste0("D",c(1:9,paste0(10,LETTERS[1:5]))))

saveRDS(list(BigO=BigO, Gman=Gman, REL=REL), file = "data/ladders.rds")

map_df(c("REBBL - Big O", "REBBL - Gman", "REBBL - REL"), 
       ~{
         teams <- api_teams(key, league = ., limit = 1000)
         data_frame(Team = teams$teams$team, Region = teams$meta$league$name %>% stringr::str_replace_all(c("REBBL - "="", "Big O"="BigO", "GMan"="Gman")))
       } 
) %>% write_csv("data/regions.csv")

# process_matches <- function(match) {
#   data_frame(
#     coach = match$opponents %>% map_chr(pluck,"coach","name"), 
#     team  = match$opponents %>% map_chr(pluck,"team","name"), 
#     score = match$opponents %>% map_int(pluck,"team","score"),
#     round = match$round
#   )
# }
# 
# data <- api_contests(key, league = "REBBL Playoffs", competition = "REBBL Playoffs - Season 9", limit = 100, status = "played")
# 
# matches <- map(data$upcoming_matches, process_matches)
# 
# old_matches <- readRDS("data/playoff_contests.rds")
# 
# if (length(old_matches) != length(matches)) saveRDS(matches, file = "data/playoff_contests.rds")
