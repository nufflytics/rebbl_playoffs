library(shiny)
library(tidyverse)
library(glue)

race_img <- function(race_id) {
  switch (race_id,
          "1"= "human.png",
          "2"= "dorf.png",
          "3"= "rats.png",
          "4"= "orc.png",
          "5"= "liz.png",
          "6"= "gobbo.png",
          "7"= "welf.png",
          "8"= "chaos.png",
          "9"= "delf.png",
          "10" = "undead.png",
          "11" = "fling.png",
          "12" = "norse.png",
          "13" = "zon.png",
          "14" = "elf.png",
          "15" = "helf.png",
          "16" = "khemri.png",
          "17" = "necro.png",
          "18" = "nurgle.png",
          "19" = "ogre.png",
          "20" = "vamp.png",
          "21" = "chorf.png",
          "22" = "uw.png",
          "24" = "bret.png",
          "25" = "kislev.png"
  )
}

team <- function(pos, team, winner = F, score = "") {
  if(is_empty(winner)) {
    winner  = F
  }
  
  if(is.null(team)) {return(tags$li(class = glue("team team-{pos}"), HTML("&nbsp;")))}
  
  a(href = glue("http://bb2leaguemanager.com/Leaderboard/team_detail.php?team_id={team$id}&community_id=10"),
    target="_blank",
    "data-tooltip" = team$name,
    tags$li(
      class =  glue("team team-{pos}{ifelse(winner, ' winner', '')} spoiler_bg no_col"),
      img(src = glue("img/{race_img(team$race)}"), height = 25),
      img(src = glue("http://images.bb2.cyanide-studio.com/logos/Logo_{team$logo}.png"), height = 25),
      team$name.1,
      span(class = "score spoiler hidden", score)
    )
  )
}

custom_team <- function(pos, name, img = NULL) {
  tags$li(
    class = glue("team team-{pos}"),
    img(src = img, height = 25),
    span(class = "score", name)
  )
}

matchup <- function(team1 = NULL, team2=NULL, class = NULL, match_result = NULL) {
  winning_coach = winner(match_result)
  
  tags$ul(class = paste("matchup", class),
          team("top", team1, team1$name.1 == winning_coach, score = score(match_result, team1$name.1) ),
          team("bottom", team2, team2$name.1 == winning_coach, score = score(match_result, team2$name.1) )
  )
}

coach_list <- list(
  #top left
  c("SeanManTV","Chabxxu"),
  c("Miraskadu", "Hindi"),
  c("LazarusDigz", "Ravenpoe"),
  c("Steave","Superfedtv"),
  c("Creatan","Dick_Delaware"),
  c("Gdaynick","Hogstench"),
  c("liamcoulston","Puppi"),
  #bottom left
  c("Stoobings","Hipzter"),
  c("Djoolyurn","Ficction"),
  c("The_Red_Joker","JapeNZ"),
  c("Spoonybard","Tyladrhas"),
  c("Findeco","Kejiruze"),
  c("Gengar","Monaker"),
  c("Werecaster","Rama Set"),
  #top right
  c("Waeve","Morka"),
  c("AGrain","AndyDavo"),
  c("FullMetalCOS","HirumaMajere"),
  c("Aldar","Stouticus"),
  c("Jeoff7733","the Sage"),
  c("Thessa","InfinitePink"),
  c("schlice","Regnen"),
  c("Uber The Noober","Shadorra"),
  #bottom right
  c("SoulOfDragnFire","JamusMcgamuS"),
  c("Gaudi","Larkstar"),
  c("Motleee","BufordTJustice"),
  c("Manus Atra","UnseenWalker"),
  c("dsharpe356","Bleedinghippy"),
  c("Viajero","Fall"),
  c("Luminous","Hoyleyboy")
)



  l <- readRDS("data/ladders.rds")
  all_coaches <- l %>% map_df(~map_df(.,~head(.) %>% bind_rows))
  
  winner <- function(match) {
    if(is_empty(match)) return(NULL)
    
    filter(match[[1]], score == max(score))$coach
  }
  
  score <- function(match, c) {
    if(is_empty(match)) return(NULL)
    
    filter(match[[1]], coach == c)$score
  }
  
  team_details <- function(coach) {
    if(is.null(coach)) return(NULL)
    
    all_coaches %>% filter(name.1 == coach)
  }
  
shinyServer(function(input, output, session) {
  
  playoff_data <- reactiveFileReader(1000*5, session, "data/playoff_contests.rds", readRDS)
  
  ro64 <- reactive({
    map(coach_list, function(coaches) {keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })
  
  ro32_coaches <- reactive({list(
    #top left
    c("Horney Cricket", winner(ro64()[[1]])),
    c(winner(ro64()[[2]]), winner(ro64()[[3]])),
    c(winner(ro64()[[4]]), winner(ro64()[[5]])),
    c(winner(ro64()[[6]]), winner(ro64()[[7]])),
    #bottom left
    c(winner(ro64()[[8]]), winner(ro64()[[9]])),
    c(winner(ro64()[[10]]), winner(ro64()[[11]])),
    c(winner(ro64()[[12]]), winner(ro64()[[13]])),
    c(winner(ro64()[[14]]), "Gerbear"),
    #top right
    c(winner(ro64()[[15]]), winner(ro64()[[16]])),
    c(winner(ro64()[[17]]), winner(ro64()[[18]])),
    c(winner(ro64()[[19]]), winner(ro64()[[20]])),
    c(winner(ro64()[[21]]), winner(ro64()[[22]])),
    #bottom right
    c("PapaNasty", winner(ro64()[[23]])),
    c(winner(ro64()[[24]]), winner(ro64()[[25]])),
    c(winner(ro64()[[26]]), winner(ro64()[[27]])),
    c(winner(ro64()[[27]]), winner(ro64()[[28]]))
  )})
  
  ro32 <- reactive({
    map(ro32_coaches(), function(coaches) {if(length(coaches != 2)) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })
  
  # ro16_coaches <- reactive({list(
  #   #top left
  #   c(winner(ro32()[[1]]), winner(ro32[[2]])),
  #   c(winner(ro32()[[3]]), winner(ro32[[4]])),
  #   #bottom left
  #   c(winner(ro32()[[5]]), winner(ro32[[6]])),
  #   c(winner(ro32()[[7]]), winner(ro32[[8]])),
  #   #top right
  #   c(winner(ro32()[[9]]), winner(ro32[[10]])),
  #   c(winner(ro32()[[11]]), winner(ro32[[12]])),
  #   #bottom right
  #   c(winner(ro32()[[13]]), winner(ro32[[14]])),
  #   c(winner(ro32()[[15]]), winner(ro32[[16]]))
  # )})
  # 
  # ro16 <- reactive({
  #   map(ro16_coaches(), function(coaches) {if(length(coaches != 2)) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  # })
  # 
  # ro8_coaches <- reactive({list(
  #   #top left
  #   c(winner(ro16()[[1]]), winner(ro16[[2]])),
  #   #bottom left
  #   c(winner(ro16()[[3]]), winner(ro16[[4]])),
  #   #top right
  #   c(winner(ro16()[[5]]), winner(ro16[[6]])),
  #   #bottom right
  #   c(winner(ro16()[[7]]), winner(ro16[[8]])),
  # )})
  # 
  # ro8 <- reactive({
  #   map(ro8_coaches(), function(coaches) {if(length(coaches != 2)) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  # })
  # 
  # sf_coaches <- reactive({list(
  #   c(winner(ro8()[[1]]), winner(ro8()[[2]])),
  #   c(winner(ro8()[[3]]), winner(ro8()[[4]]))
  # )})
  # 
  # sf <- reactive({
  #   map(sf_coaches(), function(coaches) {if(length(coaches != 2)) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  # })
  # 
  # sb_coaches <- reactive({list(
  #   c(winner(sf()[[1]]), winner(sf()[[2]]))
  # )})
  # 
  # sb <- reactive({
  #   map(sb_coaches(), function(coaches) {if(length(coaches != 2)) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  # })
  
  output$bracket <- renderUI({
    tags$section(
      id = "bracket",
      div(
        class = "container",
        div(
          class = "split split-one",
          div(
            class = "round round-one current",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starts May 23")),
            matchup(class = "blank"), # RELD1 Bye
            matchup(
              l$Gman$D3[1,],
              l$REL$D6[2,],
              match_result = ro64()[[1]]
            ),
            matchup(
              l$BigO$D1[4,],
              l$Gman$D6A[1,],
              match_result = ro64()[[2]]
            ),
            tags$ul(class = "matchup",
                    team("top", l$REL$D4[1,]),
                    a(href = "http://www.bb2leaguemanager.com/Leaderboard/team_detail.php?team_id=1953913&community_id=10",
                      target = "_blank",
                      "data-tooltip" = "GADS Gobstoppers",
                      tags$li(
                        class =  "team team-bottom",
                        img(src = "img/gobbo.png", height = 25),
                        img(src = "http://images.bb2.cyanide-studio.com/logos/Logo_Goblin_03.png", height = 25),
                        "Ravenpoe",
                        span(class = "score", "")
                      )
                    )
            ),
            matchup(
              l$Gman$D3[3,],
              l$REL$D1[5,],
              match_result = ro64()[[4]]
            ),
            matchup(
              l$Gman$D2[1,],
              l$REL$D9C[1,],
              match_result = ro64()[[5]]
            ),
            matchup(
              l$BigO$D2[1,],
              l$REL$D9E[1,],
              match_result = ro64()[[6]]
            ),
            matchup(class = "mid",
                    l$BigO$D1[2,],
                    l$Gman$D6B[1,],
                    match_result = ro64()[[7]]
            ),
            matchup(
              l$REL$D2[3,],
              l$REL$D3[2,],
              match_result = ro64()[[8]]
            ),
            matchup(
              l$BigO$D4B[1,],
              l$REL$D1[3,],
              match_result = ro64()[[9]]
            ),
            matchup(
              l$REL$D5[2,],
              l$BigO$D2[2,],
              match_result = ro64()[[10]]
            ),
            matchup(
              l$REL$D6[1,],
              l$Gman$D4[2,],
              match_result = ro64()[[11]]
            ),
            matchup(
              l$BigO$D1[3,],
              l$Gman$D6C[1,],
              match_result = ro64()[[12]]
            ),
            matchup(
              l$REL$D2[2,],
              l$REL$D4[3,],
              match_result = ro64()[[13]]
            ),
            matchup(
              l$BigO$D3[1,],
              l$Gman$D5[2,],
              match_result = ro64()[[14]]
            ),
            matchup(class = "blank") # GmanD1 Bye
          ),
          div(
            class = "round round-two spoiler hidden",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starts May 30")),
            matchup(
              l$REL$D1[1,],
              team_details(winner(ro64()[[1]])),
              match_result = ro32()[[1]]
              
            ),
            matchup(
              team_details(winner(ro64()[[2]])),
              team_details(winner(ro64()[[3]])),
              match_result = ro32()[[2]]
              ),
            matchup(
              team_details(winner(ro64()[[4]])),
              team_details(winner(ro64()[[5]])),
              match_result = ro32()[[3]]
            ),
            matchup(class = "mid",
                    team_details(winner(ro64()[[6]])),
                    team_details(winner(ro64()[[7]])),
                    match_result = ro32()[[4]]
                    ),
            matchup(
              team_details(winner(ro64()[[8]])),
              team_details(winner(ro64()[[9]])),
              match_result = ro32()[[5]]
            ),
            matchup(
              team_details(winner(ro64()[[10]])),
              team_details(winner(ro64()[[11]])),
              match_result = ro32()[[6]]
            ),
            matchup(
              team_details(winner(ro64()[[12]])),
              team_details(winner(ro64()[[13]])),
              match_result = ro32()[[7]]
            ),
            matchup(
              team_details(winner(ro64()[[14]])),
              l$Gman$D1[1,],
              match_result = ro32()[[8]]
            )
          ),
          div(
            class = "round round-three ",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starts June 6")),
            matchup(
              # team_details(winner(ro32()[[1]])),
              # team_details(winner(ro32()[[2]])),
              # match_result = ro16()[[1]]
            ),
            matchup(class = "mid",
                    # team_details(winner(ro32()[[3]])),
                    # team_details(winner(ro32()[[4]])),
                    # match_result = ro16()[[2]]
                    ),
            matchup(
              # team_details(winner(ro32()[[5]])),
              # team_details(winner(ro32()[[6]])),
              # match_result = ro16()[[3]]
            ),
            matchup(
              # team_details(winner(ro32()[[7]])),
              # team_details(winner(ro32()[[8]])),
              # match_result = ro16()[[4]]
            )
          ),
          div(
            class = "round round-four ",
            div(class = "round-details", "Hype Video Round", br(), span(class = "date", "Starts June 13")),
            matchup(class = "mid",
                    # team_details(winner(ro16()[[1]])),
                    # team_details(winner(ro16()[[2]])),
                    # match_result = ro8()[[1]]
                    ),
            matchup(
              # team_details(winner(ro16()[[3]])),
              # team_details(winner(ro16()[[4]])),
              # match_result = ro8()[[2]]
            )
          )
        ),
        div(
          class = "champion ",
          div(
            class = "semis-l",
            div(
              class = "round-details",
              "Top Half SF",
              br(),
              span(class = "date", "Starts June 20")
            ),
            matchup(class = "championship",
                    # team_details(winner(ro8()[[1]])),
                    # team_details(winner(ro8()[[2]])),
                    # match_result = sf()[[1]]
                    )
          ),
          div(
            class = "final",
            icon("trophy"),
            div(
              class = "round-details",
              "Superbowl",
              br(),
              span(class = "date", "Starts June 27")
            ),
            matchup(class = "championship",
                    # team_details(winner(sf()[[1]])),
                    # team_details(winner(sf()[[2]])),
                    # match_result = sb()[[1]]
                    )
          ),
          div(
            class = "semis-r",
            div(
              class = "round-details",
              "Bottom Half SF",
              br(),
              span(class = "date", "Starts June 20")
            ),
            matchup(class = "championship",
                    # team_details(winner(ro8()[[3]])),
                    # team_details(winner(ro8()[[4]])),
                    # match_result = sf()[[2]]
                    )
          )
        ),
        div(
          class = "split split-two",
          div(
            class = "round round-four ",
            div(class = "round-details", "Hype Video Round", br(), span(class = "date", "Starts June 13")),
            matchup(class = "mid",
                    # team_details(winner(ro16()[[5]])),
                    # team_details(winner(ro16()[[6]])),
                    # match_result = ro8()[[3]]
                    ),
            matchup(
              # team_details(winner(ro16()[[7]])),
              # team_details(winner(ro16()[[8]])),
              # match_result = ro8()[[4]]
            )
          ),
          div(
            class = "round round-three ",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starts June 6")),
            matchup(
              # team_details(winner(ro32()[[9]])),
              # team_details(winner(ro32()[[10]])),
              # match_result = ro16()[[5]]
            ),
            matchup(class = "mid",
                    # team_details(winner(ro32()[[11]])),
                    # team_details(winner(ro32()[[12]])),
                    # match_result = ro16()[[6]]
                    ),
            matchup(
              # team_details(winner(ro32()[[13]])),
              # team_details(winner(ro32()[[14]])),
              # match_result = ro16()[[7]]
            ),
            matchup(
              # team_details(winner(ro32()[[15]])),
              # team_details(winner(ro32()[[16]])),
              # match_result = ro16()[[8]]
            )
          ),
          div(
            class = "round round-two spoiler hidden",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starts May 30")),
            matchup(
              team_details(winner(ro64()[[15]])),
              team_details(winner(ro64()[[16]])),
              match_result = ro32()[[9]]
            ),
            matchup(
              team_details(winner(ro64()[[17]])),
              team_details(winner(ro64()[[18]])),
              match_result = ro32()[[10]]
            ),
            matchup(
              team_details(winner(ro64()[[19]])),
              team_details(winner(ro64()[[20]])),
              match_result = ro32()[[11]]
              ),
            matchup(class = "mid",
                    team_details(winner(ro64()[[21]])),
                    team_details(winner(ro64()[[22]])),
                    match_result = ro32()[[12]]
                    ),
            matchup(
              l$BigO$D1[1,],
              team_details(winner(ro64()[[23]])),
              match_result = ro32()[[13]]
            ),
            matchup(
              team_details(winner(ro64()[[24]])),
              team_details(winner(ro64()[[25]])),
              match_result = ro32()[[14]]
            ),
            matchup(
              team_details(winner(ro64()[[26]])),
              team_details(winner(ro64()[[27]])),
              match_result = ro32()[[15]]
            ),
            matchup(
              team_details(winner(ro64()[[28]])),
              team_details(winner(ro64()[[29]])),
              match_result = ro32()[[16]]
            )
          ),
          div(
            class = "round round-one current",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starts May 23")),
            matchup(
              l$REL$D1[2,],
              l$BigO$D4A[1,],
              match_result = ro64()[[15]]
            ),
            matchup(
              l$REL$D7[1,],
              l$Gman$D3[2,],
              match_result = ro64()[[16]]
            ),
            matchup(
              l$Gman$D1[5,],
              l$BigO$D2[3,],
              match_result = ro64()[[17]]
            ),
            matchup(
              l$Gman$D5[1,],
              l$REL$D7[2,],
              match_result = ro64()[[18]]
            ),
            matchup(
              l$REL$D9D[1,],
              l$Gman$D1[2,],
              match_result = ro64()[[19]]
            ),
            matchup(
              l$REL$D4[2,],
              l$Gman$D2[3,],
              match_result = ro64()[[20]]
            ),
            matchup(
              l$BigO$D1[5,],
              l$REL$D3[3,],
              match_result = ro64()[[21]]
            ),
            tags$ul(class ="matchup mid",
                    team("top", l$REL$D5[1,]),
                    #custom_team("bottom", "Minors Champion")
                    a(href = "https://cdn.discordapp.com/attachments/446844052302987284/446844075136778251/unknown.png",
                      target = "_blank",
                      "data-tooltip" = "Street Sharkss",
                      tags$li(
                        class =  "team team-bottom",
                        img(src = "img/liz.png", height = 25),
                        img(src = "http://images.bb2.cyanide-studio.com/logos/Logo_Lizardman_09.png", height = 25),
                        "Shadorra",
                        span(class = "score", "")
                      )
                    )
            ),
            matchup(class = "blank"), # BigOD1 Bye
            matchup(
              l$REL$D3[1,],
              l$BigO$D3[2,],
              match_result = ro64()[[23]]
            ),
            matchup(
              l$Gman$D1[4,],
              l$REL$D9A[1,],
              match_result = ro64()[[24]]
            ),
            matchup(
              l$REL$D2[1,],
              l$Gman$D6D[1,],
              match_result = ro64()[[25]]
            ),
            matchup(
              l$Gman$D1[3,],
              l$REL$D9B[1,],
              match_result = ro64()[[26]]
            ),
            matchup(
              l$REL$D8[2,],
              l$Gman$D2[2,],
              match_result = ro64()[[27]]
            ),
            matchup(
              l$Gman$D4[1,],
              l$REL$D8[1,],
              match_result = ro64()[[28]]
            ),
            matchup(
              l$REL$D1[4,],
              l$Gman$D6E[1,],
              match_result = ro64()[[29]]
            )
            
          )
        )
      )
    )
  })
  
})

