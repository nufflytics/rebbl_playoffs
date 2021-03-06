library(shiny)
library(tidyverse)
library(nufflytics)
library(glue)
key <- readRDS("data/api.key")

race_img <- function(race_id) {
  switch (as.character(race_id),
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

regions <- read_csv("data/regions.csv", trim_ws = F)

find_region<- function(team) {
  r = filter(regions, Team == team)
  
  if(nrow(r)!=1) {return(NULL)}
  
  reg = case_when(
    r$Region %in% c("REL", "Gman", "BigO") ~ glue::glue("{r$Region}_s"),
    r$Region == "REBBL" ~ "REBBL_fist_mod",
    r$Region == "Rookies" ~ "rebbrl"
  )
  
  reg
}

team <- function(pos, team, winner = F, score = "", round = 0) {
  if(is_empty(winner)) {
    winner  = F
  }
  
  spoiler = F
  
  if(is.null(team)) {return(tags$li(class = glue("team team-{pos}"), HTML("&nbsp;")))}
  
  a(href = glue("https://rebbl.net/rebbl/team/{team$id}"),
    target="_blank",
    "data-tooltip" = team$name,
    tags$li(
      class =  glue("team team-{pos}{ifelse(winner, ' winner', '')} {ifelse(spoiler, ' spoiler_bg no_col','')}"),
      img(src = glue("https://nufflytics.com/img/main/{find_region(team$name)}.png"), height = 25),
      img(src = glue("img/{race_img(team$race_id)}"), height = 25),
      #img(src = glue("http://images.bb2.cyanide-studio.com/logos/Logo_{team$logo}.png"), height = 25),
      team$name.1,
      span(class = glue("score {ifelse(spoiler, ' spoiler hidden','')}"), score)
    )
  )
}

custom_team <- function(pos, name, img = NULL) {
  tags$li(
    class = glue("team team-{pos}"),
    img(src = img, height = 25),
    span(name)
  )
}

matchup <- function(team1 = NULL, team2=NULL, class = NULL, match_result = NULL) {
  winning_coach = winner(match_result)
  
  if (is_empty(match_result)) {
    round = 0
  } else {
    round = match_result[[1]]$round[1]
    }
  
  trophy <- NULL
  
  if(!is.null(class)) {
    if(grepl("sb", class)) trophy <- img(src = "img/SB8.png", height = "120px")
    }
  
  tags$ul(class = paste("matchup", class),
          team("top", team1, team1$name.1 == winning_coach, score = score(match_result, team1$name.1), round = round),
          trophy,
          team("bottom", team2, team2$name.1 == winning_coach, score = score(match_result, team2$name.1), round = round )
  )
}


coach_list <- list(
  #top left
  c("Gman1", "Bye"),
  c("Steer","Stoobings"),
  c("JapeNZ", "wedge22"),
  c("Zsinj", "Cakengrad"),
  c("randomboy987","kaverne"),
  c("Thessa","BlissfulFire"),
  c("Stouticus","TheDrNick"),
  c("Muppetillo","Mistah J"),
  #bottom left
  c("BigO1", "Bye"),
  c("Mixtli","Holes"),
  c("Saace","Scytalen"),
  c("Chabxxu","VulpesInculta"),
  c("Werecaster","Teddy Rose"),
  c("Sandune","Mego"),
  c("SeanManTV","Hairy Coo"),
  c("Weravem","Mystaes"),
  #top right
  c("Kummostern","Razzle Storm"),
  c("michaels","Sandland"),
  c("the Sage","Larkstar"),
  c("Djoolyurn","Jimmy Burrito"),
  c("LazarusDigz","Kubusta"),
  c("Bärserk","Chubberson"),
  c("Gaudi","Khalerick"),
  c("AnteaterReborn","GemeneRick"),
  #bottom right
  c("ArchXL","AndyDavo"),
  c("Lynxx","Harringzord"),
  c("Morka","toastguy7"),
  c("McMacky","Ramhard"),
  c("PapaNasty","AndY90"),
  c("C_Arnoud","Barmution"),
  c("Tommo","tommytootall"),
  c("REL1", "Bye")
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
  
  ## Fill in data as playoffs commence
  ro64 <- reactive({
    map(coach_list, function(coaches) {keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })

  ro32_coaches <- reactive({list(
    #top left
    c(l$Gman$D1[1,]$name.1, winner(ro64()[[2]])),
    c(winner(ro64()[[3]]), winner(ro64()[[4]])),
    c(winner(ro64()[[5]]), winner(ro64()[[6]])),
    c(winner(ro64()[[7]]), winner(ro64()[[8]])),
    #bottom left
    c(l$BigO$D1[1,]$name.1, winner(ro64()[[10]])),
    c(winner(ro64()[[11]]), winner(ro64()[[12]])),
    c(winner(ro64()[[13]]), winner(ro64()[[14]])),
    c(winner(ro64()[[15]]), winner(ro64()[[16]])),
    #top right
    c(winner(ro64()[[17]]), winner(ro64()[[18]])),
    c(winner(ro64()[[19]]), winner(ro64()[[20]])),
    c(winner(ro64()[[21]]), winner(ro64()[[22]])),
    c(winner(ro64()[[23]]), winner(ro64()[[24]])),
    #bottom right
    c(winner(ro64()[[25]]), winner(ro64()[[26]])),
    c(winner(ro64()[[27]]), winner(ro64()[[28]])),
    c(winner(ro64()[[29]]), winner(ro64()[[30]])),
    c(winner(ro64()[[31]]), l$REL$D1[1,]$name.1)
  )})

  ro32 <- reactive({
    map(ro32_coaches(), function(coaches) {if(length(coaches) != 2) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })

  ro16_coaches <- reactive({list(
    #top left
    c("Bleedinghippy", winner(ro32()[[2]])),
    c(winner(ro32()[[3]]), winner(ro32()[[4]])),
    #bottom left
    c(winner(ro32()[[5]]), winner(ro32()[[6]])),
    c(winner(ro32()[[7]]), winner(ro32()[[8]])),
    #top right
    c(winner(ro32()[[9]]), winner(ro32()[[10]])),
    c(winner(ro32()[[11]]), winner(ro32()[[12]])),
    #bottom right
    c(winner(ro32()[[13]]), winner(ro32()[[14]])),
    c(winner(ro32()[[15]]), winner(ro32()[[16]]))
  )})

  ro16 <- reactive({
    map(ro16_coaches(), function(coaches) {if(length(coaches) != 2) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })

  ro8_coaches <- reactive({list(
    #top left
    c(winner(ro16()[[1]]), winner(ro16()[[2]])),
    #bottom left
    c(winner(ro16()[[3]]), winner(ro16()[[4]])),
    #top right
    c(winner(ro16()[[5]]), winner(ro16()[[6]])),
    #bottom right
    c(winner(ro16()[[7]]), winner(ro16()[[8]]))
  )})

  ro8 <- reactive({
    map(ro8_coaches(), function(coaches) {if(length(coaches) != 2) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })

  sf_coaches <- reactive({list(
    c(winner(ro8()[[1]]), winner(ro8()[[3]])),
    c(winner(ro8()[[2]]), winner(ro8()[[4]]))
  )})

  sf <- reactive({
    map(sf_coaches(), function(coaches) {if(length(coaches) != 2) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })

  sb_coaches <- reactive({list(
    #c(winner(sf()[[1]]), winner(sf()[[2]]))
    c(winner(sf()[[1]]), "Mystaes")
  )})

  sb <- reactive({
    map(sb_coaches(), function(coaches) {if(length(coaches) != 2) return(NULL) ;keep(playoff_data(), ~(all(coaches %in% .$coach)))} )
  })
  
  output$bracket <- renderUI({
    tags$section(
      id = "bracket",
      div(
        class = "container",
        div(
          class = "split split-one",
          div(
            class = "round round-one ",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starts Oct 3")),
            matchup(class = "blank"), # GmanD1 Bye
            matchup(
              l$REL$D9[2,],
              l$REL$D1[4,],
              match_result = ro64()[[2]]
            ),
            matchup(
              l$BigO$D1[2,],
              l$REL$D10B[1,],
              match_result = ro64()[[3]]
            ),
            matchup(
              l$Gman$D2[3,],
              l$REL$D7[3,],
              match_result = ro64()[[4]]
            ),
            matchup(
              l$Gman$D2[1,],
              l$BigO$D4B[1,],
              match_result = ro64()[[5]]
            ),
            matchup(
              l$REL$D2[3,],
              l$Gman$D7[1,],
              match_result = ro64()[[6]]
            ),
            matchup(
              l$REL$D3[1,],
              l$Gman$D8G[1,],
              match_result = ro64()[[7]]
            ),
            matchup(class = "mid",
                    l$Gman$D4[1,],
                    l$REL$D6[1,],
                    match_result = ro64()[[8]]
            ),
            matchup(class = "blank"), #BigO1 Bye
            matchup(
              l$Gman$D6[2,],
              l$REL$D4[1,],
              match_result = ro64()[[10]]
            ),
            matchup(
              l$REL$D1[3,],
              l$Gman$D8C[1,],
              match_result = ro64()[[11]]
            ),
            matchup(
              l$REL$D3[2,],
              l$REL$D10D[1,],
              match_result = ro64()[[12]]
            ),
            tags$ul(class = "matchup",
              team("top", l$BigO$D2[1,], winner = F, score = 1, round = 1),
              team("bottom", list(id = 2309956, name = "Snake Kittens", race_id = 5, logo = "Lizardman_18", name.1 = "Teddy Rose"), winner = T, score = 2, round = 1)
            ),
            matchup(
              l$BigO$D1[4,],
              l$Gman$D6[1,],
              match_result = ro64()[[14]]
            ),
            matchup(
              l$Gman$D1[2,],
              l$REL$D10A[2,],
              match_result = ro64()[[15]]
            ),
            matchup(
              l$Gman$D3[2,],
              l$REL$D6[2,],
              match_result = ro64()[[16]]
            )
          ),
          div(
            class = "round round-two ",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starts Oct 10")),
            tags$ul(class = "matchup",
                    team("top", l$Gman$D1[1,], winner = T, score = 2, round = 2),
                    team("bottom", l$REL$D1[4,], winner = F, score = 1, round = 2)
            )
            ,
            matchup(
              team_details(winner(ro64()[[3]])),
              team_details(winner(ro64()[[4]])),
              match_result = ro32()[[2]]
              ),
            matchup(
              team_details(winner(ro64()[[5]])),
              team_details(winner(ro64()[[6]])),
              match_result = ro32()[[3]]
            ),
            matchup(class = "mid",
                    team_details(winner(ro64()[[7]])),
                    team_details(winner(ro64()[[8]])),
                    match_result = ro32()[[4]]
                    ),
            matchup(
              l$BigO$D1[1,],
              team_details(winner(ro64()[[10]])),
              match_result = ro32()[[5]]
            ),
            matchup(
              team_details(winner(ro64()[[11]])),
              team_details(winner(ro64()[[12]])),
              match_result = ro32()[[6]]
            ),
            matchup(
              team_details(winner(ro64()[[13]])),
              team_details(winner(ro64()[[14]])),
              match_result = ro32()[[7]]
            ),
            matchup(
              team_details(winner(ro64()[[15]])),
              team_details(winner(ro64()[[16]])),
              match_result = ro32()[[8]]
            )
          ),
          div(
            class = "round round-three",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starts Oct 17")),
            matchup(
              l$Gman$D1[1,],
              team_details(winner(ro32()[[2]])),
              match_result = ro16()[[1]]
            ),
            matchup(class = "mid",
                    team_details(winner(ro32()[[3]])),
                    team_details(winner(ro32()[[4]])),
                    match_result = ro16()[[2]]
                    ),
            matchup(
              team_details(winner(ro32()[[5]])),
              team_details(winner(ro32()[[6]])),
              match_result = ro16()[[3]]
            ),
            matchup(
              team_details(winner(ro32()[[7]])),
              team_details(winner(ro32()[[8]])),
              match_result = ro16()[[4]]
            )
          ),
          div(
            class = "round round-four",
            div(class = "round-details", "Round 4", br(), span(class = "date", "Starts Oct 24")),
            matchup(class = "mid",
                    team_details(winner(ro16()[[1]])),
                    team_details(winner(ro16()[[2]])),
                    match_result = ro8()[[1]]
                    ),
            matchup(
              team_details(winner(ro16()[[3]])),
              team_details(winner(ro16()[[4]])),
              match_result = ro8()[[2]]
            )
          )
        ),
        div(
          class = "champion",
          div(
            class = "semis-l",
            div(
              class = "round-details",
              "Top Half SF",
              br(),
              span(class = "date", "Starts Oct 31")
            ),
            matchup(class = "championship",
                     team_details(winner(ro8()[[1]])),
                     team_details(winner(ro8()[[3]])),
                     match_result = sf()[[1]]
                    )
          ),
          div(
            class = "final",
            #icon("trophy"),
            div(
              class = "round-details ",
              "Magnum Cup",
              br(),
              span(class = "date", "Starts Nov 7")
            ),
            matchup(class = "championship sb current",
                     team_details(winner(sf()[[1]])),
                     team_details("Mystaes"), #team_details(winner(sf()[[2]])),
                     match_result = sb()[[1]]
                    )
          ),
          div(
            class = "semis-r ",
            div(
              class = "round-details",
              "Bottom Half SF",
              br(),
              span(class = "date", "Starts Oct 31")
            ),
            matchup(class = "championship",
                     l$REL$D6[2,],
                     l$Gman$D8E[1,],
                     list(data_frame(coach = c("Mystaes", "AndyDavo"), team = c("The Elements of Style", "Deadly Nightshade"), score = c(3,2), round = c(4,4)))  
                    )
          )
        ),
        div(
          class = "split split-two",
          div(
            class = "round round-four",
            div(class = "round-details", "Round 4", br(), span(class = "date", "Starts Oct 24")),
            matchup(class = "mid",
                    team_details(winner(ro16()[[5]])),
                    team_details(winner(ro16()[[6]])),
                    match_result = ro8()[[3]]
                    ),
            matchup(
              team_details(winner(ro16()[[7]])),
              team_details(winner(ro16()[[8]])),
              match_result = ro8()[[4]]
            )
          ),
          div(
            class = "round round-three",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starts Oct 17")),
            matchup(
              team_details(winner(ro32()[[9]])),
              team_details(winner(ro32()[[10]])),
              match_result = ro16()[[5]]
            ),
            matchup(class = "mid",
                    team_details(winner(ro32()[[11]])),
                    team_details(winner(ro32()[[12]])),
                    match_result = ro16()[[6]]
                    ),
            matchup(
              team_details(winner(ro32()[[13]])),
              team_details(winner(ro32()[[14]])),
              match_result = ro16()[[7]]
            ),
            matchup(
              team_details(winner(ro32()[[15]])),
              team_details(winner(ro32()[[16]])),
              match_result = ro16()[[8]]
            )
          ),
          div(
            class = "round round-two",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starts Oct 10")),
            matchup(
              team_details(winner(ro64()[[17]])),
              team_details(winner(ro64()[[18]])),
              match_result = ro32()[[9]]
            ),
            matchup(
              team_details(winner(ro64()[[19]])),
              team_details(winner(ro64()[[20]])),
              match_result = ro32()[[10]]
            ),
            matchup(
              team_details(winner(ro64()[[21]])),
              team_details(winner(ro64()[[22]])),
              match_result = ro32()[[11]]
              ),
            matchup(class = "mid",
                    team_details(winner(ro64()[[23]])),
                    team_details(winner(ro64()[[24]])),
                    match_result = ro32()[[12]]
                    ),
            matchup(
              team_details(winner(ro64()[[25]])),
              team_details(winner(ro64()[[26]])),
              match_result = ro32()[[13]]
            ),
            matchup(
              team_details(winner(ro64()[[27]])),
              team_details(winner(ro64()[[28]])),
              match_result = ro32()[[14]]
            ),
            matchup(
              team_details(winner(ro64()[[29]])),
              team_details(winner(ro64()[[30]])),
              match_result = ro32()[[15]]
            ),
            matchup(
              team_details(winner(ro64()[[31]])),
              l$REL$D1[1,],
              match_result = ro32()[[16]]
            )
          ),
          div(
            class = "round round-one",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starts Oct 3")),
            matchup(
              l$Gman$D3[1,],
              l$REL$D10C[1,],
              match_result = ro64()[[17]]
            ),
            matchup(
              l$REL$D2[2,],
              l$Gman$D8B[2,],
              match_result = ro64()[[18]]
            ),
            matchup(
              l$Gman$D1[3,],
              l$REL$D10E[1,],
              match_result = ro64()[[19]]
            ),
            matchup(
              l$BigO$D3[2,],
              l$REL$D4[2,],
              match_result = ro64()[[20]]
            ),
            matchup(
              l$REL$D1[2,],
              l$Gman$D8A[1,],
              match_result = ro64()[[21]]
            ),
            tags$ul(class = "matchup",
                    team("top", l$Gman$D5[1,], winner = T, score = 1, round = 1),
                    team("bottom", list(id = 2289176, name = "The Way of the Leaf", race_id = 11, logo = "Halfling_08", name.1 = "Chubberson"), winner = F, score = 0, round = 1)
            ),
            matchup(
              l$Gman$D1[4,],
              l$REL$D8[1,],
              match_result = ro64()[[23]]
            ),
            tags$ul(class = "matchup mid",
                    team("top", l$BigO$D4A[1,], winner = T, score = 3, round = 1),
                    team("bottom", list(id = 1988441, name = "The Lizzardblizzard", race_id = 15, logo = "Lizardman_09", name.1 = "GemeneRick"), winner = F, score = 2, round = 1)
            ),
            matchup(
              l$REL$D2[1,],
              l$Gman$D8E[1,],
              match_result = ro64()[[25]]
            ),
            matchup(
              l$BigO$D2[3,],
              l$Gman$D5[2,],
              match_result = ro64()[[26]]
            ),
            matchup(
              l$BigO$D3[1,],
              l$Gman$D8D[1,],
              match_result = ro64()[[27]]
            ),
            matchup(
              l$REL$D5[1,],
              l$Gman$D4[2,],
              match_result = ro64()[[28]]
            ),
            matchup(
              l$BigO$D1[3,],
              l$Gman$D8F[1,],
              match_result = ro64()[[29]]
            ),
            matchup(
              l$REL$D5[2,],
              l$Gman$D2[2,],
              match_result = ro64()[[30]]
            ),
            matchup(
              l$BigO$D2[2,],
              l$REL$D7[1,],
              match_result = ro64()[[31]]
            ),
            matchup(class = "blank") #REL1 Bye
            
          )
        )
      )
    )
  })
  
})


