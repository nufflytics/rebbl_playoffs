library(shiny)
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

shinyServer(function(input, output) {
  
  l <- readRDS("data/ladders.rds")
  
  tdiff <- as.integer(lubridate::ymd_hms(file.info("data/ladders.rds")$mtime) - lubridate::now()) + 1
  
  if(tdiff<2) {tdiff = paste0(tdiff, " hour")}
  
  if(tdiff>1) {tdiff = paste0(tdiff, " hours")}
  
  output$tdiff <- renderUI(p(style = "font-size:30%; letter-spacing: 2px;", paste("Positions updated", tdiff, "ago")))
  
  team <- function(pos, team) {
    if(is.null(team)) {return(tags$li(class = glue("team team-{pos}"), HTML("&nbsp;")))}
    
    a(href = glue("http://bb2leaguemanager.com/Leaderboard/team_detail.php?team_id={team$id}&community_id=10"),
      target="_blank",
      "data-tooltip" = team$name,
      tags$li(
        class =  glue("team team-{pos}"),
        img(src = glue("img/{race_img(team$race)}"), height = 25),
        img(src = glue("http://images.bb2.cyanide-studio.com/logos/Logo_{team$logo}.png"), height = 25),
        #team$name,
        span(class = "score", team$name.1)
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
  
  matchup <- function(team1 = NULL, team2=NULL, class = NULL) {
    tags$ul(class = paste("matchup", class),
            team("top", team1),
            team("bottom", team2)
    )
  }
  
  output$bracket <- renderUI({
    tags$section(
      id = "bracket",
      div(
        class = "container",
        div(
          class = "split split-one",
          div(
            class = "round round-one current",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starting May 23")),
            matchup(class = "blank"), # RELD1 Bye
            matchup(
              l$Gman$D3[1,],
              l$REL$D6[2,]
            ),
            matchup(
              l$BigO$D1[4,],
              l$Gman$D6A[1,]
            ),
            tags$ul(class ="matchup",
                    team("top", l$REL$D4[1,]),
                    custom_team("bottom", "Best Stunty")
            ),
            matchup(
              l$Gman$D3[3,],
              l$REL$D1[5,]
            ),
            matchup(
              l$Gman$D2[1,],
              l$REL$D9C[1,]
            ),
            matchup(
              l$BigO$D2[1,],
              l$REL$D9E[1,]
            ),
            matchup(
              l$BigO$D1[2,],
              l$Gman$D6B[1,]
            ),
            matchup(
              l$REL$D1[2,],
              l$BigO$D4A[1,]
            ),
            matchup(
              l$REL$D7[1,],
              l$Gman$D3[2,]
            ),
            matchup(
              l$Gman$D1[5,],
              l$BigO$D2[3,]
            ),
            matchup(
              l$Gman$D5[1,],
              l$REL$D7[2,]
            ),
            matchup(
              l$REL$D9D[1,],
              l$Gman$D1[2,]
            ),
            matchup(
              l$REL$D4[2,],
              l$Gman$D2[3,]
            ),
            matchup(
              l$BigO$D1[5,],
              l$REL$D3[3,]
            ),
            tags$ul(class ="matchup",
                    team("top", l$REL$D5[1,]),
                    custom_team("bottom", "Minors Champion")
            )
          ),
          div(
            class = "round round-two",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starting May 30")),
            matchup(
              l$REL$D1[1,],
              NULL
            ),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup()
          ),
          div(
            class = "round round-three",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starting June 6")),
            matchup(),
            matchup(),
            matchup(),
            matchup()
          ),
          div(
            class = "round round-four",
            div(class = "round-details", "Round 4", br(), span(class = "date", "Starting June 13")),
            matchup(),
            matchup()
          )
        ),
        div(
          class = "champion",
          div(
            class = "semis-l",
            div(
              class = "round-details",
              "SF #1",
              br(),
              span(class = "date", "Starting June 20")
            ),
            matchup(class = "championship")
          ),
          div(
            class = "final",
            icon("trophy"),
            div(
              class = "round-details",
              "Superbowl",
              br(),
              span(class = "date", "Starting June 27")
            ),
            matchup(class = "championship")
          ),
          div(
            class = "semis-r",
            div(
              class = "round-details",
              "SF #2",
              br(),
              span(class = "date", "Starting June 20")
            ),
            matchup(class = "championship")
          )
        ),
        div(
          class = "split split-two",
          div(
            class = "round round-four",
            div(class = "round-details", "Round 4", br(), span(class = "date", "Starting June 13")),
            matchup(),
            matchup()
          ),
          div(
            class = "round round-three",
            div(class = "round-details", "Round 3", br(), span(class = "date", "Starting June 6")),
            matchup(),
            matchup(),
            matchup(),
            matchup()
          ),
          div(
            class = "round round-two",
            div(class = "round-details", "Round 2", br(), span(class = "date", "Starting May 30")),
            matchup(
              l$BigO$D1[1,],
              NULL
            ),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup(),
            matchup(
              NULL,
              l$Gman$D1[1,]
            )
          ),
          div(
            class = "round round-one current",
            div(class = "round-details", "Round 1", br(), span(class = "date", "Starting May 23")),
            matchup(class = "blank"), # BigOD1 Bye
            matchup(
              l$REL$D3[1,],
              l$BigO$D3[2,]
            ),
            matchup(
              l$Gman$D1[4,],
              l$REL$D9A[1,]
            ),
            matchup(
              l$REL$D2[1,],
              l$Gman$D6D[1,]
            ),
            matchup(
              l$Gman$D1[3,],
              l$REL$D9B[1,]
            ),
            matchup(
              l$REL$D8[2,],
              l$Gman$D2[2,]
            ),
            matchup(
              l$Gman$D4[1,],
              l$REL$D8[1,]
            ),
            matchup(
              l$REL$D1[4,],
              l$Gman$D6E[1,]
            ),
            matchup(
              l$REL$D2[3,],
              l$REL$D3[2,]
            ),
            matchup(
              l$BigO$D4B[1,],
              l$REL$D1[3,]
            ),
            matchup(
              l$REL$D5[2,],
              l$BigO$D2[2,]
            ),
            matchup(
              l$REL$D6[1,],
              l$Gman$D4[2,]
            ),
            matchup(
              l$BigO$D1[3,],
              l$Gman$D6C[1,]
            ),
            matchup(
              l$REL$D2[2,],
              l$REL$D4[3,]
            ),
            matchup(
              l$BigO$D3[1,],
              l$Gman$D5[2,]
            ),
            matchup(class = "blank") # GmanD1 Bye
          )
        )
      )
    )
  })
  
})
