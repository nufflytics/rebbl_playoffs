
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  shiny::htmlTemplate(
    "template.html",
    bracket = uiOutput("bracket")
    )
  
)
