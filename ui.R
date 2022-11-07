# Load packages
library(tidyverse)
library(shinyWidgets)
library(shiny)
library(htmltools)
library(shinycssloaders)
library(viridis)
library(lubridate)
library(reactable)
library(scales)
library(data.table)
library(shinythemes)
library(tippy)
library(gt)

# Source utilities
source("utilities.R")
# Source modules
source("modules/module_home.R")
source("modules/module_table.R")

## UI ## 
fluidPage(
  theme = shinytheme(theme = "yeti"),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
  navbarPage(
    collapsible = TRUE,
    title = "NBA Coaching Challenges",
    position = "fixed-top",
    tabPanel(
      "Home",
      htmltools::br(),
      home_ui("home_page")
    ),
    tabPanel(
      "Coach's Challenge DB",
      htmltools::br(),
      table_ui("table_tab")
    )
  )
)

