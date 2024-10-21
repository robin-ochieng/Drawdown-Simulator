library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(readxl)
library(writexl)
library(janitor)
library(plyr)
library(tidyquant)
library(lubridate)
library(toOrdinal)
library(scales)
library(xkcd)
library(lifecontingencies)
library(bs4Dash)
library(bslib)
options(scipen=999)

# ILT15 Life Tables -------------------------------------------------------
cnames <- read_excel("data/ILT15.xlsx", sheet = 1, n_max = 0) %>%
    names()

life_table_female <- read_xlsx("data/ILT15.xlsx", sheet = 1, skip = 1, col_names = cnames) %>% 
    drop_na()

life_table_male <- read_xlsx("data/ILT15.xlsx", sheet = 2, skip=1, col_names = cnames) %>% 
    drop_na()

qx_female <- unlist(life_table_female[,5] * 0.5)
qx_male <- unlist(life_table_male[,5] * 0.42)

ILT15_female_reduced <- probs2lifetable(probs = qx_female, radix = 100000, type = "qx", name = "ILT15_female_reduced")
ILT15_male_reduced <- probs2lifetable(probs = qx_male, radix = 100000, type = "qx", name = "ILT15_male_reduced")
listOfTables <- list(ILT15_female_reduced, ILT15_male_reduced)

freq_list = c("Annually", "Semi-Annually", "Quarterly", "Bi-Monthly", "Monthly", "Fortnightly", "Weekly", "Daily")
p_list = c(1, 2, 4, 6, 12, 26, 52, 365)

# Rounding to 2 Decimal Places --------------------------------------------
round_2d <- function(x, two_d = F){
    if(two_d == F) {
        if(round(as.numeric(x), 1)%%1 == 0){
          return(format(round(as.numeric(x), 0), nsmall = 0, big.mark = ",", scientific=FALSE))
        } else if ((10*round(as.numeric(x), 2))%%1 == 0){
          return(format(round(as.numeric(x), 1), nsmall = 1, big.mark = ",", scientific=FALSE))
        } 
    }
    return(format(round(as.numeric(x), 2), nsmall = 2, big.mark = ",", scientific=FALSE))
}

# Define a custom theme using bslib
my_theme <- bs_theme(
  bootswatch = "united", #'flatly', 'sandstone', 'darkly', 'yeti', 'cosmo', 'lumen', 'minty',  'lux', 'pulse', 'slate', 'solar', 'spacelab', 'superhero', 'united', 'vapor'
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  success = "#4CAF50",   # Optionally customize success color
  info = "#2196F3",      # Optionally customize info color
  warning = "#FFC107",   # Optionally customize warning color
  danger = "#F44336",    # Optionally customize danger color
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333", 
  navbar_fg = "#ffffff"
)

ui <- fluidPage(
  theme = my_theme,
  useShinyjs(),
  tags$head(
    includeCSS("www/css/custom_styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
    tags$link(rel = "shortcut icon", href = "favicon/kenbright2.ico", type = "image/x-icon")
  ),
  br(),
  br(),
  source("modules/sorpCalculatorUI.R", local = TRUE)[1],
  bs4DashFooter(
    div(style = "background-color: #202123; color: white; text-align: center; padding: 8px;", 
        "© 2024 SORP Calculator | Powered by Robin")
  )
)

# Define server logic
server <- function(input, output, session) {
  source("modules/functions.R", local = TRUE)[1]
  source("modules/sorpCalculatorServer.R", local = TRUE)[1]
}

# Run the application
shinyApp(ui = ui, server = server)
