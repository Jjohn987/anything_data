library(tidyverse)
library(haloR)
library(ggrepel)
library(lubridate)
library(cowplot)
library(magick)
library(shiny)
library(shinythemes)
library(grid)
library(colourpicker)

## Load Background/Map metadata
url <- "https://twitter.com/share?ref_src=twsrc%5Etfw/"
url2 <- "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fjjohn9000.shinyapps.io%2FHalo5_Stats%2F&amp;src=sdkpreparse"
maps <- read.csv("maps.csv", stringsAsFactors = FALSE)[-c(1, 3)]


ui <- fluidPage(theme = shinytheme("slate"),

                tags$head(includeCSS("test_css.css")),

                tags$div(h1("Halo 5 Stats"), align = "center"),
                
                tags$div(h6("Download your SPARTAN and GAME STATS using Microsoft Halo 5 API", align = "right")),
 ##Javascript to read window size for dynamic plot specs
                sidebarLayout(
                          sidebarPanel(width = 3, 
                                       tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '),
                                                 tags$script("<div id='fb-root'></div>
                                                               <script>(function(d, s, id) {
                                                                 var js, fjs = d.getElementsByTagName(s)[0];
                                                                 if (d.getElementById(id)) return;
                                                                 js = d.createElement(s); js.id = id;
                                                                 js.src = 'https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.1';
                                                                 fjs.parentNode.insertBefore(js, fjs);
                                                               }(document, 'script', 'facebook-jssdk'));</script>")),
                    
                    tags$div(h5("Search"), align = "center"),
                    ##Global Input values
                    textAreaInput("player",
                                 HTML("Gamertag - <font size = 1>No trailing spaces <br> Use <font size = 3>+</font> for names with spaces </font>"),
                                  placeholder = "e.g., Hairball9000", rows = 1),
                    tags$div(actionButton("submitbutton", HTML("<font size = 1>Search </font>"), width = '30%'), align = "center"),
                    
                    selectInput("mode", "Game Mode",
                                choices = c("arena", "campaign", "warzone", "custom"),
                                selected = "arena"),
                    selectInput("map", "Background",
                                choices = maps$name,
                                selected = "Guardians"),
                    
                    selectInput("colors", "Chart Color", 
                                choices = c("Blues", "Greens", "Greys", "Oranges", "Oranges2" = "YlOrBr","Pinks" = "PuRd", "Pinks2" = "RdPu", "Purples", 
                                            "Reds", "Blue-Green" = "GnBu", "Green-Blue" = "PuBuGn", "Green-Yellow" = "YlGn",
                                            "Purple-Blue" = "BuPu", 
                                            "Red-Orange-Yellow" = "YlOrRd", "Blue-Green-Yellow" = "YlGnBu"), 
                                selected = "Blues"),
                    
                    
                    colourInput("col", "Chart Outline Color", "white"),
                    tags$div(h4("Made in R with Shiny"), align = "center"),
                    tags$div(tags$div("By Jeremy Johnson", class = "nametag"),
                    tags$br(),
                    tags$div(style = "display:inline-block", tags$a(tags$img(src = "images/flogo_RGB_HEX-144.png", style = "margin:0 10 px; width:36px"), href = url2)),
                    tags$div(style="display:inline-block", tags$a(tags$img(src ="images/Twitter_Social_Icon_Circle_Color.png", style="margin:0 10px; width:36px"), href=url)), align = "center"), 
                    includeScript("http://platform.twitter.com/widgets.js")),
                  
##Individual Plots and Local Plot Specs                   
                  mainPanel(
                    tabsetPanel(
                    tabPanel("Compiled", div(plotOutput("consolidated"), class = "well-plotpanel"),
                             tags$div(downloadButton('downloadPlot', 'Download Plot'), align = "right"),

                             column(3, 
                                    tags$div(h4("Spartan"),
                                             tags$div(
                                               sliderInput("spartanposx", "X-Axis", min = -1, max = 1,
                                                           value = -.17, step = .01, ticks = FALSE),
                                               sliderInput("spartanposy", "Y-Axis", min = -1, max = 1,
                                                           value = -.02, step = .01, ticks = FALSE, label),
                                               sliderInput("spartanscale", "Scale", min = .1, max = 2,
                                                           value = 0.95, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Emblem"),
                                      tags$div(
                                        sliderInput("emblemposx", "X-Axis", min = -.75, max = .75,
                                                    value = .26, step = .01, ticks = FALSE),
                                        sliderInput("emblemposy", "Y-Axis", min = -.75, max = .75,
                                                    value = .01, step = .01, ticks = FALSE),
                                        sliderInput("emblemscale", "Scale", min = .10, max = 1,
                                                    value = .59, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Title Text"),
                                      tags$div(
                                        sliderInput("textposx", "X-Axis", min = -.75, max = .75,
                                                    value = .5, step = .01, ticks = FALSE),
                                        sliderInput("textposy", "Y-Axis", min = -1, max = 1,
                                                    value = .98, step = .01, ticks = FALSE),
                                        sliderInput("textsize", "Scale", min = .10, max = 100,
                                                    value = 50, step = 1, ticks = FALSE), align = "left"), align = "center", class = "well"))),
                    
                    
                    
                    tabPanel("Shot Accuracy", div(plotOutput("shootingaccuracy"), class = "well-plotpanel"),
                             tags$div(downloadButton('downloadPlot1', 'Download Plot'), align = "right"),

                                column(3, 
                                  div(
                                    div(h4("Spartan"), align = "center"),
                                    
                                        sliderInput("spartanposx1", "X-Axis", min = -.75, max = .75,
                                                    value = 0.33, step = .01, ticks = FALSE),
                                        sliderInput("spartanposy1", "Y-Axis", min = -1, max = 1,
                                                    value = .02, step = .01, ticks = FALSE),
                                        sliderInput("spartanscale1", "Scale", min = .1, max = 2,
                                                    value = 1.07, step = .01, ticks = FALSE), class = "well")),
                             

                            column(3,
                                   div(h4(div("Emblem", align = "center")),
                                       sliderInput("emblemposx1", "X-Axis", min = -.75, max = .75,
                                                    value = 0.11, step = .01, ticks = FALSE),
                                        sliderInput("emblemposy1", "Y-Axis", min = -.75, max = .75,
                                                    value = -.41, step = .01, ticks = FALSE),
                                        sliderInput("emblemscale1", "Scale", min = .10, max = 2,
                                                    value = .23, step = .01, ticks = FALSE), class = "well")),
                             

                            column(3,
                                   div(
                                    div(h4("Chart"), align = "center"),
                                        sliderInput("chartposx1", "X-Axis", min = -.75, max = .75,
                                                    value = -0.18, step = .01, ticks = FALSE),
                                        sliderInput("chartposy1", "Y-Axis", min = -.75, max = .75, 
                                                    value = -0.08, step = .01, ticks = FALSE),
                                        sliderInput("chartscale1", "Scale", value = 1.18, min = 0.25, max = 2.0, step = .01, ticks = FALSE), class = "well")),

                            column(3,
                                   div(
                                     div(h4("Legend"), align = "center"),
                                        
                                        sliderInput("legendposx1", "X-Axis", min = -1, max = 1,
                                                    value = .14, step = .01, ticks = FALSE),
                                        sliderInput("legendposy1", "Y-Axis", min = -1, max = 1,
                                                    value = 0.38, step = .01, ticks = FALSE),
                                     selectInput("legenddirection1", "Direction",
                                                 choices = c("vertical", "horizontal"), 
                                                 selected = "horizontal"), class = "well"))
                            ),


                    
                    tabPanel("Kill Style", div(plotOutput("killstyles"), class = "well-plotpanel"),
                             
                             tags$div(downloadButton('downloadPlot2', 'Download Plot'), align = "right"),
                             column(3,
                                    tags$div(
                                      h4("Spartan"),
                                      tags$div(
                                        sliderInput("spartanposx2", "X-Axis", min = -.75, max = .75,
                                                    value = .24, step = .01, ticks = FALSE),
                                        sliderInput("spartanposy2", "Y-Axis", min = -1, max = 1,
                                                    value = -.01, step = .01, ticks = FALSE),
                                        sliderInput("spartanscale2", "Scale", min = .1, max = 2,
                                                    value = 1.0, step = .01, ticks = FALSE),  align = "left"), align = "center", class = "well")),
                             
                             column(3, 
                                    tags$div(
                                      h4("Emblem"),
                                      tags$div(
                                        sliderInput("emblemposx2", "X-Axis", min = -.75, max = .75,
                                                    value = -0.4, step = .01, ticks = FALSE),
                                        sliderInput("emblemposy2", "Y-Axis", min = -.75, max = .75,
                                                    value = -0.4, step = .01, ticks = FALSE),
                                        sliderInput("emblemscale2", "Scale", min = .10, max = 2,
                                                    value = .3, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Chart"),
                                      tags$div(
                                        sliderInput("chartposx2", "X-Axis", min = -1, max = 1,
                                                    value = -0.06, step = .01, ticks = FALSE),
                                        sliderInput("chartposy2", "Y-Axis", min = -.75, max = .75,
                                                    value = 0, step = .01, ticks = FALSE),
                                        sliderInput("chartscale2", "Scale", min = .25, max = 2.0, 
                                                    value = .85, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Legend"),
                                      tags$div(
                                        sliderInput("legendposx2", "X-Axis", min = -1, max = 1,
                                                    value = -0.11, step = .01, ticks = FALSE),
                                        sliderInput("legendposy2", "Y-Axis", min = -1, max = 1,
                                                    value = -0.86, step = .01, ticks = FALSE),
                                        selectInput("legenddirection2", "Direction", 
                                                    choices = c("vertical", "horizontal"), 
                                                    selected = "horizontal"), align = "left"),
                                        align = "center", class = "well"))),
                    
                    tabPanel("KDA Ratio", div(plotOutput("kda"), class = "well-plotpanel"),
                             
                             tags$div(downloadButton('downloadPlot3', 'Download Plot'), align = "right"),
                             column(3,
                                    tags$div(
                                      h4("Spartan"),
                                      tags$div(
                                        sliderInput("spartanposx3", "X-Axis", min = -.75, max = .75,
                                                    value = -.14, step = .01, ticks = FALSE),
                                        sliderInput("spartanposy3", "Y-Axis", min = -1, max = 1,
                                                    value = -0.14, step = .01, ticks = FALSE),
                                        sliderInput("spartanscale3", "Scale", min = -1, max = 2,
                                                    value = 1.6, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3, 
                                    tags$div(
                                      h4("Emblem"),
                                      tags$div(
                                        sliderInput("emblemposx3", "X-Axis", min = -.75, max = .75,
                                                    value = -.75, step = .01, ticks = FALSE),
                                        sliderInput("emblemposy3", "Y-Axis", min = -.75, max = .75,
                                                    value = .38, step = .01, ticks = FALSE),
                                        sliderInput("emblemscale3", "Scale", min = .10, max = 2,
                                                    value = .3, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3, 
                                    tags$div(
                                      h4("Chart"),
                                      tags$div(
                                        sliderInput("chartposx3", "X-Axis", min = -.75, max = .75,
                                                    value = .22, step = .01, ticks = FALSE),
                                        sliderInput("chartposy3", "Y-Axis", min = -.75, max = .75,
                                                    value = -.05, step = .01, ticks = FALSE),
                                        sliderInput("chartscale3", "Scale", value = 1.25, min = .25, max = 2.0, step = .01, ticks = FALSE),
                                        align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Legend"),
                                      tags$div(
                                        sliderInput("legendposx3", "X-Axis", min = -1, max = 1,
                                                    value = .76, step = .01, ticks = FALSE),
                                        sliderInput("legendposy3", "Y-Axis", min = -1, max = 1,
                                                    value = .4, step = .01, ticks = FALSE),
                                      selectInput("legenddirection3", "Direction", 
                                                  choices = c("vertical", "horizontal"), 
                                                  selected = "vertical"),  align = "left"), 
                                      align = "center", class = "well"))),
                    
                    tabPanel("Win:Loss Ratio", div(plotOutput("winlost"), class = "well-plotpanel"),
                             
                             tags$div(downloadButton('downloadPlot4', 'Download Plot'), align = "right"),

                             column(3,
                                    tags$div(
                                      h4("Spartan"),
                                      tags$div(
                                        sliderInput("spartanposx4", "X-Axis", min = -.75, max = .75,
                                                    value = .27, step = .01, ticks = FALSE),
                                        sliderInput("spartanposy4", "Y-Axis", min = -1, max = 1,
                                                    value = .01, step = .01, ticks = FALSE),
                                        sliderInput("spartanscale4", "Scale", min = -1, max = 2,
                                                    value = 1.0, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3, 
                                    tags$div(
                                      h4("Emblem"),
                                      tags$div(
                                        sliderInput("emblemposx4", "X-Axis", min = -.75, max = .75,
                                                    value = -.26, step = .01, ticks = FALSE),
                                        sliderInput("emblemposy4", "Y-Axis", min = -.36, max = .75,
                                                    value = .18, step = .01, ticks = FALSE),
                                        sliderInput("emblemscale4", "Scale", min = .10, max = 1,
                                                    value = .34, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3, 
                                    tags$div(
                                      h4("Chart"),
                                      tags$div(
                                        sliderInput("chartposx4", "X-Axis", min = -.75, max = .75,
                                                    value = -.13, step = .01, ticks = FALSE),
                                        sliderInput("chartposy4", "Y-Axis", min = -.75, max = .75,
                                                    value = -.05, step = .01, ticks = FALSE),
                                        sliderInput("chartscale4", "Scale", min = .25, max = 1.0, 
                                                    value = .59, step = .01, ticks = FALSE), align = "left"), align = "center", class = "well")),
                             column(3,
                                    tags$div(
                                      h4("Legend"),
                                      tags$div(
                                        sliderInput("legendposx4", "X-Axis", min = -1, max = 1,
                                                    value = -0.01, step = .01, ticks = FALSE),
                                        sliderInput("legendposy4", "Y-Axis", min = -1, max = 1,
                                                    value = -0.9, step = .01, ticks = FALSE),
                                      selectInput("legenddirection4", "Direction", 
                                                  choices = c("vertical", "horizontal"), 
                                                  selected = "horizontal"), align = "left"),
                                        align = "center", class = "well"))))
                  )
                )
)
                  
                  
                


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  ##Api Call arguments
  
  key <- "3ac802c56aa44f8cae8ac1bd0417ad63"
  
  player <- eventReactive(input$submitbutton, {
    input$player
  })
  
  ## Make inputs reactive
  mode <- reactive({req(input$mode)})
  mode_title <- reactive({str_to_title(mode())})
  
  selected_map <- reactive({input$map})
  
  
  colors <- reactive({as.character(req(input$colors))})
  linecolor <- reactive(req(input$col))
  
  textposx <- reactive({as.numeric(req(input$textposx))})
  textposy <- reactive({as.numeric(req(input$textposy))})
  textsize <- reactive({as.numeric(req(input$textsize))})
  
  chartposx1 <- reactive({as.numeric(req(input$chartposx1))})
  chartposx2 <- reactive({as.numeric(req(input$chartposx2))})
  chartposx3 <- reactive({as.numeric(req(input$chartposx3))})
  chartposx4 <- reactive({as.numeric(req(input$chartposx4))})
  
  chartposy1 <- reactive({as.numeric(req(input$chartposy1))})
  chartposy2 <- reactive({as.numeric(req(input$chartposy2))})
  chartposy3 <- reactive({as.numeric(req(input$chartposy3))})
  chartposy4 <- reactive({as.numeric(req(input$chartposy4))})
  
  chartscale1 <- reactive({as.numeric(req(input$chartscale1))})
  chartscale2 <- reactive({as.numeric(req(input$chartscale2))})
  chartscale3 <- reactive({as.numeric(req(input$chartscale3))})
  chartscale4 <- reactive({as.numeric(req(input$chartscale4))})
  
  spartanposx <- reactive({as.numeric(req(input$spartanposx))})
  spartanposx1 <- reactive({as.numeric(req(input$spartanposx1))})
  spartanposx2 <- reactive({as.numeric(req(input$spartanposx2))})
  spartanposx3 <- reactive({as.numeric(req(input$spartanposx3))})
  spartanposx4 <- reactive({as.numeric(req(input$spartanposx4))})
  
  spartanposy <-  reactive({req(input$spartanposy)})
  spartanposy1 <- reactive({req(input$spartanposy1)})
  spartanposy2 <- reactive({req(input$spartanposy2)})
  spartanposy3 <- reactive({req(input$spartanposy3)})
  spartanposy4 <- reactive({req(input$spartanposy4)})
  
  spartanscale <-  reactive({req(input$spartanscale)})
  spartanscale1 <-  reactive({req(input$spartanscale1)})
  spartanscale2 <-  reactive({req(input$spartanscale2)})
  spartanscale3 <-  reactive({req(input$spartanscale3)})
  spartanscale4 <-  reactive({req(input$spartanscale4)})
  
  emblemposx <- reactive({as.numeric(req(input$emblemposx))})
  emblemposx1 <- reactive({as.numeric(req(input$emblemposx1))})
  emblemposx2 <- reactive({as.numeric(req(input$emblemposx2))})
  emblemposx3 <- reactive({as.numeric(req(input$emblemposx3))})
  emblemposx4 <- reactive({as.numeric(req(input$emblemposx4))})
  
  emblemposy <- reactive({as.numeric(req(input$emblemposy))})
  emblemposy1 <- reactive({as.numeric(req(input$emblemposy1))})
  emblemposy2 <- reactive({as.numeric(req(input$emblemposy2))})
  emblemposy3 <- reactive({as.numeric(req(input$emblemposy3))})
  emblemposy4 <- reactive({as.numeric(req(input$emblemposy4))})
  
  emblemscale <- reactive({as.numeric(req(input$emblemscale))})
  emblemscale1 <- reactive({as.numeric(req(input$emblemscale1))})
  emblemscale2 <- reactive({as.numeric(req(input$emblemscale2))})
  emblemscale3 <- reactive({as.numeric(req(input$emblemscale3))})
  emblemscale4 <- reactive({as.numeric(req(input$emblemscale4))})
  
  killstylelegendx <- reactive({req(input$killstylelegendx)})
  killstylelegendy <- reactive({req(input$killstylelegendy)})
  killstylescale <- reactive({req(input$killstylescale)})
  
  legendposx1 <- reactive({req(input$legendposx1)})
  legendposy1 <- reactive({req(input$legendposy1)})
  legenddirection1 <- reactive({req(input$legenddirection1)})
  
  legendposx2 <- reactive({req(input$legendposx2)})
  legendposy2 <- reactive({req(input$legendposy2)})
  legenddirection2 <- reactive({req(input$legenddirection2)})
  
  legendposx3 <- reactive({req(input$legendposx3)})
  legendposy3 <- reactive({req(input$legendposy3)})
  legenddirection3 <- reactive({req(input$legenddirection3)})
  
  legendposx4 <- reactive({req(input$legendposx4)})
  legendposy4 <- reactive({req(input$legendposy4)})
  legenddirection4 <- reactive({req(input$legenddirection4)})
  

  ##Api Call functions
  
  service_record <- reactive({h5_ServiceRecord(players = player(), key = key, mode = mode())})
  emblem <- reactive({h5_ProfileEmblem(key = key, player = player(), size = 256)})
  spartan <- reactive({h5_ProfileSpartan(player = player(), key = key, size = 512)})
  
  dimension_x <- reactive({input$dimension[1]})
  base_width <- reactive({((dimension_x()/5)*3)/75})
  base_height <- 400/75
  ##Read images
  
  emblem_img <- reactive({image_read(emblem())})
  spartan_img <- reactive({image_read(spartan())})
  
  background <- reactive({image_read(paste("Halo_maps/", selected_map(), ".jpg", sep = ""))})

  ## Define function to subset API data according to mode
  
  access_service_record <- reactive({
    
    switch(mode(),
           
           campaign = service_record()$Results$Result$CampaignStat,
           arena =  service_record()$Results$Result$ArenaStats,
           warzone = service_record()$Results$Result$WarzoneStat,
           custom = service_record()$Results$Result$CustomStat)
    
  })
  
  
  ##Define lookup vectors for dataframes 
  
  shooting_vars <- c("TotalKills", "TotalHeadshots", "TotalShotsFired", "TotalShotsLanded")
  
  shooting_lut <- c("TotalKills" = "Kills", "TotalHeadshots" = "Headshots", "TotalShotsFired" = "Fired", "TotalShotsLanded" = "Landed")
  
  
  kill_vars <- c("TotalMeleeKills", "TotalAssassinations", "TotalGroundPoundKills",
                 
                 "TotalShoulderBashKills", "TotalPowerWeaponKills", "TotalGrenadeKills",
                 
                 "TotalKills")
  
  kill_lut <- c("TotalMeleeKills" = "Melee", "TotalAssassinations" = "Assasinations",  "TotalGroundPoundKills" = "Ground Pound",
          "TotalShoulderBashKills" = "Shoulder Bash",  "TotalPowerWeaponKills" = "Power Weapon", "TotalGrenadeKills" = "Grenade",
          "TotalKills" = "Total")
  
  wins_losses <- c("TotalGamesWon", "TotalGamesLost", "TotalGamesTied")
  
  lut2 <- c("TotalGamesWon" = "Won", "TotalGamesLost" = "Lost", "TotalGamesTied" = "Tied")
  
  
  KDA_vars <- c("TotalDeaths", "TotalAssists", "TotalSpartanKills")
  
  lut3 <- c("TotalDeaths" = "Deaths", "TotalAssists" = "Assists", "TotalSpartanKills" = "Spartan Kills")
  
  
  
  ##****************************************************************************##
  
  ##****************************************************************************##
  
  ##                              MAKE DATA FRAMES
  
  ##****************************************************************************##
  
  ##****************************************************************************##
  
  ##DATAFRAME 1 - SHOOTING ACCURACY DATAFRAME
  
  shooting_data <- reactive({
    shooting_stats <- access_service_record()[shooting_vars] %>%
      unlist() %>%
      data.frame(stringsAsFactors = FALSE) %>%
      rownames_to_column() %>%
      set_names(c("Shots", "value")) %>%
      mutate(Shots = shooting_lut[Shots]) %>%
      mutate(value = as.numeric(value)) %>%
      arrange(-value) %>%
      mutate(Shots = factor(Shots, Shots))
    
  })
  
  
  ##DATAFRAME 2 - KILL STYLE DATAFRAME
  
  kill_styles <- reactive({access_service_record()[kill_vars]})
  
  kill_style <- reactive({
    
    ##Rename variable names, consider subsetting for optimized speed
    
    kill_style1 <- kill_styles() %>%
      gather(Kill_Type, value, 1:6) %>%
      mutate(Kill_Type = kill_lut[Kill_Type],
             value = as.numeric(value))
    
    ##API doesn't include main weapon kills
    
    ##So add up all all kills, subtract from total kills to get main weapon kills.
    
    kill_sums <- kill_style1 %>%
      summarise(sum = sum(value), real_total = unique(TotalKills))
    
    if(kill_sums[1] != kill_sums[2]) {
      
      unnacounted_difference <- kill_sums[[2]] - kill_sums[[1]]
      
      Kill_Type <- "Main Weapon"
      value <- unnacounted_difference
      TotalKills <- unique(kill_sums$real_total)
      
      filler_row <- data.frame(TotalKills, Kill_Type, value, stringsAsFactors = FALSE)
      
      kill_sums <-  kill_style1 %>%
        bind_rows(filler_row) 
      
    }
    
    kill_sums %>%
      filter(value > 0) %>% 
      arrange(-value) %>%
      mutate(Kill_Type = factor(Kill_Type, Kill_Type))
  })
  
  ##DATAFRAME 3 - WIN:LOST RATIO DATAFRAME
  
  winlost <- reactive({
    
    winlost <- access_service_record()[wins_losses] %>%
      mutate(Win_Loss = signif(TotalGamesWon/TotalGamesLost, 3), 
             Win_Loss = ifelse(Win_Loss == Inf, "Undefeated", Win_Loss)) %>%
      gather(category, value, 1:3) %>%
      mutate(value = as.numeric(value),
             category = lut2[category],
             percentage = signif(value/sum(value)*100, 3)) %>%
      filter(value > 0) 
    
    ##If length 0, impute placeholding values
    
    if(nrow(winlost) == 0) {
      winlost[1,2] <- "None" 
      winlost[1,3] <- 0.001
      winlost[1,4] <- 0
    }
    
    winlost %>%
      arrange(value) %>%
      mutate(category = factor(category, category))
  })
  
  ##DATAFRAME 4 - KILL:DEATH RATIO DATAFRAME
  
  KDA_df <- reactive({
    
    KDA_df <- access_service_record()[KDA_vars]
    KDA_df <- KDA_df %>%
      mutate(KillDeathRatio = signif(ifelse(TotalDeaths == 0, TotalSpartanKills, TotalSpartanKills/TotalDeaths),2)) %>%
      gather(var, value, 1:3) %>%
      mutate(var = lut3[var],
             value = as.numeric(value)) %>% 
      filter(value > 0) %>%
      arrange(value) %>%
      mutate(var = factor(var, var))
    
  })
  
  
  ##****************************************************************************##
  
  ##****************************************************************************##
  
  ##                              MAKE PLOTS
  
  ##****************************************************************************##
  
  ##****************************************************************************##
  
  ##Plot 1 - Shooting Accuracty Plot (Single and Consolidated)
  
  shooting_accuracy_plot_consolidated <- reactive({
    
    shooting_data() %>%
      ggplot(aes(x = Shots, y = value, fill = Shots)) +
      geom_col(alpha = .8, color = linecolor()) +
      geom_label(aes(label = value), color = "black", fill = "white", size = 4, alpha = 0.5, position = position_stack(vjust = 0.6), 
                 show.legend = FALSE) +
      scale_fill_brewer(palette = colors(), direction = -1) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position = c(0, .75),
            legend.background = element_rect(color = "white", fill=alpha("white", 0.5)),
            legend.direction = "vertical") +
      labs(x = NULL, y = NULL) +
      coord_flip() +
      scale_y_reverse()
    
  })
  
  shooting_accuracy_single <- function() {
    
    base_plot <- shooting_data() %>%
      ggplot(aes("", y = value, fill = Shots, color = linecolor())) +
      geom_bar(stat = "identity", width = .5, color = linecolor(), alpha = .8) +
      scale_fill_brewer(palette = colors(), direction = -1) +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            legend.title = element_text(face = "bold"),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "top",
            legend.background = element_rect(color = "black", fill=alpha("white", 0.5)),
            legend.direction = legenddirection1()) +
      labs(x = NULL, y = NULL) +
      coord_polar(theta = "y", direction = 1) +
      geom_label_repel(aes(label = value), fill = "white", color = "black", size = 4, alpha = 0.5, position = position_identity(), 
                       show.legend = FALSE) 
    
    
    shooting_accuracy_legend <- get_legend(base_plot)
    base_plot_legendless <- base_plot + theme(legend.position = "none")
    ##Layer in spartan, emblem, and reactive chart
    
    ggdraw() +
      
      draw_image(background(), scale = 1.3) +
      draw_image(emblem_img(), x = emblemposx1(), y = emblemposy1(), scale = emblemscale1()) +
      draw_image(spartan_img(), x = spartanposx1(), y = spartanposy1(), scale = spartanscale1()) +
      draw_plot(base_plot_legendless, x = chartposx1(), y = chartposy1(), scale = chartscale1()) +
      draw_plot(shooting_accuracy_legend, x = legendposx1(), y = legendposy1())
    
  }
  
  
  ##Plot 2 - Kill Styles Plot Single and Consolidated
  
  
  kill_styles_single <- function() {
    
    base_plot2 <- kill_style() %>%
      ggplot(aes(x = Kill_Type, y = value, fill = Kill_Type, color = linecolor())) + 
      labs(x = NULL, y = NULL) +
      geom_bar(stat = "identity", color = linecolor(), alpha = .8) +
      scale_fill_brewer(palette = colors(), direction = -1) +
      theme(panel.grid.minor = element_blank(),
            legend.position = c(.4, .9),
            legend.direction = legenddirection2(),
            legend.background = element_rect(fill=alpha("white", 0.5)),
            legend.title = element_text(face = "bold"),
            axis.text.x = element_text(color = "white", hjust = -.05, angle = 315),
            axis.ticks.x = element_line(color = linecolor()),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_line(color = linecolor()),
            axis.line.x = element_line(color = linecolor())) +
      geom_label(aes(label = value), fill = "white", color = "black", show.legend = FALSE, size = 4, alpha = .5, position = position_stack(vjust = 0.6)) 
    

           
    base_plot2_legend <- get_legend(base_plot2)
    
    base_plot2 <- base_plot2 + theme(legend.position = "none")
    
    
    ggdraw() +
      
      draw_image(background(), scale = 1.3) +
      draw_image(emblem_img(), x = emblemposx2(), y = emblemposy2(), scale = emblemscale2()) +
      draw_image(spartan_img(), x = spartanposx2(), y = spartanposy2(), scale = spartanscale2()) +
      draw_plot(base_plot2, x = chartposx2(), y = chartposy2(), scale = chartscale2()) +
      draw_plot(base_plot2_legend, x = legendposx2(), y = legendposy2())
    
  }
  
  kill_styles_consolidated <- reactive({
    
    kill_style() %>%
      mutate(value_log = log(value)) %>%
      ggplot(aes(x = Kill_Type, y = value, fill = Kill_Type, color = linecolor())) + 
      labs(x = NULL, y = NULL) +
      geom_bar(stat = "identity", color = linecolor(), alpha = .8) +
      theme(panel.grid.minor = element_blank(),
            legend.position = c(.6, .70),
            legend.direction = "vertical",
            legend.background = element_rect(fill=alpha("white", 0.5)),
            legend.title = element_text(face = "bold"),
            plot.title = element_text(hjust = .5, size = 20),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) +
      scale_fill_brewer(palette = colors(), direction = 1) +
      geom_label(aes(label = value), size = 4, fill = "white", color = "black", show.legend = FALSE, alpha = .5, position = position_stack(vjust = .6)) +
      coord_flip()
    
  })
  
  ##PLOT 3 - KILL:DEATH RATIO PLOT (SINGLE AND CONSOLIATED)
  
  KDA_single <- function() {
    
    base_plot3 <- KDA_df() %>%
      ggplot(aes(x = "", y = reorder(value, value), fill = var)) +
      geom_bar(stat = "identity", alpha = 0.8, color  = linecolor(), width = .5, position = "stack") +
      scale_fill_brewer(palette = colors()) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 20, hjust = 0.5),
            legend.position = "top",
            legend.direction = legenddirection3(),
            legend.background = element_rect(fill = alpha("white", 0.4))) +
      ylab(label = NULL) +
      xlab(label = NULL) +
      coord_polar(theta = "y", direction = 1) +
      geom_label(aes(label = value), position = position_stack(vjust = .5), size = 4, show.legend = FALSE, fill = "white", color = "black", alpha = .7) 
      
    base_plot3_legend <- get_legend(base_plot3)
    base_plot3 <- base_plot3 + theme(legend.position = "none")
    
    ggdraw() +
      draw_image(background(), scale = 1.3) +
      draw_image(emblem_img(), x = emblemposx3(), y = emblemposy3(), scale = emblemscale3()) +
      draw_image(spartan_img(), x = spartanposx3(), y = spartanposy3(), scale = spartanscale3()) +
      draw_plot(base_plot3, x = chartposx3(), y = chartposy3(), scale = chartscale3()) +
      draw_plot(base_plot3_legend, x = legendposx3(), y = legendposy3())

  }
  
  KDA_plot_consolidated <- reactive({
    
    KDA_df() %>%
      ggplot(aes(x = var, y = value, fill = var)) +
      geom_col(alpha = 0.8, color = linecolor(), width = 1) +
      scale_fill_brewer(palette = colors(), direction = 1) +
      geom_label(aes(label = value), position = position_stack(vjust = .6), size = 4, show.legend = FALSE, fill = "white", color = "black", alpha = 0.5) +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks= element_blank(),
            legend.title = element_blank(),
            legend.position = c(.6, .275),
            legend.direction = "vertical",
            legend.background = element_rect(fill = alpha("white", 0.5)),
            plot.caption = element_text(color = linecolor(), hjust = .7)) +
      ylab(label = NULL) +
      xlab(label = NULL) +
      coord_flip() +
      labs(caption = paste(mode_title(), " Kills:Deaths = ", unique(KDA_df()$KillDeathRatio), sep = "")) 
  })
  
  ##Plot 4 - Win:Lost Ratio Plot (Single and Consolidated)
  
  win_lost_plot_single <- reactive({
    
    base_plot4 <-  winlost() %>%
      ggplot(aes(x = category, y = value, fill = category)) +
      geom_bar(stat = "identity", color = linecolor(), alpha = .8) +
      labs(x = NULL, y = NULL) +
      scale_fill_brewer(palette = colors()) +
      geom_label(aes(label = value), position = position_stack(vjust = .5), size = 4, fill = "white", color = "black", alpha = .5, show.legend = FALSE) +
      theme(axis.line.x = element_line(color = linecolor()),
            axis.ticks = element_line(color = linecolor()),
            axis.line.y = element_line(color = linecolor()),
            axis.text = element_blank(),
            legend.direction = legenddirection4(),
            legend.background = element_rect(fill=alpha("white", 0.5)),
            legend.position = c(.3, 1.0),
            legend.title = element_blank(),
            panel.background = element_blank())
    
    base_plot4_legend <- get_legend(base_plot4)
    base_plot4 <- base_plot4 + theme(legend.position = "none")
    
    ggdraw() +
      draw_image(background(), scale = 1.3) +
      draw_image(emblem_img(), x = emblemposx4(), y = emblemposy4(), scale = emblemscale4()) +
      draw_image(spartan_img(), x = spartanposx4(), y = spartanposy4(), scale = spartanscale4()) +
      draw_plot(base_plot4, x = chartposx4(), y = chartposy4(), scale = chartscale4()) +
      draw_plot(base_plot4_legend, x = legendposx4(), y = legendposy4())
  })
  
  
  win_lost_plot_consolidated <- reactive({winlost() %>%
      ggplot(aes(x = category, y = value, fill = category)) +
      geom_bar(stat = "identity", color = linecolor(), alpha = .8) +
      labs(x = NULL, y = NULL) +
      scale_fill_brewer(palette = colors(), direction = 1) +
      geom_label(aes(label = value), position = position_stack(vjust = .5), size = 4, fill = "white", color = "black", alpha = .5, show.legend = FALSE) +
      theme(axis.ticks = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            legend.direction = "vertical",
            legend.background = element_rect(fill=alpha("white", 0.5)),
            legend.position = c(0, .275),
            legend.title = element_blank(),
            plot.caption = element_text(color = linecolor(), hjust = .2),
            panel.background = element_blank()) +
      coord_flip() +
      scale_y_reverse() +
      labs(caption = paste(mode_title(), " Wins:Losses = " , unique(winlost()$Win_Loss), sep = ""))
    
  })
  
  ##****************************************************************************##
  
  ##****************************************************************************##
  
  ##                             SHINY OUTPUTS
  
  ##**************************************************************************.34, **##
  
  ##****************************************************************************##
  
  
  output$shootingaccuracy <- renderPlot({
    
    shooting_accuracy_single() 
    
  })
  
  output$killstyles <- renderPlot({
    
    
    kill_styles_single()
    
    
  })
  
  output$kda <- renderPlot({
    
    KDA_single()
    
  })
  
  output$winlost <- renderPlot({
    
    win_lost_plot_single()
    
  })
  
  consolidated <- function(){
    
    
    bitmap1 <- emblem_img()[[1]]
    bitmap1[4,,] <- as.raw(as.integer(bitmap1[4,,]) * 0.2)
    img1 <- image_read(bitmap1)
    
    
    plot1 <- plot_grid(shooting_accuracy_plot_consolidated(), kill_styles_consolidated(),
                       win_lost_plot_consolidated(), KDA_plot_consolidated(),
                       nrow = 2, rel_heights = c(2, 1))
    
    edited_player_name <- reactive({player() %>% str_replace(., "\\+", " ")})
    
    ggdraw() +
      draw_image(background(), scale = 1.30) +
      draw_text(paste(edited_player_name()), x = textposx(), y = textposy(), size = textsize(), family = "Halo", color = linecolor(), alpha = 1) +
      draw_image(img1, x = emblemposx(), y = emblemposy(), scale = emblemscale()) +
      draw_image(spartan_img(), x = spartanposx(), y = spartanposy(), scale = spartanscale()) +
      draw_plot(plot1) 
    
  }
  
  output$consolidated <- renderPlot({
    
    consolidated()
    
  })
  
  
  
  output$downloadPlot <- downloadHandler(
    
    filename = "Halo5_Stats.png",
    content = function(file) {
      save_plot(file, consolidated(), base_width = base_width(), base_height = base_height)       
      
    }
  )
  

  output$downloadPlot1 <- downloadHandler(
    
    filename = "Shooting_accuracy.png",
    content = function(file) {
      save_plot(file, shooting_accuracy_single(), base_width = base_width(), base_height = base_height)   
    }
  )
  
  
  output$downloadPlot2 <- downloadHandler(
    
    filename = "Halo5_Kill_Styles.png",
    content = function(file) {
      save_plot(file, kill_styles_single(), base_width = base_width(), base_height = base_height)       
      
    }
  )
  
  output$downloadPlot3 <- downloadHandler(
    
    filename = "Halo5_KDA_Ratio.png",
    content = function(file) {
      save_plot(file, KDA_single(), base_width = base_width(), base_height = base_height)       
      
    }
  )
  
  output$downloadPlot4 <- downloadHandler(
    
    filename = "Halo5_Win_Lost_Ratio.png",
    content = function(file) {
      save_plot(file, win_lost_plot_single(), base_width = base_width(), base_height = base_height)      
      
    }
  )

}
shinyApp(ui = ui, server = server)


