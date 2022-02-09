#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shiny)
library(car)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(ggplot2)
library(writexl)
library(DT)
library(rsconnect)

options(scipen = 9999)

seasons = 2021
data <-  purrr::map_df(seasons, function(x) {
    readRDS(
        url(
            glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
        )
    )
})
colnames(data)
get_rush_type <- function(df, run_typ_arg, facet_var, start_week, end_week) 
{
    
    df %>%
        filter(play_type == "run"
               & week >= as.integer(start_week) 
               & week <= as.integer(end_week)) %>%
        left_join(fast_scraper_roster(c(2020)), by = c("rusher_player_id" = "gsis_id")) %>%
        filter(position == "RB") %>%
        dplyr::group_by_at(run_typ_arg) %>%
        summarize(rush_yards = sum(yards_gained),
                  attempts = n(),
                  yards_per_attempt = round((rush_yards / attempts),1),
                  mean_epa = round(sum(epa),2))%>%
        left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
        rename(fac_var = !!facet_var)  %>%
        mutate(fac_var = if_else(is.na(fac_var),"middle",fac_var)) %>%
        ungroup() %>%
        select(team_logo_espn, fac_var,attempts,yards_per_attempt, mean_epa) %>%
        ggplot(mapping = aes(x = mean_epa, y = attempts))+
        geom_image(aes(image = team_logo_espn), size = 0.087)+
        facet_wrap(~ fac_var, nrow = 2) + 
        labs(title = "EPA and Rushing Attempts by Rushing Type", x = "Estimated Points Added (sum)", y ="Rushing Attempts")
    } 


# Define UI for application that draws a histogram
ui <- 
    navbarPage("Offensive & Defensive NFL Stats",collapsible = TRUE, inverse = TRUE,
               tabPanel("Rushing Stats",fluidPage(theme = shinytheme("superhero"),
                   # Application title
                    titlePanel("Rushing Stats"),
                   
                   # Sidebar with a slider input for week of season 
                    sidebarLayout(
                       sidebarPanel(
                           selectInput('Start_Week','Start Week', choices = 1:21),
                           selectInput('End_Week','End Week', 17, choices = 1:21),
                           checkboxGroupInput('Position','Position(s)', choices = c("RB","WR","QB"),selected=c("RB","WR","QB"),inline = TRUE),
                           numericInput("MinAttempts", "Minimum Rushing Attempts:", 5, min = 1, max = 1000), width = 2),
                       
                       # Show a plot of the generated distribution
                    mainPanel("",
                                 dataTableOutput("rushing_table"),
                              tags$style(HTML("
                                                .dataTables_wrapper, .dataTables_wrapper 
                                                .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                                .dataTables_wrapper .dataTables_processing, 
                                                .dataTables_wrapper .dataTables_paginate {
                                                color: #fff;
                                                }
                                                
                                                #DataTables_Table_0_length{
                                                color: #E66C2C;
                                                }
                            
                                                thead {
                                                color: #fff;
                                                }
                            
                                                 tbody {
                                                color: #000000;
                                                }
                            
                                               "))
                       )
                   )
               )
               ),
               tabPanel("Passing Stats",
                        fluidPage(theme = shinytheme("superhero"),
                            titlePanel("Passing Stats"),
                   # Sidebar with a slider input for week of season 
                       sidebarLayout(
                           sidebarPanel(
                               selectInput('Start_Week_QB','Start Week', choices = 1:21),
                               selectInput('End_Week_QB','End Week', 17, choices = 1:21),
                               numericInput("MinPassAttempts", "Minimum Pass Attempts:", 30, min = 1, max = 1000), width = 2
                                        ),
                           
                       # Show a plot of the generated distribution
                           mainPanel("",
                                     dataTableOutput("passing_table"),
                                     tags$style(HTML("
                                                .dataTables_wrapper, .dataTables_wrapper 
                                                .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                                .dataTables_wrapper .dataTables_processing, 
                                                .dataTables_wrapper .dataTables_paginate {
                                                color: #fff;
                                                }
                                                
                                                #DataTables_Table_1_length{
                                                color: #E66C2C;
                                                }
                            
                                                thead {
                                                color: #fff;
                                                }
                            
                                                 tbody {
                                                color: #000000;
                                                }
                            
                                               "))
                                    )
                                    )
                                )
               ),
               tabPanel("Receiving Stats",
                        fluidPage(theme = shinytheme("superhero"),
                            titlePanel("Receiving Stats"),
                            # Sidebar with a slider input for week of season 
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput('Start_Week_WR','Start Week', choices = 1:21),
                                    selectInput('End_Week_WR','End Week', 17, choices = 1:21),
                                    checkboxGroupInput('Position_rec','Position(s)', choices = c("RB","WR","TE"),selected=c("RB","WR","TE"),inline = TRUE),
                                    numericInput("MinTargets", "Minimum Targets: ", 10, min = 1, max = 1000), width = 2,
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel("",
                                          dataTableOutput("rec_table"),
                                          tags$style(HTML("
                                                .dataTables_wrapper, .dataTables_wrapper 
                                                .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                                .dataTables_wrapper .dataTables_processing, 
                                                .dataTables_wrapper .dataTables_paginate {
                                                color: #fff;
                                                }
                                                
                                                #DataTables_Table_2_length{
                                                color: #E66C2C;
                                                }
                            
                                                thead {
                                                color: #fff;
                                                }
                            
                                                 tbody {
                                                color: #000000;
                                                }
                            
                                               "))
                                )
                            )
                        )
                ),
               tabPanel("Defensive Stats",
                        fluidPage(theme = shinytheme("superhero"),
                                  titlePanel("Defensive Stats"),
                                  # Sidebar with a slider input for week of season 
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput('Start_Week_DEF','Start Week', choices = 1:21),
                                          selectInput('End_Week_DEF','End Week', 17, choices = 1:21),
                                          width = 2,
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel("",
                                                dataTableOutput("def_table"),
                                                tags$style(HTML("
                                                .dataTables_wrapper, .dataTables_wrapper 
                                                .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                                .dataTables_wrapper .dataTables_processing, 
                                                .dataTables_wrapper .dataTables_paginate {
                                                color: #fff;
                                                }
                                                
                                                #DataTables_Table_3_length{
                                                color: #E66C2C;
                                                }
                            
                                                thead {
                                                color: #fff;
                                                }
                            
                                                 tbody {
                                                color: #000000;
                                                }
                            
                                               "))
                                      )
                                  )
                        )
                ),
               tabPanel("Rushing Type Stats",
                        fluidPage(theme = shinytheme("superhero"),
                                  titlePanel("Rushing Type Stats"),
                                  # Sidebar with a slider input for week of season 
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput('Start_Week_RT','Start Week', choices = 1:21),
                                          selectInput('End_Week_RT','End Week', 17, choices = 1:21),
                                          selectInput('run_gap_loc','Rushing Type', 'run_gap', choices = c('run_gap',
                                                                                                           'run_location')),
                                          width = 2,
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel("..this takes a few seconds to load..",
                                                plotOutput("run_loc"),
                                                tags$style(HTML("
                                                .dataTables_wrapper, .dataTables_wrapper 
                                                .dataTables_filter, .dataTables_wrapper .dataTables_info, 
                                                .dataTables_wrapper .dataTables_processing, 
                                                .dataTables_wrapper .dataTables_paginate {
                                                color: #fff;
                                                }
                                                
                                                #DataTables_Table_3_length{
                                                color: #E66C2C;
                                                }
                            
                                                thead {
                                                color: #fff;
                                                }
                            
                                                 tbody {
                                                color: #000000;
                                                }
                            
                                               "))
                                      )
                                  )
                        )
               )
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$rushing_table <-renderDataTable(rushing_df <- data %>%
                                       filter(play_type == "run" 
                                              & week >= as.integer(input$Start_Week) 
                                              & week <= as.integer(input$End_Week)) %>%
                                       left_join(fast_scraper_roster(c(2020)), by = c("rusher_player_id" = "gsis_id")) %>%
                                       group_by(rusher_player_id,position, posteam) %>%
                                       summarize(rush_yards = sum(yards_gained),
                                                 rush_touchdowns = sum(rush_touchdown),
                                                 attempts = n(),
                                                 yards_per_attempt = round((rush_yards / attempts),1),
                                                 mean_epa = round(mean(epa),2),
                                                 games_played = n_distinct(game_id),
                                                 player = max(rusher_player_name),
                                                 att_pgm = round(attempts/games_played),2) %>%
                                       filter(attempts >= input$MinAttempts) %>%
                                       filter(position %in% input$Position) %>%
                                       left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
                                       mutate(team = paste("<img src=","'",team_logo_espn,"'", " height = ","'30'","> </img>",sep = "")) %>%
                                       ungroup() %>%
                                       select(team,player, position,games_played, 
                                              attempts, att_pgm,rush_yards, yards_per_attempt,
                                              rush_touchdowns, mean_epa) %>%
                                       arrange(desc(rush_yards)), options = list(pageLength = 50,initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'color': '#fff'});",
                                           "}")),class = 'cell-border stripe',escape = FALSE,
                                       selection = 'single'
                                       )

        output$passing_table <-renderDataTable(passing_df <- data %>%
                                               filter(play_type == "pass" 
                                                      & week >= as.integer(input$Start_Week_QB) 
                                                      & week <= as.integer(input$End_Week_QB)
                                                      & sack == 0
                                                      ) %>%
                                               left_join(fast_scraper_roster(c(2020)), by = c("passer_player_id" = "gsis_id")) %>%
                                               group_by(passer_player_id,posteam) %>%
                                               summarize(pass_yards = sum(yards_gained),
                                                         pass_touchdowns = sum(pass_touchdown),
                                                         pass_attempts = sum(pass_attempt),
                                                         yards_per_attempt = round((pass_yards / pass_attempts),1),
                                                         mean_epa = round(mean(epa),2),
                                                         games_played = n_distinct(game_id),
                                                         player = max(passer_player_name),
                                                         cpoe = round(mean(cpoe, na.rm = TRUE),1)) %>% ### FIX THIS CALC
                                               filter(pass_attempts >= input$MinPassAttempts) %>%
                                               left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
                                               mutate(team = paste("<img src=","'",team_logo_espn,"'", " height = ","'30'","> </img>",sep = "")) %>%
                                               ungroup() %>%
                                               select(team,player,games_played, 
                                                      pass_attempts, pass_yards, yards_per_attempt, cpoe,
                                                      pass_touchdowns, mean_epa) %>%
                                               arrange(desc(yards_per_attempt)), options = list(pageLength = 40),class = 'cell-border stripe',escape = FALSE)

        output$rec_table <-renderDataTable(rec_df <- data %>%
                                                   filter(play_type == "pass" 
                                                          & week >= as.integer(input$Start_Week_WR) 
                                                          & week <= as.integer(input$End_Week_WR)
                                                          & receiver_player_id != ""
                                                          )%>%
                                                   left_join(fast_scraper_roster(c(2020)), by = c("receiver_player_id" = "gsis_id")) %>%
                                                   group_by(receiver_player_id,position, posteam) %>%
                                                    mutate(reception = if_else(pass_attempt == 1 & incomplete_pass == 0,1,0)) %>%
                                                   summarize(games_played = n_distinct(game_id),
                                                             rec_yards = sum(receiving_yards, na.rm = TRUE),
                                                             yards_pgm = round(rec_yards/games_played,1),
                                                             rec_tds = sum(pass_touchdown),
                                                             targets_pgm = round(n()/games_played,2),
                                                             targets = n(),
                                                             rec_pgm = round(sum(reception)/games_played,2),
                                                             recs = sum(reception, na.rm=TRUE),
                                                             catch_rate = round((sum(reception, na.rm=TRUE) / n())*100,2),
                                                             avg_air_yards = round(mean(air_yards, na.rm = TRUE),1),
                                                             yac = round(mean(yards_after_catch, na.rm = TRUE),1),
                                                             mean_epa = round(mean(epa, na.rm = TRUE),2),
                                                             player = max(receiver_player_name)
                                                             ) %>%
                                                   filter(position %in% input$Position_rec) %>%
                                                   filter(targets >= input$MinTargets) %>%
                                                   left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
                                                   mutate(team = paste("<img src=","'",team_logo_espn,"'", " height = ","'30'","> </img>",sep = "")) %>%
                                                  ungroup() %>%
                                                  select(team,player, position, games_played, targets_pgm,rec_pgm, targets, recs,
                                                          catch_rate,rec_yards, yards_pgm, yac, avg_air_yards,
                                                          rec_tds, mean_epa) %>%
                                                   arrange(desc(rec_yards)), 
                                           options = list(pageLength = 50),class = 'cell-border stripe',escape = FALSE)       
        
        output$def_table <-renderDataTable(def_df <- data %>%
                                               filter(play_type %in% c("pass","run") 
                                                      & week >= as.integer(input$Start_Week_DEF) 
                                                      & week <= as.integer(input$End_Week_DEF)) %>%
                                               group_by(defteam) %>%
                                               summarize(games_played = n_distinct(game_id),
                                                         yards_pgm = round(sum(yards_gained, na.rm = TRUE)/games_played,1),
                                                         targets_pgm = round(sum(pass_attempt)/games_played,2),
                                                         rushes_pgm = round(sum(rush_attempt)/games_played,2),
                                                         INTs = sum(interception, na.rm = TRUE),
                                                         sacks_pgm = round(sum(sack, na.rm = TRUE)/games_played,2),
                                                         rec_yds_pgm = round(sum(receiving_yards, na.rm = TRUE)/games_played,1),
                                                         rush_yds_pgm = round((sum(yards_gained, na.rm = TRUE) - sum(receiving_yards, na.rm = TRUE))/games_played,1),
                                                         rush_yds_att = round((sum(yards_gained, na.rm = TRUE) - sum(receiving_yards, na.rm = TRUE))/sum(rush_attempt),2),
                                                         avg_air_yards = round(mean(air_yards, na.rm = TRUE),1),
                                                         mean_epa = round(mean(epa, na.rm = TRUE),2)
                                               ) %>%
                                               left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>%
                                               mutate(team = paste("<img src=","'",team_logo_espn,"'", " height = ","'30'","> </img>",sep = "")) %>%
                                               select(team, team_nick, yards_pgm, targets_pgm,
                                                      rushes_pgm, INTs, sacks_pgm, rec_yds_pgm,
                                                      rush_yds_pgm, rush_yds_att, avg_air_yards,mean_epa) %>%
                                               arrange(yards_pgm), 
                                           options = list(pageLength = 32),class = 'cell-border stripe',escape = FALSE) 
        

        output$run_loc <-renderPlot(get_rush_type(data, c(input$run_gap_loc,"posteam"), input$run_gap_loc, input$Start_Week_RT, input$End_Week_RT),
                                    width = 1050, height = 833)
        }

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::deployApp('Desktop/dev/R/school/hds500/week6/nfl_metrics')

