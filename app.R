#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(formattable)
library(DT)     
library(stringi)
library(shinythemes)
library(shinymaterial)
source('shiny_data_update.R')

lineup_data <- read.csv("lineup_data.csv")
team_names <- unique(as.vector(stri_trans_general(lineup_data$Team,
                                               "Latin-ASCII")))





# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  

    tabsetPanel(type = "tabs",
                tabPanel("Welcome to the Show",
                         includeHTML("introduction.html"),
    
    
    
   

    # Sidebar with a slider input for number of bins 
    

        # Show a plot of the generated distribution
        mainPanel(
          
          
        )
        
                ),
    tabPanel("Your Team",
             selectInput('team_choice', 'Team', choices = team_names),
             sliderInput("minimum_poss",
                         "Minimum Number of Possessions:",
                         value = 50,
                         min = 0,
                         max = max(lineup_data$Offensive.Poss) * 2.5),
            
             
             
             
             
             formattableOutput("table"),
             
             plotOutput("lineup_distribution"),
             
    ),
    
    tabPanel("Your Player",
             
             
             
             fluidRow(
               column(4,
                      textInput("player_searched", "Search for a Player", "")
               ),
               column(4,
                      textInput("player_searched_sec", "Search for a second Player", "")
               ),
               column(4,
                      sliderInput("minimum_poss_player",
                                  "Minimum Number of Possessions:",
                                  value = 50,
                                  min = 0,
                                  max = max(lineup_data$Offensive.Poss) * 2.5)
               )
             ),
             
             
             
             
             
             
             
             
             
             
             
             formattableOutput("player_plot")),
    
    
    tabPanel("Top Performances",
             
             
                 sidebarPanel(
             sliderInput("minimum_poss_performers",
                         "Minimum Number of Possessions:",
                         value = 50,
                         min = 0,
                         max = max(lineup_data$Offensive.Poss) * 2.5),
             hr(),
             helpText("Use the scatter plot to the right to investigate the
                      top offensive and defensive lineup performances. To do so,
                      click on the respective bubbles to find out which five players
                      are responsible and get additional information.\
                      In addition to that, find two overview tables for offense and defense below")),
             
             mainPanel(
               fluidRow(
                 column(width = 12,
                        h1("Points near click (Scroll down for Top-10 Lists)"),
                        formattableOutput("click_info")
                 )
                 
               ),
             
             plotOutput("plot_ortg_drtg", height = 700, width = 700,
                        click = "plot_ortg_drtg_click",
                        brush = brushOpts(
                            id = "plot_ortg_drtg_brush"
                        )),
             tags$h1("Top 10 NetRtg"),
             formattableOutput("best_offenses"),
             tags$h1("Top 10 DRtg"),
             formattableOutput("best_defenses"),
             tags$h1("Top 10 ORtg"),
             formattableOutput("best_netrtg"),
             
             
             
             
             
             
             
             )
    )
   
        
    
    )
        
      
        
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  
  
  output$best_netrtg <- renderFormattable({
    
    
    lineup_data_display <- lineup_data %>% 
      select(Lineup, `Offensive.Poss`,
             `Defensive.Poss`, ORtg, DRtg, Team)%>% 
      mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
             NetRtg = ORtg - DRtg) %>% 
      select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
    
    lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                   "Latin-ASCII")
    lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                     "Latin-ASCII")
    
    lineup_data_display <- dplyr::filter(lineup_data_display, input$minimum_poss_performers < Possessions)
    lineup_data_display <- lineup_data_display[order(lineup_data_display$ORtg, decreasing = TRUE),]
    lineup_data_display <- head(lineup_data_display,10)
    
    
    
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <- 
      formatter("span", 
                style = x ~ style(
                  font.weight = "bold", 
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
    
    formattable(lineup_data_display,
                align = c("l", "c", "c", "c", "c", "c"), list(
                  `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                  `NetRtg`= improvement_formatter,
                  `Possessions` = color_bar(customRed)))
    
    
    
    
  })
  
  
  
  
  output$best_defenses <- renderFormattable({
    
    
    lineup_data_display <- lineup_data %>% 
      select(Lineup, `Offensive.Poss`,
             `Defensive.Poss`, ORtg, DRtg, Team)%>% 
      mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
             NetRtg = ORtg - DRtg) %>% 
      select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
    
    lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                   "Latin-ASCII")
    lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                     "Latin-ASCII")
    
    lineup_data_display <- dplyr::filter(lineup_data_display, input$minimum_poss_performers < Possessions)
    lineup_data_display <- lineup_data_display[order(lineup_data_display$DRtg, decreasing = FALSE),]
    lineup_data_display <- head(lineup_data_display,10)
    
    
    
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <- 
      formatter("span", 
                style = x ~ style(
                  font.weight = "bold", 
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
    
    formattable(lineup_data_display,
                align = c("l", "c", "c", "c", "c", "c"), list(
                  `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                  `NetRtg`= improvement_formatter,
                  `Possessions` = color_bar(customRed)))
    
    
    
    
  })
  
  
  
  
  
  
  output$best_offenses <- renderFormattable({
  
  
    lineup_data_display <- lineup_data %>% 
      select(Lineup, `Offensive.Poss`,
             `Defensive.Poss`, ORtg, DRtg, Team)%>% 
      mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
             NetRtg = ORtg - DRtg) %>% 
      select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
    
    lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                   "Latin-ASCII")
    lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                     "Latin-ASCII")
    
    lineup_data_display <- dplyr::filter(lineup_data_display, input$minimum_poss_performers < Possessions)
    lineup_data_display <- lineup_data_display[order(lineup_data_display$NetRtg, decreasing = TRUE),]
    lineup_data_display <- head(lineup_data_display,10)
    
    
    
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <- 
      formatter("span", 
                style = x ~ style(
                  font.weight = "bold", 
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
    
    formattable(lineup_data_display,
                align = c("l", "c", "c", "c", "c", "c"), list(
                  `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                  `NetRtg`= improvement_formatter,
                  `Possessions` = color_bar(customRed)))
    
    
    
    
  })
  
  
  output$player_plot <- renderFormattable({
  
    lineup_data_display <- lineup_data %>% 
      select(Lineup, `Offensive.Poss`,
             `Defensive.Poss`, ORtg, DRtg, Team)%>% 
      mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
             NetRtg = ORtg - DRtg) %>% 
      select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
      
    
    lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                   "Latin-ASCII")
    lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                     "Latin-ASCII")
    
    lineup_data_display <- dplyr::filter(lineup_data_display, grepl(input$player_searched, Lineup))
    lineup_data_display <- dplyr::filter(lineup_data_display, grepl(input$player_searched_sec, Lineup))
    
    
    
    lineup_data_display <- dplyr::filter(lineup_data_display, input$minimum_poss_player < Possessions)
    
    
    

    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <- 
      formatter("span", 
                style = x ~ style(
                  font.weight = "bold", 
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
  
    formattable(lineup_data_display,
                align = c("l", "c", "c", "c", "c", "c"), list(
                  `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                  `NetRtg`= improvement_formatter,
                  `Possessions` = color_bar(customRed)))
    
    
    
    
    
    
    
  })
    
  
  
  
  
  
  
  
  
  
    
    output$click_info <- renderFormattable({
        
        
        lineup_data_display <- lineup_data %>% 
            select(Lineup, `Offensive.Poss`,
                   `Defensive.Poss`, ORtg, DRtg, Team)%>% 
            mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
                   NetRtg = ORtg - DRtg) %>% 
            select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
        
        lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                       "Latin-ASCII")
        lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                         "Latin-ASCII")
        
        
        lineup_data_display <- dplyr::filter(lineup_data_display, input$minimum_poss_performers < Possessions)
        
        
        
        customGreen0 = "#DeF7E9"
        customGreen = "#71CA97"
        customRed = "#ff7f7f"
        improvement_formatter <- 
            formatter("span", 
                      style = x ~ style(
                          font.weight = "bold", 
                          color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
        tmp <- nearPoints(lineup_data_display, input$plot_ortg_drtg_click, addDist = TRUE)
        tmp$dist_ <- NULL
        formattable(tmp,
                    align = c("l", "c", "c", "c", "c", "c"), list(
                        `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                        `NetRtg`= improvement_formatter,
                        `Possessions` = color_bar(customRed)))
    })
    
    
    output$plot_ortg_drtg <- renderPlot({
        
        
        
        
        
        
        lineup_data_display <- lineup_data %>% 
            select(Lineup, `Offensive.Poss`,
                   `Defensive.Poss`, ORtg, DRtg, Team)%>% 
            mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
                   NetRtg = ORtg - DRtg) %>% 
            select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team) %>% 
           
            filter(Possessions > input$minimum_poss_performers)
        
        
        
        ggplot(data = lineup_data_display, aes(ORtg, DRtg, size = Possessions, colour=NetRtg)) + 
            geom_point( alpha=0.7)+
            geom_hline(yintercept=1.12)+
            geom_vline(xintercept=1.12)+
            xlim(0, 3)+
            ylim(0,3)
            
    })
   
    
    
    
    output$lineup_distribution <- renderPlot({
     
      
      lineup_data_display <- lineup_data %>% 
        select(Lineup, `Offensive.Poss`,
               `Defensive.Poss`, ORtg, DRtg, Team) %>% 
        mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
               NetRtg = ORtg - DRtg) %>% 
        select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team)
        
        

      lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                     "Latin-ASCII")
      lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                       "Latin-ASCII")
      
      
      
      lineup_data_display <- dplyr::filter(lineup_data_display, Team == input$team_choice)
      lineup_data_display <- lineup_data_display[order(lineup_data_display$Possessions),]
      
      
      ggplot(data = lineup_data_display, aes(x=Lineup, y=Possessions))+
        geom_bar(stat = "identity")+
        geom_hline(yintercept = input$minimum_poss, colour="red")+
        labs(title = "How many lineups are there? How many are you missing out on?")+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(size = 22),
              panel.background = element_blank())
        
      
    })
      
      
      
    
    
    output$gamesPlot <- renderPlot({
        teams_games_played <- get_games_per_team(batch)
        teams_games_played$team <- stri_trans_general(teams_games_played$team,
                                        "Latin-ASCII")
        
        
        ggplot(data = teams_games_played)+
            geom_bar(aes(x=team,y=n),stat="identity")+
            coord_flip()
            
        
        
        
    })
    
    
    output$possessionsPlot <- renderPlot({
        
        poss <- get_possesions_per_team(lineup_data)
        poss$Team <- stri_trans_general(poss$Team,
                                                       "Latin-ASCII")
        
        
        
        ggplot(data = poss)+
            geom_bar(aes(x=Team,y=poss),stat="identity")+
            coord_flip()
        
        
        
    })
    
    output$table <- renderFormattable({
        
        customGreen0 = "#DeF7E9"
        customGreen = "#71CA97"
        customRed = "#ff7f7f"
        
        
        
        improvement_formatter <- 
            formatter("span", 
                      style = x ~ style(
                          font.weight = "bold", 
                          color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))
        
        
        lineup_data_display <- lineup_data %>% 
                               select(Lineup, `Offensive.Poss`,
                                      `Defensive.Poss`, ORtg, DRtg, Team)%>% 
                               mutate(Possessions = `Offensive.Poss` + `Defensive.Poss`,
                                      NetRtg = ORtg - DRtg) %>% 
                               select(Lineup, Possessions, NetRtg, ORtg, DRtg, Team) %>%
                              
                               filter(Possessions > input$minimum_poss)
            
            
            
        lineup_data_display$Team <- stri_trans_general(lineup_data_display$Team,
                                                       "Latin-ASCII")
        lineup_data_display$Lineup <- stri_trans_general(lineup_data_display$Lineup,
                                                         "Latin-ASCII")
        
        lineup_data_display <- dplyr::filter(lineup_data_display, Team == input$team_choice)
        
        
        formattable(lineup_data_display, 
                    align = c("l", "c", "c", "c", "c", "c"), list(
                        `Lineup` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                        `NetRtg`= improvement_formatter,
                        `Possessions` = color_bar(customRed)))
        
        })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
