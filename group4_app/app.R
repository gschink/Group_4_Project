library(tidyverse)
library(readxl)
library(wesanderson)
library(shiny)
library(quanteda)
library(quanteda.textplots)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(scales)
library(stringr)
library(forcats)
library(ggrepel)
library(shinyWidgets)
library(shinythemes)
library(magrittr)
library(dplyr)
        
df <- read_excel("finaler_project.xls")
  
Era <- df %>% 
  group_by(Era)%>%
  distinct(Era)%>%
  arrange(Era)

Genre <- df %>% 
  group_by(genre)%>%
  distinct(genre)%>%
  arrange(genre)

Genre <- Genre %>%
  filter(genre!='Family'& genre!='Film-Noir'& genre!='Fantasy' & genre!='Thriller')

Genre <-  as.character(Genre$genre)

df$votes=str_remove(df$votes,",")
df$votes=str_remove(df$votes,",")
df$votes=as.integer(df$votes)

df$gross=str_remove(df$gross,".")
df$gross=str_remove(df$gross,"$")
df$gross=str_remove(df$gross,"M")
df$gross=as.numeric(df$gross)

ui <- fluidPage(tags$style(
                           '.topimg {
                           margin-left:-30px;
                           margin-right:-30px;}'), 
                div(class='topimg', img(src="https://play-lh.googleusercontent.com/R5x56X7xOHZenDcDjP9q9qwWGg3iB7KKEz1KUMHbEurjDRXY4VLb3LBjOZ4u1_XiXzC-", height='400px',width="100%")),
      tags$div(style = 'background-color:black;',
      tabsetPanel(type = 'tabs',
               tabPanel(tags$span(style='font-family:Impact, sans-serif; color:#f3ce13;','Main Page'),
                        tags$style('.container-fluid{background-color: #f3ce13}'),
                              column(2,
                                  selectInput(inputId ='Era_Val', label = tags$span(style='font-family:Impact, sans-serif;','Era'), multiple=F, 
                                              choices= Era, selected = 1990)),
                               column(10, 
                                      fluidRow(
                                           column(5,
                                                       tags$h5(tags$span(style='font-family:Impact, sans-serif;', 'Word Cloud')),
                                                        plotOutput('wordcloud')
                                                        ),
                                            column(5, 
                                                   tags$h5(tags$span(style='font-family:Impact, sans-serif;', "Bar Plot"),
                                                    plotOutput('freq')
                                                  ))),
                                          fluidRow(
                                               column(5, 
                                                      tags$h5(tags$span(style='font-family:Impact, sans-serif;', "Run Time by Year"),
                                                      plotOutput('peter_graph')
                                                      )),
                                              column(5,
                                                     h5(tags$span(style='font-family:Impact, sans-serif;','Violin Plot')),
                                                     plotOutput('violins')
                                            ))
                                      )),
                tabPanel(tags$span(style='font-family:Impact, sans-serif; color:#f3ce13;','Word Cloud'),
                         column(2,
                              radioButtons('genre_cloud_check', label = 'Pick genre or genres', choices = Genre)),
                         column(10,
                               plotOutput('tabwordcloud', height='800px')
                         )),
                tabPanel(tags$span(style='font-family:Impact, sans-serif; color:#f3ce13;','Run Time by Year'), 
                         column(2, radioButtons('runtime_genre', label = 'Pick genre or genres', choices = Genre)),
                         column(10,
                                plotOutput('tab_peter_graph', height='800px')
                         )),
                tabPanel(tags$span(style='font-family:Impact, sans-serif; color:#f3ce13;','Dot Plot'),
                         column(2, radioButtons("bar_col", "Choose metric:", choices=Genre)),
                         column(10,
                                plotOutput('tab_freq',height='800px')
                         )),
              tabPanel(tags$span(style='font-family:Impact, sans-serif; color:#f3ce13;','Violins'), 
                       column(2, radioButtons("violin_genre", "Choose metric:", choices=Genre)),
                       column(10,
                              plotOutput('tab_violin',height='800px')
                              ))
      )))



server <- function(input, output, session){
    
  output$wordcloud <- renderPlot({df2 <- df %>% filter(Era == input$Era_Val)
                      CORPUS <- corpus(df2$timeline)
                      DFM <- dfm(tokens(CORPUS))
                      DFM <- dfm(tokens(CORPUS,
                                 remove_punct=T))
                      DFM <- dfm_remove(DFM, stopwords('english'))
                      DFM <- dfm_wordstem(DFM)       
                      pal <- wes_palette("Zissou1", 100, type="continuous")
                      textplot_wordcloud(DFM, max_size = 6,max_words=90, color=pal)
                      })
  
  output$peter_graph<-renderPlot({df3 <- df %>% filter(Era==input$Era_Val) 
                                  df4 <- df3 %>% group_by(year) %>% summarise(runmean=mean(runtime))
                                  
                                  ggplot(df4, aes(x=year,y=runmean))+
                                  geom_line()+
                                  labs(caption="H/T IMBD Top Movies Database",
                                    title="Average Runtime of a Movie by Year",
                                    x="Year of Release",y="Mean Run Time\n(in min)") +
                                  scale_x_continuous(n.breaks = 10)+
                                  scale_y_continuous()+
                                  theme_economist()+
                                  theme(plot.caption = element_text(size = 5),
                                        plot.title = element_text(face = "bold",
                                                                  size = 15),
                                        axis.title.x = element_text(face = "bold",
                                                                    size = 10),
                                        axis.title.y = element_text(face = "bold",
                                                                    size = 10))})
  output$freq <- renderPlot({final2 = df %>% 
                              group_by(genre)%>%
                              filter(Era == input$Era_Val)%>%
                              mutate(vote_mean = mean(votes))
                            
                            ggplot(final2, aes(x= reorder(genre,vote_mean,sum), y=vote_mean)) + 
                              theme_economist()+
                              geom_col() + 
                              labs(title="Avg. IMDB Votes by Genre",
                                   x="Primary Genre of Movie",
                                   y="Mean Votes")+
                              scale_y_continuous(labels = comma)
                            })
  
  output$violins <- renderPlot({final3=df%>% filter(Era==input$Era_Val) %>%
                                         group_by(year)
  
                                ggplot(final3,aes(x = year, y = rating))+ 
                                  geom_violin(width = 10) + 
                                theme_economist()+
                                labs(title = "Violin Plot of Movie Ratings",
                                     x="Year of Release",
                                     y="IMDB Rating")+
                                scale_x_continuous(n.breaks = 10)+
                                geom_boxplot(aes(group=Era),width=1)})
  
  output$tabwordcloud <- renderPlot({df2 <- df %>% filter(genre == input$genre_cloud_check)
                                     CORPUS <- corpus(df2$timeline)
                                     DFM <- dfm(tokens(CORPUS))
                                     DFM <- dfm(tokens(CORPUS,
                                                      remove_punct=T))
                                     DFM <- dfm_remove(DFM, stopwords('english'))
                                     DFM <- dfm_wordstem(DFM)       
                                     pal <- wes_palette("Darjeeling1", 200, type="continuous")
                                     textplot_wordcloud(DFM, max_words=75, color=pal)
                                    })
  
  output$tab_peter_graph<-renderPlot({df3 <- df %>% filter(genre==input$runtime_genre) 
  df4 <- df3 %>% group_by(year) %>% summarise(runmean=mean(runtime))
  
  ggplot(df4, aes(x=year,y=runmean))+
    geom_line()+
    labs(caption="H/T IMBD Top Movies Database",
         title="Average Runtime of a Movie by Genre Chosen",
         x="Year of Release",y="Mean Run Time\n(in min)") +
    scale_x_continuous(n.breaks = 10)+
    scale_y_continuous()+
    theme_economist()+
    theme(plot.caption = element_text(size = 5),
          plot.title = element_text(face = "bold",
                                    size = 15),
          axis.title.x = element_text(face = "bold",
                                      size = 10),
          axis.title.y = element_text(face = "bold",
                                     size = 10))})
  
  output$tab_freq <- renderPlot({
    final2 <- df %>%
      group_by(genre,era)%>% 
      summarise_at(vars(votes), 
                   list(votes = mean))%>% 
      filter(genre == input$bar_col)
    
    
    # Plot
    ggplot(final2, aes(x=era, y=votes)) + 
      geom_point(col="tomato2", size=6) +   # Draw points
      geom_segment(aes(x=era, 
                       xend=era, 
                       y=min(votes), 
                       yend=max(votes)), 
                   linetype="dashed", 
                   size=0.1) + 
      theme_economist()+# Draw dashed lines
      labs(title="Dot Plot", 
           subtitle="Era Vs Avg. votes") +  
      coord_flip()+
      scale_y_continuous(labels = comma)
    
  })
  
  output$tab_violin <- renderPlot({final3<-df%>% filter(genre==input$violin_genre) %>%
    group_by(year)%>%
    count()
  
  ggplot(final3, aes(x = n, y = year))+ 
    geom_violin(width = 10) + 
    theme_economist()+
    theme(axis.ticks.x=element_blank())+
    labs(title = "Violin Plot of Movie Ratings",
         x="Count",
         y="Year")})
}

shinyApp(ui, server)

