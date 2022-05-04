library(scales)
library(readr)
library(ggplot2)
library(sf)
library(usmap)
library(tidyverse)
library(plotly)
library(rnaturalearth)
library(rnaturalearthhires)
library(forcats)
library(lubridate)
library(plotly)
library(shiny)

# psuedocode:
# ui = navbarPage("Multiple Shiny Apps on One Web Page",
#                   tabPanel("shiny app 1", call(path to shiny app1) ),
#                   tabPanel("shiny app 2", call(path to shiny app2) ),
#                   tabPanel("shiny app 3", call(path to shiny app3) ))
# server = 

library(bslib)

# 1st visualization
datasaljustin = read_csv("justindatasal.csv")

scatterplot = function(df) {
  p = ggplot(df, aes(Avg.Salary.K., 
                     cost.of.living, 
                     col = region,
                     text = paste(
                       "Company: ", Company,
                       "\nJob Title: ", Job.Title,
                       "\nAverage Salary: $", format(Avg.Salary.K.*1000, 
                                                     scientific = F,
                                                     big.mark = ','), 
                       "\nCost of Living: ", cost.of.living,
                       "\nLocation: ", City,", ",State, sep = ''))) +
    geom_point() +
    labs(title = "Data Scientist Salary vs Cost of Living Index",
         x = "Salary ($K)",
         y = "Cost of Living Index",
         color = "Region") +
    scale_color_tableau(
      palette = "Tableau 10",
      type = "regular",
      direction = 1,
    )
  ggplotly(p, tooltip = "text") %>%
    style(hoveron = "fill")
}

text <- function(){
  sidebarLayout(sidebarPanel = (""),
                mainPanel = mainPanel (
                  h3("Appendix"),
                  h4("Graph axes:"),
                  h6("Salary in thousands of dollars"),
                  h6("Cost of Living Index: The cost of living normalized across the entire nation, where 100 is the national average. For example, as COL index of 120 means that place is 20% more expensive than average. It is similar to how ERA+ and OPS+ work in Baseball."),
                  h4("Regions:"),
                  h6("Midwest region: Iowa, Illinois, Indiana, Kansas, Michign, Minnesota, Missouri, North Dakota, Ohio, South Dakota and Wisconsin"),
                  h6("Northeast region: Connecticut, Maine, New Hampshire, New Jersey, Pennsylvania, Rhode Island and Vermont"),
                  h6("South region: Alabama, Arkansas, Deleware, Florida, Georgia, Kenntucky, Louisianna, Maryland, Mississippi, Oklahoma, South Carolina, Tennessee, Texas, Virginia, West Virginia and the District of Columbia"),
                  h6("West region: Alaska, Arizona, Colorado, Hawaii, Idaho, Montana, New Mexico, Nevada, Oregon, Utah, Washington and Wyoming"),
                  align = "left"
                )
  )
}

#Third Visualization
datasaldavid = read_csv("daviddatasal.csv")
y = colnames(datasaldavid)[3:5]
names(y) = c("Jobs","Average Salary","Average Rating")



ui <- fluidPage(
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction", 
          p("p creates a paragraph of text."),
          p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
        ),
        tabPanel(
          "First Visualization", 
          plotlyOutput("state_scatter"),
          text()
        ),
        tabPanel(
          "Second Visualization", 
        ),
        tabPanel(
          "Third Visualization", 
          # Application title
          titlePanel("The United States mapped by Data Science Jobs"),
          
          # Sidebar with a slider input for number of bins 
          sidebarLayout(
            sidebarPanel(
              selectInput("vars",
                          "Select a metric",
                          choices = y 
              ),
              h3("Variable Definitions"),
              h5("Jobs: Number of jobs listed for hiring"),
              h5("Average Salary: Average salary for those listed jobs"),
              h5("Average Rating: Average Company rating for a state"),
              h5("\n\nNote: States that are grey are missing data.")
            ),
            
            
            mainPanel(
              plotlyOutput("map"),
            )
          )
        ),
        tabPanel(
          "Conclusion", 
          p("Hello World!")
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$state_scatter <- renderPlotly({
    scatterplot(datasaljustin)
  })
  
  output$map <- renderPlotly({
    ix = which(y == input$vars)
    g = plot_usmap(data = datasaldavid,values = input$vars, size = 0.05) +
      scale_fill_continuous(
        low = "white", high = "dark green", name = names(y)[ix],
        label = scales::comma
      )+
      theme(legend.position = "right") 
    ggplotly(g) %>%
      style(hoveron = "fill")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)