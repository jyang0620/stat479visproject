library(scales)
library(readr)
library(ggplot2)
library(ggthemes)
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
library(shinythemes)
library(bslib)

# First visualization
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

#Second Visualization
datasalyuning = read_csv("yuningatasal.csv")

industry <- pull(datasalyuning, Industry) %>%
  unique() %>%
  na.omit()

box <- function(df){    
  ggplot(data = df %>% filter(selected)) +
    geom_boxplot(aes(x = Avg.Salary.K, 
                     y = fct_reorder(Industry, Avg.Salary.K, median),
                     fill = "gray69", alpha=0.2)) + 
    # facet_wrap(~Sector) +#suggestion: reorder box plots
    theme(legend.position="none",
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18)) +
    # scale_fill_brewer(palette = "Spectral")+
    labs(title= "Average Salaries vs Industry", 
         x= "Average Salary in Thousands",
         y= "Industry")
}

#Third Visualization
datasaldavid = read_csv("daviddatasal.csv")
y = colnames(datasaldavid)[3:5]
names(y) = c("Jobs","Average Salary","Average Rating")



ui <- fluidPage(
    theme= shinytheme("united"),
    
    mainPanel(
      "Data Science Salaries", 
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
          titlePanel(h1("Average Salaries for Data Science Jobs", align = "center")),
          sidebarLayout(
            sidebarPanel(selectInput("industry", 
                                     "Industry", 
                                     industry, 
                                     selected = industry[1:5],
                                     multiple = TRUE)),
            mainPanel(plotOutput("boxplot"))
          )
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
  
  df_subset <- reactive({
    datasalyuning %>% mutate(
      selected = (
        (Industry %in% input$industry)
      ))
  })
  output$boxplot = renderPlot(box(df_subset()), height= 1200, width = 800)
  
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