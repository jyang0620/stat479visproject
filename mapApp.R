
library(readr)
library(ggplot2)
library(sf)
library(usmap)
library(tidyverse)
library(plotly)
library(rnaturalearth)

library(shiny)
data = read_csv("https://github.com/jyang0620/stat479visproject/raw/main/DSsalary.csv")
data$state = data$State
counts = as.data.frame(table(data$State))
data = inner_join(data, counts, c("state" = "Var1"))
col = as.data.frame( data %>%
                         group_by(state) %>% 
                         summarise(
                             avg_salary = round(mean(`Avg Salary(K)`),3),
                             cost_of_living = round(mean(cost.of.living), 1),
                             avg_rating = round(mean(Rating),2),
                             availability = Freq
                             
                         )) %>% distinct()
# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    titlePanel("Breaking Values Down by State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("vars",
                        "Select a metric",
                        c("availability", "avg_rating", 
                          "avg_salary", "cost_of_living")
            ),
            h3("Variable Definitions"),
            h5("availability: Number of jobs listed for hiring"),
            h5("Cost_of_living: The cost of living normalized across the entire nation, where 100 is the national average."),
            h5("avg_salary: Average salary"),
            h5("avg_ratign: Average job rating"),
            h5("States that are grey are missing data.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("map"),
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlotly({
        g = plot_usmap(data = col,values = input$vars, color = "dark green") +
            scale_fill_continuous(
                low = "white", high = "dark green", name = input$vars,
                label = scales::comma
            )+
            theme(legend.position = "right")
        ggplotly(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
