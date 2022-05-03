library(scales)
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
                             Jobs = Freq,
                             Average_salary = round(mean(`Avg Salary(K)`),3)*1000,
                             Average_rating = round(mean(Rating),2)
                             
                         )) %>% distinct()


ui <- fluidPage(
    
    # Application title
    titlePanel("Breaking Values Down by State"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("vars",
                        "Select a metric",
                        colnames(col)[2:4]
            ),
            h3("Variable Definitions"),
            h5("Jobs: Number of jobs listed for hiring"),
            h5("Average_salary: Average salary for those listed jobs"),
            h5("Average_rating: Average Company rating for a state"),
            h4("\n\nNote: States that are grey are missing data.")
        ),
        
        
        mainPanel(
            plotlyOutput("map"),
            
            
        )
    )
)


server <- function(input, output) {
    output$map <- renderPlotly({
        g = plot_usmap(data = col,values = input$vars, color = "dark green") +
            scale_fill_continuous(
                low = "white", high = "dark green", name = input$vars,
                label = scales::comma
            )+
            theme(legend.position = "right")
        ggplotly(g) %>%
            style(hoveron = "fill")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
