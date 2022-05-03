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
y = colnames(col)[2:4]
names(y) = c("Jobs","Average Salary","Average Rating")

ui <- fluidPage(
    
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
)


server <- function(input, output) {
    output$map <- renderPlotly({
        ix = which(y == input$vars)
        g = plot_usmap(data = col,values = input$vars, size = 0.05) +
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
