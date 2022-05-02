library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats)
library(lubridate)
library(plotly)
library(shiny)

options(warn=-1)
datasal = read_csv("DSsalary.csv")

datasal = datasal %>%
  separate(`Company Name`, c('Company', "Rating"), sep = "\n")
  
datasal2 = data.frame(datasal)

# rnaturalearth notes
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
states <- ne_states("United States of America") %>%
  st_as_sf() %>%
  select(region, postal) 

datasal = datasal %>%
  left_join(states, by = c("State" = "postal"))

for (i in seq(length(datasal$State))) {
  if (datasal$State[i] == "CA") {
    datasal$region[i] = "California"
  }
  if (datasal$State[i] == "MA") {
    datasal$region[i] = "Massachusetts"
  }
  if (datasal$State[i] == "NY") {
    datasal$region[i] = "New York"
  }
}
            
datasal2 = data.frame(datasal)            

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

ui = fluidPage(
  plotlyOutput("state_scatter"),
  text()
  
)

server = function(input, output) {
  output$state_scatter <- renderPlotly({
    scatterplot(datasal2)
  })
}

shinyApp(ui, server)
