library(readr)
library(tidyverse)
library(ggplot2)
library(forcats)
library(lubridate)
library(plotly)
library(shiny)

datasal = read_csv("DSsalary.csv")

datasal = datasal %>%
  separate(`Company Name`, c('Company', "Rating"), sep = "\n")
  
datasal2 = data.frame(datasal)

# rnaturalearth notes
library(rnaturalearth)
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
         color = "Region")
  ggplotly(p, tooltip = "text") %>%
    style(hoveron = "fill")
}

ui = fluidPage(
  plotlyOutput("state_scatter")
)

server = function(input, output) {
  output$state_scatter <- renderPlotly({
    scatterplot(datasal2)
  })
}

shinyApp(ui, server)
