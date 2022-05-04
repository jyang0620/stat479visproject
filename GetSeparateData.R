# Making Specific Datafiles

# Visualization 1
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

datasal2 = data.frame(datasal)[,1:16] 
write.csv(datasal2, file="justindatasal.csv", row.names = FALSE)

# Visualization 2

# Visualization 3

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
write.csv(col, file="daviddatasal.csv")
