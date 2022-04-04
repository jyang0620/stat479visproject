library(tidyverse)

# Read in data
ds_sal = read_csv("https://uwmadison.box.com/shared/static/l2n9u9d97yxzibvd71y7a370kt0pj1pu.csv")
costoliv = read_csv("https://uwmadison.box.com/shared/static/6xphn35fqqq8svwr3y9iis1cpxjqfo8b.csv")

# Separating Location into City and State
ds_salfix = ds_sal %>%
  separate(Location, c('City','State'),sep = ', ')
ds_salfix[127,]$City = 'Los Angeles'
ds_salfix[127,]$State = 'CA'

# Filtering columns to our desired variables
ds_salfix = ds_salfix[,c(1:3,5:8,13:14,19:21)]
ds_salfix$cost.of.living = NA #empty column

ds_salfix <- ds_salfix %>% 
  rename(Avg.Salary.K = 'Avg Salary(K)',
         Job.Title = 'Job Title') %>% # Suggestion: Trim names of job titles (1st 10 characters or manually)
  mutate(Avg.Salary.K = as.numeric(Avg.Salary.K))

# Boxplot
box = function(salary, x){    
  # salary = filter(salary, selected == TRUE)
  print(salary)
  ggplot(salary) +
    geom_boxplot(aes_string(x = "Avg.Salary.K", 
                     y = x)) + #suggestion: reorder box plots
                     # fill=x)) +
    theme(legend.position="bottom") + 
    labs(title= "Boxplot of Average Salaries", 
         x= "Average Salary (K)",
         y= x)
}
ui = fluidPage(
  titlePanel(h1("Average Salaries for Data Science Jobs", align = "center")),
  inputPanel(
    selectInput("x_var", "Select a variable that you want to compare average salaries", 
                c("State" = "State", "Job Title" = "Job.Title"), selected = "State")
  ),
  mainPanel(plotOutput("boxplot"))
)

server = function(input, output){
  output$boxplot = renderPlot({box(ds_salfix, input$x_var)})
}

app <- shinyApp(ui, server)
