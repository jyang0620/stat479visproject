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
      tabsetPanel(
        tabPanel(
          "Introduction", 
          strong("Introduction", style = "font-family: 'times'"), br(),
          p("According to the US Department of Labor Statistics, as of May 2020, the current average salary of a data scientist is $100,560, indicating a monetarily successful profession. In order to understand, a set of questions as potential points of interest were developed, pertaining to which states have the highest paying jobs, what is the most optimal job in terms of net earnings and which industries and/or sectors have the highest paying jobs. The aim of this project is to visualize the state of this profession, providing aid in the decision making process for students thinking about becoming data scientists in the future and the current data scientists searching for the optimal position.", style = "font-family: 'times'; font-si16pt"), br(),
          strong("Data", style = "font-family: 'times'"), br(),
          p("The data we used was scrapped from glassdoor, a review aggregation website of companies. Glassdoor allows users to anonymously rate companies, submit salaries and apply for jobs, all important for this project. The initial kaggle dataset is pre-cleaned and filtered, already containing information about salary, job descriptions and company ratings. After filtering out unwanted variables, removing those with missing data and mutating our columns, our newly processed data contains the Company Name, the city and state the job is located in, Rating, Industry, Sector, Job Title and two salary related columns: one with Average reported salary ranges and one with the calculated Average Salary based on the Salary Estimate.", style = "font-family: 'times'; font-si16pt"), br(),
          p("Additionally, we appended a column containing the cost of living index, obtaining the numbers from a secondary calculated dataset from AdvisorSmith, a small business analytics resource. The cost of living index takes the cost of living of a place and normalizes the number across the entire nation. Similar to how OPS+ works in baseball, AdvisorSmith adjusts the number on a scale where 100 represents the national average, meaning a cost of living index of 125 means that a place is 25% more expensive than an average place. As a result, this creates a standardized statistic that can be both easily grasped and visualized with.", style = "font-family: 'times'; font-si16pt"), br(),
          p("While appending to our kaggle dataframe, a major hurdle occurs as AdvisorSmith only accounts for the major cities, not the suburbs found within their respective metropolitan areas. This is most prominently seen for jobs located in the San Francisco Bay Area, as we had to discard jobs located in cities such as Palo Alto, Foster City and Santa Clara. To solve this problem, we manually added a column in both our salary and cost of living datasets of the metropolitan area of each unique city using excel. As a result, it allows us to join the two based on the metropolitan area column, filling in the missing cost of living values of the suburbs.", style = "font-family: 'times'; font-si16pt"), br(),
          p("Once we obtained our generalized cleaned data set, each visualization did their own set of transformations. The first plot required a left join to sort all states by region using the rnaturalearth package. The second required grouping the data by industry and calculating the salary values based on it. For the final visualization, the process was similar through the grouping the values by state. These resulting cleaned data sets were then plotted in the subsequent visualizations.", style = "font-family: 'times'; font-si16pt"), br(),
          p("To access our visualizations, click the headers at the top of the page!", style = "font-family: 'times'; font-si16pt")
        ),
        tabPanel(
          "First Visualization", 
          plotlyOutput("state_scatter"),
          text(), br(),
          strong("Description", style = "font-family: 'times'"), br(),
          p("The first visualization is a scatter plot that seeks to find the most optimal job in terms of salary and cost of living. It graphs the average reported salary for that specific job in that company on the x-axis and the cost of living index on the y-axis, creating a dynamic to where the ideal values are located towards the bottom right corner. Each point is colored by the region the job is based in, with the states of California, Massachusetts and New York defined as their own region. To add more interactivity for exploration and obtaining specific information about a point, a tooltip was added that brings up information regarding the specific point upon hover. Lastly, an appendix was added below the graph, defining the values of the axes and the specific states in each defined geographic region.", style = "font-family: 'times'; font-si16pt"), br(),
          p("From the graph, a key observation is that there is no correlation between the two variables, with the points plotted seemingly randomly. This is slightly counterintuitive, as one would expect the salary to rise in conjunction with living costs. Additionally, all the jobs based in the San Francisco Bay Area are plotted along a straight line, well separated from the rest of the points. Lastly, the most bottomright and ideal point represents a job for a Director of Data Science in Chicago, indicating that the most ideal position is located in the Midwest.	", style = "font-family: 'times'; font-si16pt"), br(),
        ),
        tabPanel(
          "Second Visualization", 
          titlePanel(h1("Average Salaries for Data Science Jobs", align = "center")),
          strong("Description", style = "font-family: 'times'"), br(),
          p("In this visualization, we seek to answer the question of what industries provide the highest-paying data science related jobs. In this interactive visualization, users can select industries of their interest to view and compare their relative box plots in terms of average annual salary in thousands. Boxplots are ordered by descending median values.", style = "font-family: 'times'; font-si16pt"), br(),
          p("Some findings from this visualization includes that data science jobs in financial analytics, telecommunication services, and brokerage are among the highest paying ones; while similar jobs in food and beverage manufacturing, architectural and engineering services, and social assistance industries are among the lowest paying ones.", style = "font-family: 'times'; font-si16pt"),
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
              strong("Description", style = "font-family: 'times'"), br(),
              p("This third visualization is a map that uses a continuous scale to display the average job rating, job availability and average salary. The color of the state is directly correlated to each of the metrics. The states with the higher value become a darker shade of green and have a lighter, paler shade of green for a lower value. When hovering the cursor of each state, the selected metric for that will appear. There is a legend detailing the exact scale used in the visual. An important observation from the graph is the fact that particular states are missing color.  We see this as a possible indicator that there aren’t any jobs of interest in those missing states or that a person is interested in data analytics or science.", style = "font-family: 'times'; font-si16pt"), br(),
            ),
            
          )
        ),
        tabPanel(
          "Conclusion", 
          strong("Conclusion", style = "font-family: 'times'"), br(),
          p("After creating the visuals and reviewing them, our visualizations indicate that working in the Midwestern region of the country in financial analytics, telecommunication services or brokerage is the best way to optimize personal finances when only considering cost of living, average salary, and industry of employment, even if the availability doesn't compare to other regions. On the other hand, we can conclude that while a large majority of jobs are located in California, living in the state and working in social assistance or beverage manufacturing would leave someone with a minimal amount of money in their bank account. However, it is important to remember that this conclusion is only useful for the states that were provided in the data set and listed on glassdoor.", style = "font-family: 'times'; font-si16pt"), br(),
          strong("References", style = "font-family: 'times'"), br(),
          img(src = "https://uwmadison.box.com/shared/static/tx32osgpvzn2ewuak6wcdu1y5jac4g64.png", width = 600, height = 300), br(),
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