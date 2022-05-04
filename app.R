library(tidyverse)
library(naniar)
library(RColorBrewer)
theme_set(theme_bw(16))

ds_salfix <- read_csv("DSsalary.csv")


ds_salfix <- ds_salfix %>% 
    rename(Avg.Salary.K = 'Avg Salary(K)',
           Job.Title = 'Job Title') %>% # Suggestion: Trim names of job titles (1st 10 characters or manually)
    mutate(Avg.Salary.K = as.numeric(Avg.Salary.K)) %>% 
    replace_with_na_all(condition = ~.x == -1)

industry <- pull(ds_salfix, Industry) %>%
    unique() %>%
    na.omit()
    
industry <- sort(industry)

small_samples <- ds_salfix %>% 
    group_by(Industry) %>% 
    summarize(n = n()) %>% 
    arrange(n) %>% 
    filter(n<= 5) %>% 
    pull(Industry)


ds_salfix <- ds_salfix %>% 
    mutate(Industry = replace(Industry, Industry %in% small_samples, "Other"))


# Boxplot
box <- function(df){    
    ggplot(data = df %>% filter(selected)) +
        geom_boxplot(aes(x = Avg.Salary.K, 
                         y = fct_reorder(Industry, Avg.Salary.K, median)
                         # fill = Industry
                         )) + 
        facet_wrap(~Sector) +#suggestion: reorder box plots
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
ui <- fluidPage(
    titlePanel(h1("Average Salaries for Data Science Jobs", align = "center")),
    sidebarLayout(
        sidebarPanel(selectInput("industry", "Industry", industry, multiple = TRUE)),
        mainPanel(plotOutput("boxplot"))
    )
)

server <- function(input, output){
    df_subset <- reactive({
        ds_salfix %>% mutate(
            selected = (
                (Industry %in% input$industry)
            ))
    })
    output$boxplot = renderPlot(box(df_subset()), height= 1200, width = 800)
}

shinyApp(ui, server)