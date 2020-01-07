
# runApp("~/Development/inc5000_companies/app.R")

library(tidyverse)
library(shiny)
library(gghighlight)

########################################################################
############################## PARSE DATA ##############################
########################################################################

company_data <- read_csv("companies_data_final.csv")

# INDUSTRY AGGREGATE
company_parsed <- company_data %>% 
  group_by(industry) %>% 
  summarize(revenue_millions = sum(revenue_millions),
            count = n()) %>% 
  ungroup() %>% 
  filter(!is.na(revenue_millions)) %>% 
  mutate(pct_count = count / sum(count),
         pct_revenue = revenue_millions / sum(revenue_millions)) 

# INDUSTRIES INDEX
industries <- company_data %>% 
  select(industry) %>% 
  distinct() %>% 
  filter(!grepl(",", industry)) %>% 
  mutate(x = row_number()) %>% 
  spread(industry, x)

# TOP COMPANIES BY REVENUE
top_companies <- company_data %>% 
  mutate(employees = as.numeric(sub(",", "", employees)),
         revenue_millions = as.numeric(revenue_millions),
         revenue_per_employee = round(revenue_millions / employees, 2)) 

############################################################################
############################## USER INTERFACE ##############################
############################################################################

ui <- fluidPage(

  titlePanel("Inc.5000 Fastest Growing Private Companies"),
  
  br(),

  sidebarLayout(
    
    sidebarPanel(
      
      p("This simple web app was built in R with the", a(href = "https://shiny.rstudio.com/", "Shiny package.", target = "_blank")),
      p("Its purpose is to answer the question:", em("how much revenue did each industry generate in 2019 and how many companies were surveyed?")),
      p("For details on the data scraping and cleaning process you can", a(href = "https://www.christopheryee.org/blog/top-industries-from-inc-5000-fastest-growing-companies-in-2019/", 
                                "read more about that here.", target = "_blank")),
      
      br(),
      varSelectInput("industry_variable", "Select Industry:", industries, selected = "Health"),  
      
      br(),
      strong("Data Source"),
      tags$ul(
        tags$li("Original:", a(href = "https://www.inc.com/inc5000/2019/top-private-companies-2019-inc5000.html", "Inc. 5000", target = "_blank")),
        tags$li("Enhanced:", a(href = "https://twitter.com/viperchill", "@viperchill", target = "_blank"))
      ),
      
      strong("Author"),
      tags$ul(
        tags$li("Twitter:", a(href = "https://twitter.com/eeysirhc", "@eeysirhc", target = "_blank")),
        tags$li("Website:", a(href = "https://www.christopheryee.org", "www.christopheryee.org", target = "_blank"))
      ),
      width = 3),

    mainPanel(
      plotOutput("industry_comparison"),
      DT::dataTableOutput("industry_datagrid")
    )
  )

)

####################################################################
############################## SERVER ##############################
####################################################################

server <- function(input, output, session) {
  
##### INDUSTRIES SURVEYED x TOTAL REVENUE
  output$industry_comparison <- renderPlot({
    company_parsed %>%
      ggplot(aes(pct_count, pct_revenue, label = industry)) +
      geom_point() +
      geom_text(vjust = 1, hjust = 1) +
      gghighlight(industry == input$industry_variable) +
      geom_abline(color = 'salmon', lty = 'dashed') + 
      scale_x_log10(labels = scales::percent_format(round(1))) +
      scale_y_log10(labels = scales::percent_format(round(1))) +
      labs(x = "% of Total Companies (log)", y = "% of Total Revenue (log)") +
      theme_minimal(base_size = 15)
  })
  
##### TOP COMPANIES BY INDUSTRY
  output$industry_datagrid <- DT::renderDataTable({
    top_companies %>% 
      filter(industry == input$industry_variable) %>% 
      arrange(desc(revenue_per_employee)) %>% 
      select(page_url, website, employees, revenue_millions, revenue_per_employee)
  }, options = list(pageLength = 20))  
}

#########################################################################
############################## APPLICATION ##############################
#########################################################################

shinyApp(ui, server)
