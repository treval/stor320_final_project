library(tidyverse)
library(ggplot2)

shinyUI(
  
  fluidPage(
    
    sidebarLayout(
      
      sidebarPanel(
        width=3,
        titlePanel("Healthcare Variation in the US"),
        p("This app can be used to explore variation in healthcare costs and amount of health insurance 
          coverage for regional, statewise, and citywise scopes. Select which locations you would like
          to view data for, and if any differences pop out, you can run a significance test for
          difference between two means."),
        br(),
        p("This version does not have all selection functionality or final versions of the plots shown."),
        selectInput(
          inputId="scope",
          label="Select Scope",
          choices = c("Regions", "States", "Cities")
        ),
        selectInput(
          inputId="metric",
          label="Select Metric",
          choices = colnames(x=Covered_By_State)[6:8]
        ),
        checkboxGroupInput(
          inputId="in_states",
          label="States",
          choices=unique(healthcare$`Provider State`),
          width="300px",
          inline=TRUE
        ),
        checkboxGroupInput(
          inputId="in_region",
          label="Regions",
          choices=unique(Covered_By_Region$Regions),
          inline=TRUE,
          width="200px"
        )
      ),
      
      mainPanel(
        
        fluidRow(
          titlePanel("Map and Summary Statistics"), 
            column(
              titlePanel("Map"),
              p("The plan is for this to highlight the selection of either state or region with some R GIS package"),
              width=6,
              tags$img(src="blank_map.png", width="400px", height="250px")
            ),
            column(
              titlePanel("Summary Statistics"),
              width=6,
              selectInput(
                inputId="summarize_state",
                label="State",
                choices=unique(healthcare$`Provider State`)
              ),
              p("Summary statistics for State or Region selected will go here.")
            )
        ),
        
        fluidRow(
          titlePanel("State Scope"),
          column(
            titlePanel("State Average Metric Plot"),
            width = 4, 
            plotOutput("state_avg_plot")
          ),
          column(
            width=4,
            titlePanel("State Coverage Percentage Plot"),
            plotOutput("state_ratio_plot")
          ),
          column(
            width=4,
            titlePanel("T-Test")
          )
        ),
        
        fluidRow(
          titlePanel("Region Scope"),
          column(
            width=4,
            titlePanel("Region Average Metric Plot"),
            plotOutput("region_avg_plot")
          ),
          column(
            width=4,
            titlePanel("Region Coverage Percentage Plot"),
            plotOutput("region_cov_ratio_plot")
          ),
          column(
            width=4,
            titlePanel("T-Test")
          )
        ),
        
        fluidRow(
          titlePanel("Procedure Cost Exploration"),
          column(
            width=4,
            checkboxGroupInput(
              inputId="in_diseases",
              label="Diseases",
              choices=unique(Cost_By_Disease$DRG_id),
              inline=TRUE
            )
          ),
          column(
            width=4,
            titlePanel("Procedure Costs"),
            plotOutput("disease_avg_cost_plot")
          )
        )
        
      )
    )
  )
)
