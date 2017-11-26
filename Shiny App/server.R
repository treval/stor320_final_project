library(tidyverse)
library(ggplot2)

healthcare <- read_csv("tidied_data.csv")
Covered_By_State <- read_csv("by_state.csv")
Covered_By_Region <- read_csv("by_region.csv")
Cost_By_Disease <- read_csv("by_disease.csv")

plot_states_avg_tot_charge <- function(state_array) {
  df <- Covered_By_State %>% 
    filter(`Provider State` %in% state_array)
  
  return(ggplot(df) + 
           geom_col(mapping=aes(x=`Provider State`, y=`State Average Total Charges`), fill="#99badd") + 
           ylim(0,80000))
}

plot_states_cov_ratio <- function(state_array) {
  df <- Covered_By_State %>% 
    filter(`Provider State` %in% state_array)
  
  return(ggplot(df) + 
           geom_col(mapping=aes(x=`Provider State`, y=`State Covered Ratio`), fill="#99badd") + 
           ylim(0,1))
}

plot_region_avg_covered <- function(region_array) {
  df <- Covered_By_Region %>%
    filter(Regions %in% region_array)
  
  return(ggplot(df) +
           geom_col(mapping=aes(x=`Regions`, y=Average_Covered), fill="#99badd") +
           ylim(0,60000))
}

plot_region_cov_ratio <- function(region_array) {
  df <- Covered_By_Region %>%
    filter(Regions %in% region_array)
  
  return(ggplot(df) +
           geom_col(mapping=aes(x=`Regions`, y=Covered_Ratio), fill="#99badd") +
           ylim(0,1))
}

plot_avg_disease_cost <- function(disease_array) {
  df <- Cost_By_Disease %>%
    filter(DRG_id %in% disease_array)
  
  return(ggplot(df) +
           geom_col(mapping=aes(x=`DRG_id`, y=`Country Average Payment`), fill="#99badd") +
           ylim(0,50000))
}

shinyServer(
  function(input, output) {
    output$state_avg_plot<-renderPlot(
      expr=plot_states_avg_tot_charge(input$in_states)
    )
    
    output$state_ratio_plot<-renderPlot(
      expr=plot_states_cov_ratio(input$in_states)
    )
    
    output$region_avg_plot<-renderPlot(
      expr=plot_region_avg_covered(input$in_region)
    )
    
    output$region_cov_ratio_plot<-renderPlot(
      expr=plot_region_cov_ratio(input$in_region)
    )
    
    output$disease_avg_cost_plot<-renderPlot(
      expr=plot_avg_disease_cost(input$in_diseases)
    )
  }
)