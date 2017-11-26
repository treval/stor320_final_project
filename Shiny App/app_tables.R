library(tidyverse)

#Tidy original dataset
healthcare <- read_csv("app_data.csv")
healthcare$`Average Covered Charges` <- gsub("\\$", "", healthcare$`Average Covered Charges`) %>% as.numeric()
healthcare$`Average Medicare Payments` <- gsub("\\$", "", healthcare$`Average Medicare Payments`) %>% as.numeric()
healthcare$`Average Total Payments` <- gsub("\\$", "", healthcare$`Average Total Payments`) %>% as.numeric()
healthcare <- healthcare %>% separate(`DRG Definition`, into=c("DRG_id","DRG_Def"), sep="-")

healthcare <- healthcare %>% arrange(`Provider State`)
healthcare <- mutate(healthcare, Total_Covered_Charges = `Average Covered Charges` * `Total Discharges`)
healthcare <- mutate(healthcare, `Total Payments` = `Average Total Payments` * `Total Discharges`)

write_csv(healthcare, "./tidied_data.csv")

#state aggregation
healthcare <- mutate(healthcare, Total_Covered_Charges = `Average Covered Charges` * `Total Discharges`)
healthcare <- mutate(healthcare, `Total Payments` = `Average Total Payments` * `Total Discharges`)

Covered_By_State <- healthcare %>% 
  group_by(`Provider State`) %>% 
  select(`Provider State`, `Total Discharges`, `Total Payments`, Total_Covered_Charges) %>%
  summarise(
    `State Total Covered` = sum(`Total_Covered_Charges`),
    `State Total Discharges` = sum(`Total Discharges`),
    `State Average Covered` = `State Total Covered`/`State Total Discharges`,
    `State Total Payment` = sum(`Total Payments`),
    `State Average Payment` = `State Total Payment`/`State Total Discharges`, 
    `State Average Total Charges` = `State Average Covered` + `State Average Payment`,
    `State Covered Ratio` = `State Average Covered` / `State Average Total Charges` 
  )

write_csv(Covered_By_State, "./by_state.csv")

#region aggregation
#West: WA,OR,CA,NV,ID,UT,MT,WY,CO,AZ,NM,AK,HI
west_states <- c("WA", "OR", "CA", "NV", "ID", "UT", "MT", "WY", "CO", "AZ", "NM", "AK", "HI")
#Midwest: ND,SD,NE,KS,MN,IA,MO,WI,IL,IN,MI,OH
midwest_states <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
#Northeast: NY,PA,NH,VT,ME,MA,RI,CT,NJ
northeast_states <- c("NY", "PA", "NH", "VT", "ME", "MA", "RI", "CT", "NJ")
#South: OK,TX,AR,LA,MS,TN,KY,AL,WV,DE,MD,DC,VA,NC,SC,GA,FL
south_states <- c("OK", "TX", "AR", "LA", "MS", "TN", "KY", "AL", "WV", "DE", "MD", "DC", "VA", "NC", "SC", "GA", "FL")

med_cost_west <- filter(Covered_By_State, `Provider State` %in% west_states)
med_cost_west <- mutate(med_cost_west, Region="West")
west_average <- sum(med_cost_west$`State Total Payment`)/sum(med_cost_west$`State Total Discharges`)

Covered_West <- filter(Covered_By_State, `Provider State` %in% west_states)
Covered_West <- mutate(Covered_West, Region="West")
Average_Covered_West = sum(Covered_West$`State Total Covered`)/sum(Covered_West$`State Total Discharges`)
Total_Charges_West = Average_Covered_West + west_average
Covered_West_Ratio = Average_Covered_West/Total_Charges_West

med_cost_midwest <- filter(Covered_By_State, `Provider State` %in% midwest_states)
med_cost_midwest <- mutate(med_cost_midwest, Region="Midwest")
midwest_average <- sum(med_cost_midwest$`State Total Payment`)/sum(med_cost_midwest$`State Total Discharges`)

Covered_Midwest <- filter(Covered_By_State, `Provider State` %in% midwest_states)
Covered_Midwest <- mutate(Covered_Midwest, Region="Midwest")
Average_Covered_Midwest = sum(Covered_Midwest$`State Total Covered`)/sum(Covered_Midwest$`State Total Discharges`)
Total_Charges_Midwest = Average_Covered_Midwest + midwest_average
Covered_Midwest_Ratio = Average_Covered_Midwest/Total_Charges_Midwest

med_cost_northeast <- filter(Covered_By_State, `Provider State` %in% northeast_states)
med_cost_northeast <- mutate(med_cost_northeast, Region="Northeast")
northeast_average <- sum(med_cost_northeast$`State Total Payment`)/sum(med_cost_northeast$`State Total Discharges`)

Covered_Northeast <- filter(Covered_By_State, `Provider State` %in% northeast_states)
Covered_Northeast <- mutate(Covered_Northeast, Region="Northeast")
Average_Covered_Northeast = sum(Covered_Northeast$`State Total Covered`)/sum(Covered_Northeast$`State Total Discharges`)
Total_Charges_Northeast = Average_Covered_Northeast + northeast_average
Covered_Northeast_Ratio = Average_Covered_Northeast/Total_Charges_Northeast

med_cost_south <- filter(Covered_By_State, `Provider State` %in% south_states)
med_cost_south <- mutate(med_cost_south, Region="South")
south_average <- sum(med_cost_south$`State Total Payment`)/sum(med_cost_south$`State Total Discharges`)

Covered_South <- filter(Covered_By_State, `Provider State` %in% south_states)
Covered_South  <- mutate(Covered_South , Region="South")
Average_Covered_South = sum(Covered_South$`State Total Covered`)/sum(Covered_South$`State Total Discharges`)
Total_Charges_South = Average_Covered_South + south_average
Covered_South_Ratio = Average_Covered_South/Total_Charges_South

Regions <- c("West", "Midwest", "Northeast", "South")
Average_Payment <- c(west_average, midwest_average, northeast_average, south_average)
Average_Covered <- c(Average_Covered_West, Average_Covered_Midwest, Average_Covered_Northeast, Average_Covered_South)
Covered_Ratio <- c(Covered_West_Ratio, Covered_Midwest_Ratio, Covered_Northeast_Ratio, Covered_South_Ratio)
Covered_By_Region <- data.frame(Regions, Average_Payment, Average_Covered, Covered_Ratio)

write_csv(Covered_By_Region, "./by_region.csv")

#disease aggregation
disease_country <- healthcare %>% 
  group_by(DRG_id) %>% 
  select(DRG_id, `Total Discharges`, `Total Payments`) %>%
  summarise(
    `Country Total Payments` = sum(`Total Payments`),
    `Country Total Discharges` = sum(`Total Discharges`),
    `Country Average Payment` = `Country Total Payments`/`Country Total Discharges` 
  )

write_csv(disease_country, "./by_disease.csv")
