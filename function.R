library(tidyverse)

### Data cleaning
aihw_hwe_77_data_tables <- read_excel("C:/Users/CSSHAW/Dropbox (Grattan Institute)/Pub Pte efficiency/Shiny App/Data/aihw-hwe-77-data-tables.xls", 
                                      sheet = "Table A3", range = "A4:N25", na = ". .") 
aihw_hwe_77_data_tables[aihw_hwe_77_data_tables == "—"] <- 0
data <- aihw_hwe_77_data_tables %>%
  mutate(Area = `Area of expenditure`,
         Rebate = as.numeric(aihw_hwe_77_data_tables$`Premium rebates`),
         Insurance = as.numeric(aihw_hwe_77_data_tables$`Health insurance providers`),
         Individuals = as.numeric(aihw_hwe_77_data_tables$Individuals)) %>%
  select(Area,Rebate,Insurance,Individuals,Total = Total...5)

estimated_impact <- function(i1,i2,i3,i4){

q1 = i1
q2 = ifelse(i2 == FALSE,TRUE,FALSE) 
q3 = ifelse(i3 == FALSE,TRUE,FALSE)
q4 = i4

### Calculation

## Get admin spend for Total, Insurance & Rebate
admin <- data %>%
  filter(Area == "Administration")

## Get out of pocket for individual 
OOP_spend <- data %>%
  filter(Area %in% c("Hospitals"))  %>%
  select(Individuals) %>%
  group_by() %>%
  summarise(sum = sum(Individuals))

## Get extras spend for Insurance & Rebate
extras_spend <- data %>%
  filter(Area %in% c("Primary health care","Other services")) %>%
  group_by() %>%
  summarise(rebate_extras = sum(Rebate),
            insurance_extras = sum(Insurance))

# Get total spend for all
total_spend <- data %>%
  filter(Area == "Total health expenditure")

## Total rebates - q2*Extras
Rebate    <- total_spend$Rebate - as.numeric(q2)*extras_spend$rebate_extras 
## Total insurance - q3*Extras - q4*Out of Pocket - admin adjustment
## Admin Adjustment = (Insurance Admin Proportion - Total Admin Proportion)*Insurance Admin
Insurance <- (total_spend$Insurance - 
                as.numeric(q3)*extras_spend$insurance_extras + 
                as.numeric(q4)*OOP_spend -
                (admin$Insurance/total_spend$Insurance - admin$Total/total_spend$Total) * admin$Insurance) 
answer    <- ((45-q1)/45)*(Insurance/Rebate)
return(answer$sum)
}

