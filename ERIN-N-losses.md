ERIN N losses
================
2023-06-09

This document takes in values from a .csv file separated into two
specific documents, constants, and inputs. Constant values include
emission factors for specific N losses and inputs are primarily N loads
(or N inputs) of each contributor to N losses.

``` r
#Note: Input correct path address for Constants and Inputs
Data_constants <- read.csv(file = '/Users/adityaswarup/Library/CloudStorage/OneDrive-UniversityofWaterloo/ERIN Constants.csv', header = FALSE, sep = ',', dec = '.')

Data_inputs <- read.csv(file = '/Users/adityaswarup/Library/CloudStorage/OneDrive-UniversityofWaterloo/ERIN Inputs.csv', header = FALSE, sep = ',', dec = '.') 

Days_in_month <- Data_constants[4:5, 1:13]

#Emission factors if drainage = Poor. Ensure correct dataframe is referenced if .csv file is edited.
NO3_EF_Poor <- Data_constants[10:16,1:13]
NH3_EF_Poor <- Data_constants[18:31,1:13]
N2O_EF_Poor <- Data_constants[33:45,1:13]
N2_EF_Poor <- Data_constants[47:53,1:13]

#Emission factors if drainage = Moderate. Ensure correct dataframe is referenced if .csv file is edited.
NO3_EF_Moderate <- Data_constants[56:62,1:13]
NH3_EF_Moderate <- Data_constants[64:77,1:13]
N2O_EF_Moderate <- Data_constants[79:91,1:13]
N2_EF_Moderate <- Data_constants[93:99,1:13]

#Emission factors if drainage = Well. Ensure correct dataframe is referenced if .csv file is edited.
NO3_EF_Well <- Data_constants[102:108,1:13]
NH3_EF_Well <- Data_constants[110:123,1:13]
N2O_EF_Well <- Data_constants[125:137,1:13]
N2_EF_Well <- Data_constants[139:145,1:13]

#TAN constant for Slurry and Soiled water NH3 emissions. Ensure correct dataframe is referenced if .csv file is edited.
TAN_Slurry <- as.numeric(Data_constants[7, 2])

#N load values, main input. Ensure correct dataframe is referenced if .csv file is edited.
N_load_table <- Data_inputs[9:17,1:13]

#Area and drainage values for each grazing area. Ensure correct dataframe is referenced if .csv file is edited.
grassland_management <- Data_inputs[29:33, 1:3]
area_grazing <- as.numeric(grassland_management[2,2])
area_1cut <- as.numeric(grassland_management[3,2])
area_2cut <- as.numeric(grassland_management[4,2])
area_total <- area_grazing + area_1cut + area_2cut

#Livestock data. Ensure correct dataframe is referenced if .csv file is edited.
Livestock_data <- Data_inputs[1:7, 1:13]

Slurry_soiled_water_storage_data <- Data_inputs[25:27,1:3]

EF_grazing <- function() { #function to get EF for grazing only
  if (grassland_management[grassland_management[,1] == "grazing only", 3] == "Poor") {
    NO3_EF_grazing <<- NO3_EF_Poor
    NH3_EF_grazing <<- NH3_EF_Poor
    N2O_EF_grazing <<- N2O_EF_Poor
    N2_EF_grazing <<- N2_EF_Poor
  }
  else if (grassland_management[grassland_management[,1] == "grazing only", 3] == "Moderate") {
    NO3_EF_grazing <<- NO3_EF_Moderate
    NH3_EF_grazing <<- NH3_EF_Moderate
    N2O_EF_grazing <<- N2O_EF_Moderate
    N2_EF_grazing <<- N2_EF_Moderate
  }
  else {
    NO3_EF_grazing <<- NO3_EF_Well
    NH3_EF_grazing <<- NH3_EF_Well
    N2O_EF_grazing <<- N2O_EF_Well
    N2_EF_grazing <<- N2_EF_Well
  }
}

EF_1cut <- function() { # function to get EF for 1 cut + grazing
  if (grassland_management[grassland_management[,1] == "1 cut + grazing", 3] == "Poor") {
    NO3_EF_1cut <<- NO3_EF_Poor
    NH3_EF_1cut <<- NH3_EF_Poor
    N2O_EF_1cut <<- N2O_EF_Poor
    N2_EF_1cut <<- N2_EF_Poor
  }
  else if (grassland_management[grassland_management[,1] == "1 cut + grazing", 3] == "Moderate") {
    NO3_EF_1cut <<- NO3_EF_Moderate
    NH3_EF_1cut <<- NH3_EF_Moderate
    N2O_EF_1cut <<- N2O_EF_Moderate
    N2_EF_1cut <<- N2_EF_Moderate
  }
  else {
    NO3_EF_1cut <<- NO3_EF_Well
    NH3_EF_1cut <<- NH3_EF_Well
    N2O_EF_1cut <<- N2O_EF_Well
    N2_EF_1cut <<- N2_EF_Well
  }
}

EF_2cut <- function() { #function to get EF for 2 cut + grazing
  if (grassland_management[grassland_management[,1] == "2 cut + grazing", 3] == "Poor") {
    NO3_EF_2cut <<- NO3_EF_Poor
    NH3_EF_2cut <<- NH3_EF_Poor
    N2O_EF_2cut <<- N2O_EF_Poor
    N2_EF_2cut <<- N2_EF_Poor
  }
  else if (grassland_management[grassland_management[,1] == "2 cut + grazing", 3] == "Moderate") {
    NO3_EF_2cut <<- NO3_EF_Moderate
    NH3_EF_2cut <<- NH3_EF_Moderate
    N2O_EF_2cut <<- N2O_EF_Moderate
    N2_EF_2cut <<- N2_EF_Moderate
  }
  else {
    NO3_EF_2cut <<- NO3_EF_Well
    NH3_EF_2cut <<- NH3_EF_Well
    N2O_EF_2cut <<- N2O_EF_Well
    N2_EF_2cut <<- N2_EF_Well
  }
}

EF_grazing() #Calls the function and applies appropriate EF for respective drainage values (For grazing only)
EF_1cut() #Calls the function and applies appropriate EF for respective drainage values (For 1 cut + grazing)
EF_2cut() #Calls the function and applies appropriate EF for respective drainage values (For 2 cut + grazing)
```

NO3 Emissions:

``` r
Urine_loss_NO3 <- function() { #Gets NO3 emissions from urine.
  
  Urine_N_load <- N_load_table[N_load_table[,1] == "Urine",] #Gets N load for urine. 
  
  NO3_EF_grazing_Urine <- NO3_EF_grazing[NO3_EF_grazing[,1] == "Urine", ] #Gets EF for urine.
  NO3_EF_1cut_Urine <- NO3_EF_1cut[NO3_EF_1cut[,1] == "Urine", ] 
  NO3_EF_2cut_Urine <- NO3_EF_2cut[NO3_EF_2cut[,1] == "Urine", ] 
  
  Urine_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Urine_NO3_emission_1cut <- 0
  Urine_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Urine_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NO3_emission_grazing <- Urine_NO3_emission_grazing + Urine_NO3_emission_grazing_calc
    
    Urine_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NO3_emission_1cut <- Urine_NO3_emission_1cut + Urine_NO3_emission_1cut_calc
    
    Urine_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NO3_emission_2cut <- Urine_NO3_emission_2cut + Urine_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Urine_NO3_emission <<- (Urine_NO3_emission_grazing + Urine_NO3_emission_1cut + Urine_NO3_emission_2cut) / area_total
  
}

Urea_loss_NO3 <- function() { #Gets NO3 emissions from urea.
  
  Urea_N_load <- N_load_table[N_load_table[,1] == "Urea",] #Gets N load for urea.
  
  NO3_EF_grazing_Urea <- NO3_EF_grazing[NO3_EF_grazing[,1] == "Urea", ] #Gets EF for urea.
  NO3_EF_1cut_Urea <- NO3_EF_1cut[NO3_EF_1cut[,1] == "Urea", ]
  NO3_EF_2cut_Urea <- NO3_EF_2cut[NO3_EF_2cut[,1] == "Urea", ]
  
  Urea_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Urea_NO3_emission_1cut <- 0
  Urea_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Urea_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NO3_emission_grazing <- Urea_NO3_emission_grazing + Urea_NO3_emission_grazing_calc
    
    Urea_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NO3_emission_1cut <- Urea_NO3_emission_1cut + Urea_NO3_emission_1cut_calc
    
    Urea_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NO3_emission_2cut <- Urea_NO3_emission_2cut + Urea_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Urea_NO3_emission <<- (Urea_NO3_emission_grazing + Urea_NO3_emission_1cut + Urea_NO3_emission_2cut) / area_total
  
}

CAN_loss_NO3 <- function() { #Gets NO3 emissions from CAN.
  
  CAN_N_load <- N_load_table[N_load_table[,1] == "CAN",] #Gets N load for CAN.
  
  NO3_EF_grazing_CAN <- NO3_EF_grazing[NO3_EF_grazing[,1] == "CAN", ] #Gets EF for CAN.
  NO3_EF_1cut_CAN <- NO3_EF_1cut[NO3_EF_1cut[,1] == "CAN", ]
  NO3_EF_2cut_CAN <- NO3_EF_2cut[NO3_EF_2cut[,1] == "CAN", ]
  
  CAN_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  CAN_NO3_emission_1cut <- 0
  CAN_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    CAN_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NO3_emission_grazing <- CAN_NO3_emission_grazing + CAN_NO3_emission_grazing_calc
    
    CAN_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NO3_emission_1cut <- CAN_NO3_emission_1cut + CAN_NO3_emission_1cut_calc
    
    CAN_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NO3_emission_2cut <- CAN_NO3_emission_2cut + CAN_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  CAN_NO3_emission <<- (CAN_NO3_emission_grazing + CAN_NO3_emission_1cut + CAN_NO3_emission_2cut) / area_total
  
}

Dung_loss_NO3 <- function() { #Gets NO3 emissions from dung. 
  
  Dung_N_load <- N_load_table[N_load_table[,1] == "Dung",] #Gets N load for dung.
  
  NO3_EF_grazing_Dung <- NO3_EF_grazing[NO3_EF_grazing[,1] == "Dung", ] #Gets EF for dung.
  NO3_EF_1cut_Dung <- NO3_EF_1cut[NO3_EF_1cut[,1] == "Dung", ]
  NO3_EF_2cut_Dung <- NO3_EF_2cut[NO3_EF_2cut[,1] == "Dung", ]
  
  Dung_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Dung_NO3_emission_1cut <- 0
  Dung_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Dung_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NO3_emission_grazing <- Dung_NO3_emission_grazing + Dung_NO3_emission_grazing_calc
    
    Dung_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NO3_emission_1cut <- Dung_NO3_emission_1cut + Dung_NO3_emission_1cut_calc
    
    Dung_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NO3_emission_2cut <- Dung_NO3_emission_2cut + Dung_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Dung_NO3_emission <<- (Dung_NO3_emission_grazing + Dung_NO3_emission_1cut + Dung_NO3_emission_2cut) / area_total
  
}

Slurry_Spreading_loss_NO3 <- function() { #Gets NO3 emissions from slurry.
  
  #Gets N load for slurry.
  Slurry_Spreading_N_load <- N_load_table[N_load_table[,1] == "Slurry spreading",] 
  
  #Gets EF for slurry. 
  NO3_EF_grazing_Slurry_Spreading <- NO3_EF_grazing[NO3_EF_grazing[,1] == "Slurry spreading", ] 
  NO3_EF_1cut_Slurry_Spreading <- NO3_EF_1cut[NO3_EF_1cut[,1] == "Slurry spreading", ]
  NO3_EF_2cut_Slurry_Spreading <- NO3_EF_2cut[NO3_EF_2cut[,1] == "Slurry spreading", ]
  
  Slurry_Spreading_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Slurry_Spreading_NO3_emission_1cut <- 0
  Slurry_Spreading_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Slurry_Spreading_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NO3_emission_grazing <- Slurry_Spreading_NO3_emission_grazing + Slurry_Spreading_NO3_emission_grazing_calc
    
    Slurry_Spreading_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NO3_emission_1cut <- Slurry_Spreading_NO3_emission_1cut + Slurry_Spreading_NO3_emission_1cut_calc
    
    Slurry_Spreading_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NO3_emission_2cut <- Slurry_Spreading_NO3_emission_2cut + Slurry_Spreading_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Slurry_Spreading_NO3_emission <<- (Slurry_Spreading_NO3_emission_grazing + Slurry_Spreading_NO3_emission_1cut + Slurry_Spreading_NO3_emission_2cut) / area_total
  
}

Soiled_Water_Spreading_loss_NO3 <- function() { #Gets NO3 emissions from soiled water.
  
  #Gets N load for soiled water.
  Soiled_Water_Spreading_N_load <- N_load_table[N_load_table[,1] == "Soiled water spreading",]
  
  #Gets EF for soiled water.
  NO3_EF_grazing_Soiled_Water_Spreading <- NO3_EF_grazing[NO3_EF_grazing[,1] == "Soiled water spreading", ] 
  NO3_EF_1cut_Soiled_Water_Spreading <- NO3_EF_1cut[NO3_EF_1cut[,1] == "Soiled water spreading", ]
  NO3_EF_2cut_Soiled_Water_Spreading <- NO3_EF_2cut[NO3_EF_2cut[,1] == "Soiled water spreading", ]
  
  Soiled_Water_Spreading_NO3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Soiled_Water_Spreading_NO3_emission_1cut <- 0
  Soiled_Water_Spreading_NO3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Soiled_Water_Spreading_NO3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NO3_EF_grazing_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NO3_emission_grazing <- Soiled_Water_Spreading_NO3_emission_grazing + Soiled_Water_Spreading_NO3_emission_grazing_calc
    
    Soiled_Water_Spreading_NO3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NO3_EF_1cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NO3_emission_1cut <- Soiled_Water_Spreading_NO3_emission_1cut + Soiled_Water_Spreading_NO3_emission_1cut_calc
    
    Soiled_Water_Spreading_NO3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NO3_EF_2cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NO3_emission_2cut <- Soiled_Water_Spreading_NO3_emission_2cut + Soiled_Water_Spreading_NO3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Soiled_Water_Spreading_NO3_emission <<- (Soiled_Water_Spreading_NO3_emission_grazing + Soiled_Water_Spreading_NO3_emission_1cut + Soiled_Water_Spreading_NO3_emission_2cut) / area_total
  
}

Urine_loss_NO3() #Calls function calculating N loss from urine in NO3.
Urea_loss_NO3() #Calls function calculating N loss from urea in NO3.
CAN_loss_NO3() #Calls function calculating N loss from CAN in NO3.
Dung_loss_NO3() #Calls function calculating N loss from dung in NO3.
Slurry_Spreading_loss_NO3() #Calls function calculating N loss from slurry spreading in NO3.
Soiled_Water_Spreading_loss_NO3() #Calls function calculating N loss from soiled water spreading in NO3.
```

NH3 Emissions:

``` r
Urine_loss_NH3 <- function() { #Gets NH3 emissions from urine.
  
  #Gets N load for urine.
  Urine_N_load <- N_load_table[N_load_table[,1] == "Urine",]
  
  #Gets EF for urine.
  NH3_EF_grazing_Urine <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Urine", ] 
  NH3_EF_1cut_Urine <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Urine", ]
  NH3_EF_2cut_Urine <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Urine", ]
  
  Urine_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Urine_NH3_emission_1cut <- 0
  Urine_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Urine_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NH3_emission_grazing <- Urine_NH3_emission_grazing + Urine_NH3_emission_grazing_calc
    
    Urine_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NH3_emission_1cut <- Urine_NH3_emission_1cut + Urine_NH3_emission_1cut_calc
    
    Urine_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_NH3_emission_2cut <- Urine_NH3_emission_2cut + Urine_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Urine_NH3_emission <<- (Urine_NH3_emission_grazing + Urine_NH3_emission_1cut + Urine_NH3_emission_2cut) / area_total
  
}

Urea_loss_NH3 <- function() { #Gets NH3 emissions from urea.
  
  #Gets N load for urea.
  Urea_N_load <- N_load_table[N_load_table[,1] == "Urea",]
  
  #Gets EF for urea.
  NH3_EF_grazing_Urea <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Urea", ] 
  NH3_EF_1cut_Urea <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Urea", ]
  NH3_EF_2cut_Urea <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Urea", ]
  
  Urea_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Urea_NH3_emission_1cut <- 0
  Urea_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Urea_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NH3_emission_grazing <- Urea_NH3_emission_grazing + Urea_NH3_emission_grazing_calc
    
    Urea_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NH3_emission_1cut <- Urea_NH3_emission_1cut + Urea_NH3_emission_1cut_calc
    
    Urea_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_NH3_emission_2cut <- Urea_NH3_emission_2cut + Urea_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Urea_NH3_emission <<- (Urea_NH3_emission_grazing + Urea_NH3_emission_1cut + Urea_NH3_emission_2cut) / area_total
  
}

CAN_loss_NH3 <- function() { #Gets NH3 emissions from CAN.
  
  #Gets N load for CAN.
  CAN_N_load <- N_load_table[N_load_table[,1] == "CAN",]
  
  #Gets EF for CAN.
  NH3_EF_grazing_CAN <- NH3_EF_grazing[NH3_EF_grazing[,1] == "CAN", ] 
  NH3_EF_1cut_CAN <- NH3_EF_1cut[NH3_EF_1cut[,1] == "CAN", ]
  NH3_EF_2cut_CAN <- NH3_EF_2cut[NH3_EF_2cut[,1] == "CAN", ]
  
  CAN_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  CAN_NH3_emission_1cut <- 0
  CAN_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    CAN_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NH3_emission_grazing <- CAN_NH3_emission_grazing + CAN_NH3_emission_grazing_calc
    
    CAN_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NH3_emission_1cut <- CAN_NH3_emission_1cut + CAN_NH3_emission_1cut_calc
    
    CAN_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_NH3_emission_2cut <- CAN_NH3_emission_2cut + CAN_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  CAN_NH3_emission <<- (CAN_NH3_emission_grazing + CAN_NH3_emission_1cut + CAN_NH3_emission_2cut) / area_total
  
}

Dung_loss_NH3 <- function() { #Gets NH3 emissions from dung.
  
  #Gets N load for dung.
  Dung_N_load <- N_load_table[N_load_table[,1] == "Dung",]
  
  #Gets EF for dung. 
  NH3_EF_grazing_Dung <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Dung", ] 
  NH3_EF_1cut_Dung <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Dung", ]
  NH3_EF_2cut_Dung <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Dung", ]
  
  Dung_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Dung_NH3_emission_1cut <- 0
  Dung_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Dung_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NH3_emission_grazing <- Dung_NH3_emission_grazing + Dung_NH3_emission_grazing_calc
    
    Dung_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NH3_emission_1cut <- Dung_NH3_emission_1cut + Dung_NH3_emission_1cut_calc
    
    Dung_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_NH3_emission_2cut <- Dung_NH3_emission_2cut + Dung_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Dung_NH3_emission <<- (Dung_NH3_emission_grazing + Dung_NH3_emission_1cut + Dung_NH3_emission_2cut) / area_total
  
}

Slurry_Spreading_loss_NH3 <- function() { #Gets NH3 emissions from slurry spreading.
  
  #Gets N load for slurry spreading.
  Slurry_Spreading_N_load <- N_load_table[N_load_table[,1] == "Slurry spreading",]
  
  #Gets EF for slurry spreading.
  NH3_EF_grazing_Slurry_Spreading <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Slurry spreading", ] 
  NH3_EF_1cut_Slurry_Spreading <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Slurry spreading", ]
  NH3_EF_2cut_Slurry_Spreading <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Slurry spreading", ]
  
  Slurry_Spreading_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Slurry_Spreading_NH3_emission_1cut <- 0
  Slurry_Spreading_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Slurry_Spreading_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NH3_emission_grazing <- Slurry_Spreading_NH3_emission_grazing + Slurry_Spreading_NH3_emission_grazing_calc
    
    Slurry_Spreading_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NH3_emission_1cut <- Slurry_Spreading_NH3_emission_1cut + Slurry_Spreading_NH3_emission_1cut_calc
    
    Slurry_Spreading_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_NH3_emission_2cut <- Slurry_Spreading_NH3_emission_2cut + Slurry_Spreading_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Slurry_Spreading_NH3_emission <<- TAN_Slurry * (Slurry_Spreading_NH3_emission_grazing + Slurry_Spreading_NH3_emission_1cut + Slurry_Spreading_NH3_emission_2cut) / area_total
  
}

Soiled_Water_Spreading_loss_NH3 <- function() { #Gets NH3 emissions from soiled water spreading.
  
  #Gets N load for soiled water spreading.
  Soiled_Water_Spreading_N_load <- N_load_table[N_load_table[,1] == "Soiled water spreading",]
  
  #Gets EF for soiled water spreading. 
  NH3_EF_grazing_Soiled_Water_Spreading <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Soiled water spreading", ] 
  NH3_EF_1cut_Soiled_Water_Spreading <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Soiled water spreading", ]
  NH3_EF_2cut_Soiled_Water_Spreading <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Soiled water spreading", ]
  
  Soiled_Water_Spreading_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Soiled_Water_Spreading_NH3_emission_1cut <- 0
  Soiled_Water_Spreading_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Soiled_Water_Spreading_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NH3_emission_grazing <- Soiled_Water_Spreading_NH3_emission_grazing + Soiled_Water_Spreading_NH3_emission_grazing_calc
    
    Soiled_Water_Spreading_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NH3_emission_1cut <- Soiled_Water_Spreading_NH3_emission_1cut + Soiled_Water_Spreading_NH3_emission_1cut_calc
    
    Soiled_Water_Spreading_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_NH3_emission_2cut <- Soiled_Water_Spreading_NH3_emission_2cut + Soiled_Water_Spreading_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Soiled_Water_Spreading_NH3_emission <<- TAN_Slurry * (Soiled_Water_Spreading_NH3_emission_grazing + Soiled_Water_Spreading_NH3_emission_1cut + Soiled_Water_Spreading_NH3_emission_2cut) / area_total
  
}

Housing_dairy_cows_loss_NH3 <- function(){#Gets NH3 emissions for dairy cows housing. 
  
  #Gets cows number in Livestock Units.
  Cows <- Livestock_data[Livestock_data[,1] == "Cows (mean)", ]
 
  #Gets days housed for cows. 
  Days_housed <- Livestock_data[Livestock_data[,1] == "days housed (including milking time)", ]
  
  #Gets EF for dairy cows. Picked any of grazing, 1 cut, 2 cut, since EF not determined by drainage   values in this case.
  NH3_EF_dairy_cows <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Dairy Cows", ]
  
  Dairy_cows_NH3_emission <- 0 #Variables defined locally for 'for loop' below
  
  for(i in 2:13) {#N emission calculations
    
    Dairy_cows_NH3_emission_calc <- as.numeric(Cows[,i]) * as.numeric(Days_housed[,i]) *
    as.numeric(NH3_EF_dairy_cows[,i])
    Dairy_cows_NH3_emission <- Dairy_cows_NH3_emission + Dairy_cows_NH3_emission_calc
    
  }
  
  Dairy_cows_NH3_emission <<- Dairy_cows_NH3_emission/1000/area_total
  
}

Housing_yearlings_loss_NH3 <- function(){#Gets NH3 emissions for dairy cows housing. 
  
  #Gets cows number in Livestock Units.
  Yearlings <- Livestock_data[Livestock_data[,1] == "Yearlings", ]
 
  #Gets days housed for cows. 
  Days_housed <- Livestock_data[Livestock_data[,1] == "days housed (including milking time)", ]
  
  #Gets EF for dairy cows. Picked any of grazing, 1 cut, 2 cut, since EF not determined by drainage   values in this case.
  NH3_EF_yearlings <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Yearlings", ]
  
  Yearlings_NH3_emission <- 0 #Variables defined locally for 'for loop' below
  
  for(i in 2:13) {#N emission calculations
    
    Yearlings_NH3_emission_calc <- as.numeric(Yearlings[,i]) * as.numeric(Days_housed[,i]) *
    as.numeric(NH3_EF_yearlings[,i])
    Yearlings_NH3_emission <- Yearlings_NH3_emission + Yearlings_NH3_emission_calc
    
  }
  
  Yearlings_NH3_emission <<- Yearlings_NH3_emission/1000/area_total
  
}

Housing_calves_loss_NH3 <- function(){#Gets NH3 emissions for dairy cows housing. 
  
  #Gets cows number in Livestock Units.
  Calves <- Livestock_data[Livestock_data[,1] == "Calves", ]
  
  #Gets days housed for cows. 
  Days_housed <- Livestock_data[Livestock_data[,1] == "days housed (including milking time)", ]
  
  #Gets EF for dairy cows. Picked any of grazing, 1 cut, 2 cut, since EF not determined by drainage   values in this case.
  NH3_EF_calves <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Calves", ]
  
  Calves_NH3_emission <- 0 #Variables defined locally for 'for loop' below
  
  for(i in 2:13) {#N emission calculations
    
    Calves_NH3_emission_calc <- as.numeric(Calves[,i]) * as.numeric(Days_housed[,i]) *
    as.numeric(NH3_EF_calves[,i])
    Calves_NH3_emission <- Calves_NH3_emission + Calves_NH3_emission_calc
    
  }
  
  Calves_NH3_emission <<- Calves_NH3_emission/1000/area_total
  
}

Yard_emissions_loss_NH3 <- function(){#Gets NH3 emissions for dairy cows housing. 
  
  #Gets cows number in Livestock Units.
  Cows_milked <- Livestock_data[Livestock_data[,1] == "# Cows milked", ]
  
  #Gets EF for dairy cows. Picked any of grazing, 1 cut, 2 cut, since EF not determined by drainage   values in this case.
  NH3_EF_yard <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Emissions yard", ]
  
  Yard_NH3_emission <- 0 #Variables defined locally for 'for loop' below
  
  for(i in 2:13) {#N emission calculations
    
    Yard_NH3_emission_calc <- as.numeric(Cows_milked[,i]) * as.numeric(Days_in_month[2,i]) *
    as.numeric(NH3_EF_yard[,i])
    
    Yard_NH3_emission <- Yard_NH3_emission + Yard_NH3_emission_calc
    
  }
  
  Yard_NH3_emission <<- Yard_NH3_emission/1000/area_total
  
}

Pathways_loss_NH3 <- function() { #Gets NH3 emissions from dung.
  
  #Gets N load for Pathways.
  Pathways_N_load <- N_load_table[N_load_table[,1] == "Pathways (excreted)",]
  
  #Gets EF for Pathways. 
  NH3_EF_grazing_Pathways <- NH3_EF_grazing[NH3_EF_grazing[,1] == "Pathways", ] 
  NH3_EF_1cut_Pathways <- NH3_EF_1cut[NH3_EF_1cut[,1] == "Pathways", ]
  NH3_EF_2cut_Pathways <- NH3_EF_2cut[NH3_EF_2cut[,1] == "Pathways", ]
  
  Pathways_NH3_emission_grazing <- 0 #Variables defined locally for 'for loop' below.
  Pathways_NH3_emission_1cut <- 0
  Pathways_NH3_emission_2cut <- 0
  
  for (i in 2:13) { #N emission calculations for each grazing area, summed over each month.
    Pathways_NH3_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(NH3_EF_grazing_Pathways[,i]) * as.numeric(Pathways_N_load[,i]))
    Pathways_NH3_emission_grazing <- Pathways_NH3_emission_grazing + Pathways_NH3_emission_grazing_calc
    
    Pathways_NH3_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(NH3_EF_1cut_Pathways[,i]) * as.numeric(Pathways_N_load[,i]))
    Pathways_NH3_emission_1cut <- Pathways_NH3_emission_1cut + Pathways_NH3_emission_1cut_calc
    
    Pathways_NH3_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(NH3_EF_2cut_Pathways[,i]) * as.numeric(Pathways_N_load[,i]))
    Pathways_NH3_emission_2cut <- Pathways_NH3_emission_2cut + Pathways_NH3_emission_2cut_calc
  }
  
  #Sum of total emission over each grazing area. 
  Pathways_NH3_emission <<- (Pathways_NH3_emission_grazing + Pathways_NH3_emission_1cut + Pathways_NH3_emission_2cut) / area_total
  
}

Slurry_soiled_water_storage_loss_NH3 <- function() {
  
  Slurry_storage_NH3_emission <<- as.numeric(Slurry_soiled_water_storage_data[2, 2]) * 365 * as.numeric(NH3_EF_grazing[NH3_EF_grazing[,1] == "Slurry storage", 2])/1000/area_total
  
  Soiled_water_storage_NH3_emission <<- as.numeric(Slurry_soiled_water_storage_data[3, 2]) * 365 * as.numeric(NH3_EF_grazing[NH3_EF_grazing[,1] == "Soiled water storage", 2])/1000/area_total
}


Urine_loss_NH3() #Calls function calculating N loss from urine in NH3.
Urea_loss_NH3() #Calls function calculating N loss from urea in NH3.
CAN_loss_NH3() #Calls function calculating N loss from CAN in NH3.
Dung_loss_NH3() #Calls function calculating N loss from dung in NH3.
Slurry_Spreading_loss_NH3() #Calls function calculating N loss from slurry spreading in NH3.
Soiled_Water_Spreading_loss_NH3() #Calls function calculating N loss from soiled water spreading in NH3.
Housing_dairy_cows_loss_NH3() #Calls function calculating N loss from dairy cows in NH3
Housing_yearlings_loss_NH3() #Calls function calculating N loss from yearlings in NH3
Housing_calves_loss_NH3() #Calls function calculating N loss from calves in NH3
Yard_emissions_loss_NH3() #Calls function calculating N loss from yard in NH3
Pathways_loss_NH3() #Calls function calculating N loss from pathways in NH3.
Slurry_soiled_water_storage_loss_NH3() #Calls function calculating N loss from slurry and soiled water storage in NH3.
```

N2O Emissions:

``` r
Urine_loss_N2O <- function() {
  
  Urine_N_load <- N_load_table[N_load_table[,1] == "Urine",]
  N2O_EF_grazing_Urine <- N2O_EF_grazing[N2O_EF_grazing[,1] == "Urine", ] 
  N2O_EF_1cut_Urine <- N2O_EF_1cut[N2O_EF_1cut[,1] == "Urine", ]
  N2O_EF_2cut_Urine <- N2O_EF_2cut[N2O_EF_2cut[,1] == "Urine", ]
  Urine_N2O_emission_grazing <- 0
  Urine_N2O_emission_1cut <- 0
  Urine_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    Urine_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2O_emission_grazing <- Urine_N2O_emission_grazing + Urine_N2O_emission_grazing_calc
    
    Urine_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2O_emission_1cut <- Urine_N2O_emission_1cut + Urine_N2O_emission_1cut_calc
    
    Urine_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2O_emission_2cut <- Urine_N2O_emission_2cut + Urine_N2O_emission_2cut_calc
  }
  
  Urine_N2O_emission <<- (Urine_N2O_emission_grazing + Urine_N2O_emission_1cut + Urine_N2O_emission_2cut)/ area_total
  
}

Urea_loss_N2O <- function() {
  
  Urea_N_load <- N_load_table[N_load_table[,1] == "Urea",]
  N2O_EF_grazing_Urea <- N2O_EF_grazing[N2O_EF_grazing[,1] == "Urea", ] 
  N2O_EF_1cut_Urea <- N2O_EF_1cut[N2O_EF_1cut[,1] == "Urea", ]
  N2O_EF_2cut_Urea <- N2O_EF_2cut[N2O_EF_2cut[,1] == "Urea", ]
  Urea_N2O_emission_grazing <- 0
  Urea_N2O_emission_1cut <- 0
  Urea_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    Urea_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2O_emission_grazing <- Urea_N2O_emission_grazing + Urea_N2O_emission_grazing_calc
    
    Urea_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2O_emission_1cut <- Urea_N2O_emission_1cut + Urea_N2O_emission_1cut_calc
    
    Urea_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2O_emission_2cut <- Urea_N2O_emission_2cut + Urea_N2O_emission_2cut_calc
  }
  
  Urea_N2O_emission <<- (Urea_N2O_emission_grazing + Urea_N2O_emission_1cut + Urea_N2O_emission_2cut)/ area_total
  
}

CAN_loss_N2O <- function() {
  
  CAN_N_load <- N_load_table[N_load_table[,1] == "CAN",]
  N2O_EF_grazing_CAN <- N2O_EF_grazing[N2O_EF_grazing[,1] == "CAN", ] 
  N2O_EF_1cut_CAN <- N2O_EF_1cut[N2O_EF_1cut[,1] == "CAN", ]
  N2O_EF_2cut_CAN <- N2O_EF_2cut[N2O_EF_2cut[,1] == "CAN", ]
  CAN_N2O_emission_grazing <- 0
  CAN_N2O_emission_1cut <- 0
  CAN_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    CAN_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2O_emission_grazing <- CAN_N2O_emission_grazing + CAN_N2O_emission_grazing_calc
    
    CAN_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2O_emission_1cut <- CAN_N2O_emission_1cut + CAN_N2O_emission_1cut_calc
    
    CAN_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2O_emission_2cut <- CAN_N2O_emission_2cut + CAN_N2O_emission_2cut_calc
  }
  
  CAN_N2O_emission <<- (CAN_N2O_emission_grazing + CAN_N2O_emission_1cut + CAN_N2O_emission_2cut)/ area_total
  
}

Dung_loss_N2O <- function() {
  
  Dung_N_load <- N_load_table[N_load_table[,1] == "Dung",]
  N2O_EF_grazing_Dung <- N2O_EF_grazing[N2O_EF_grazing[,1] == "Dung", ] 
  N2O_EF_1cut_Dung <- N2O_EF_1cut[N2O_EF_1cut[,1] == "Dung", ]
  N2O_EF_2cut_Dung <- N2O_EF_2cut[N2O_EF_2cut[,1] == "Dung", ]
  Dung_N2O_emission_grazing <- 0
  Dung_N2O_emission_1cut <- 0
  Dung_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    Dung_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2O_emission_grazing <- Dung_N2O_emission_grazing + Dung_N2O_emission_grazing_calc
    
    Dung_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2O_emission_1cut <- Dung_N2O_emission_1cut + Dung_N2O_emission_1cut_calc
    
    Dung_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2O_emission_2cut <- Dung_N2O_emission_2cut + Dung_N2O_emission_2cut_calc
  }
  
  Dung_N2O_emission <<- (Dung_N2O_emission_grazing + Dung_N2O_emission_1cut + Dung_N2O_emission_2cut)/ area_total
  
}

Slurry_Spreading_loss_N2O <- function() {
  
  Slurry_Spreading_N_load <- N_load_table[N_load_table[,1] == "Slurry spreading",]
  N2O_EF_grazing_Slurry_Spreading <- N2O_EF_grazing[N2O_EF_grazing[,1] == "Slurry spreading", ] 
  N2O_EF_1cut_Slurry_Spreading <- N2O_EF_1cut[N2O_EF_1cut[,1] == "Slurry spreading", ]
  N2O_EF_2cut_Slurry_Spreading <- N2O_EF_2cut[N2O_EF_2cut[,1] == "Slurry spreading", ]
  Slurry_Spreading_N2O_emission_grazing <- 0
  Slurry_Spreading_N2O_emission_1cut <- 0
  Slurry_Spreading_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    Slurry_Spreading_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2O_emission_grazing <- Slurry_Spreading_N2O_emission_grazing + Slurry_Spreading_N2O_emission_grazing_calc
    
    Slurry_Spreading_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2O_emission_1cut <- Slurry_Spreading_N2O_emission_1cut + Slurry_Spreading_N2O_emission_1cut_calc
    
    Slurry_Spreading_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2O_emission_2cut <- Slurry_Spreading_N2O_emission_2cut + Slurry_Spreading_N2O_emission_2cut_calc
  }
  
  Slurry_Spreading_N2O_emission <<- (Slurry_Spreading_N2O_emission_grazing + Slurry_Spreading_N2O_emission_1cut + Slurry_Spreading_N2O_emission_2cut) / area_total
  
}

Soiled_Water_Spreading_loss_N2O <- function() {
  
  Soiled_Water_Spreading_N_load <- N_load_table[N_load_table[,1] == "Soiled water spreading",]
  N2O_EF_grazing_Soiled_Water_Spreading <- N2O_EF_grazing[N2O_EF_grazing[,1] == "Soiled water spreading", ] 
  N2O_EF_1cut_Soiled_Water_Spreading <- N2O_EF_1cut[N2O_EF_1cut[,1] == "Soiled water spreading", ]
  N2O_EF_2cut_Soiled_Water_Spreading <- N2O_EF_2cut[N2O_EF_2cut[,1] == "Soiled water spreading", ]
  Soiled_Water_Spreading_N2O_emission_grazing <- 0
  Soiled_Water_Spreading_N2O_emission_1cut <- 0
  Soiled_Water_Spreading_N2O_emission_2cut <- 0
  
  for (i in 2:13) {
    Soiled_Water_Spreading_N2O_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2O_EF_grazing_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2O_emission_grazing <- Soiled_Water_Spreading_N2O_emission_grazing + Soiled_Water_Spreading_N2O_emission_grazing_calc
    
    Soiled_Water_Spreading_N2O_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2O_EF_1cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2O_emission_1cut <- Soiled_Water_Spreading_N2O_emission_1cut + Soiled_Water_Spreading_N2O_emission_1cut_calc
    
    Soiled_Water_Spreading_N2O_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2O_EF_2cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2O_emission_2cut <- Soiled_Water_Spreading_N2O_emission_2cut + Soiled_Water_Spreading_N2O_emission_2cut_calc
  }
  
  Soiled_Water_Spreading_N2O_emission <<- (Soiled_Water_Spreading_N2O_emission_grazing + Soiled_Water_Spreading_N2O_emission_1cut + Soiled_Water_Spreading_N2O_emission_2cut)/ area_total
  
}

Housing_storage_pathways_loss_N2O <- function() {
  
  Housing_storage_pathways_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Housing and storage + pathways-NH3", 2])
  
  Housing_storage_pathways_N2O_emission <<- Housing_storage_pathways_EF * (Dairy_cows_NH3_emission + Calves_NH3_emission + Yearlings_NH3_emission + Soiled_water_storage_NH3_emission + Slurry_storage_NH3_emission + Yard_NH3_emission + Pathways_NH3_emission) 
  
}

Fertiliser_loss_N2O <- function() {
  
  Fertiliser_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Fertiliser-NH3", 2])
  
  Fertiliser_N2O_emission <<- Fertiliser_EF * (Urea_NH3_emission + CAN_NH3_emission)
  
}

Grazing_loss_N2O <- function() {
  
  Grazing_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Grazing-NH3", 2])
  
  Grazing_N2O_emission <<- Grazing_EF * (Urine_NH3_emission + Dung_NH3_emission)
  
}

Slurry_Spreading_loss_N2O <- function() {
  
  Slurry_Spreading_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Slurry-NH3", 2])
  
  Slurry_Spreading_N2O_emission <<- Slurry_Spreading_EF * (Slurry_Spreading_NH3_emission)
  
}

Soiled_Water_Spreading_loss_N2O <- function() {
  
  Soiled_Water_Spreading_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Soiled water-NH3", 2])
  
  Soiled_Water_Spreading_N2O_emission <<- Soiled_Water_Spreading_EF * (Soiled_Water_Spreading_NH3_emission)
  
}

Leaching_loss_N2O <- function() {
  Leaching_EF <- as.numeric(N2O_EF_grazing[N2O_EF_grazing[,1] == "Leaching - NO3", 2])
  
  Leaching_N2O_emission <<- Leaching_EF * (Urine_NO3_emission + Dung_NO3_emission + CAN_NO3_emission + Urea_NO3_emission + Slurry_Spreading_NO3_emission + Soiled_Water_Spreading_NO3_emission)
}

Urine_loss_N2O()
Urea_loss_N2O()
CAN_loss_N2O()
Dung_loss_N2O()
Slurry_Spreading_loss_N2O()
Soiled_Water_Spreading_loss_N2O()
Housing_storage_pathways_loss_N2O()
Fertiliser_loss_N2O()
Grazing_loss_N2O()
Leaching_loss_N2O()
```

N2 Emissions:

``` r
Urine_loss_N2 <- function() {
  
  Urine_N_load <- N_load_table[N_load_table[,1] == "Urine",]
  N2_EF_grazing_Urine <- N2_EF_grazing[N2_EF_grazing[,1] == "Urine", ] 
  N2_EF_1cut_Urine <- N2_EF_1cut[N2_EF_1cut[,1] == "Urine", ]
  N2_EF_2cut_Urine <- N2_EF_2cut[N2_EF_2cut[,1] == "Urine", ]
  Urine_N2_emission_grazing <- 0
  Urine_N2_emission_1cut <- 0
  Urine_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    Urine_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2_emission_grazing <- Urine_N2_emission_grazing + Urine_N2_emission_grazing_calc
    
    Urine_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2_emission_1cut <- Urine_N2_emission_1cut + Urine_N2_emission_1cut_calc
    
    Urine_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_Urine[,i]) * as.numeric(Urine_N_load[,i]))
    Urine_N2_emission_2cut <- Urine_N2_emission_2cut + Urine_N2_emission_2cut_calc
  }
  
  Urine_N2_emission <<- (Urine_N2_emission_grazing + Urine_N2_emission_1cut + Urine_N2_emission_2cut)/ area_total
  
}

Urea_loss_N2 <- function() {
  
  Urea_N_load <- N_load_table[N_load_table[,1] == "Urea",]
  N2_EF_grazing_Urea <- N2_EF_grazing[N2_EF_grazing[,1] == "Urea", ] 
  N2_EF_1cut_Urea <- N2_EF_1cut[N2_EF_1cut[,1] == "Urea", ]
  N2_EF_2cut_Urea <- N2_EF_2cut[N2_EF_2cut[,1] == "Urea", ]
  Urea_N2_emission_grazing <- 0
  Urea_N2_emission_1cut <- 0
  Urea_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    Urea_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2_emission_grazing <- Urea_N2_emission_grazing + Urea_N2_emission_grazing_calc
    
    Urea_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2_emission_1cut <- Urea_N2_emission_1cut + Urea_N2_emission_1cut_calc
    
    Urea_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_Urea[,i]) * as.numeric(Urea_N_load[,i]))
    Urea_N2_emission_2cut <- Urea_N2_emission_2cut + Urea_N2_emission_2cut_calc
  }
  
  Urea_N2_emission <<- (Urea_N2_emission_grazing + Urea_N2_emission_1cut + Urea_N2_emission_2cut)/ area_total
  
}

CAN_loss_N2 <- function() {
  
  CAN_N_load <- N_load_table[N_load_table[,1] == "CAN",]
  N2_EF_grazing_CAN <- N2_EF_grazing[N2_EF_grazing[,1] == "CAN", ] 
  N2_EF_1cut_CAN <- N2_EF_1cut[N2_EF_1cut[,1] == "CAN", ]
  N2_EF_2cut_CAN <- N2_EF_2cut[N2_EF_2cut[,1] == "CAN", ]
  CAN_N2_emission_grazing <- 0
  CAN_N2_emission_1cut <- 0
  CAN_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    CAN_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2_emission_grazing <- CAN_N2_emission_grazing + CAN_N2_emission_grazing_calc
    
    CAN_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2_emission_1cut <- CAN_N2_emission_1cut + CAN_N2_emission_1cut_calc
    
    CAN_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_CAN[,i]) * as.numeric(CAN_N_load[,i]))
    CAN_N2_emission_2cut <- CAN_N2_emission_2cut + CAN_N2_emission_2cut_calc
  }
  
  CAN_N2_emission <<- (CAN_N2_emission_grazing + CAN_N2_emission_1cut + CAN_N2_emission_2cut)/ area_total
  
}

Dung_loss_N2 <- function() {
  
  Dung_N_load <- N_load_table[N_load_table[,1] == "Dung",]
  N2_EF_grazing_Dung <- N2_EF_grazing[N2_EF_grazing[,1] == "Dung", ] 
  N2_EF_1cut_Dung <- N2_EF_1cut[N2_EF_1cut[,1] == "Dung", ]
  N2_EF_2cut_Dung <- N2_EF_2cut[N2_EF_2cut[,1] == "Dung", ]
  Dung_N2_emission_grazing <- 0
  Dung_N2_emission_1cut <- 0
  Dung_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    Dung_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2_emission_grazing <- Dung_N2_emission_grazing + Dung_N2_emission_grazing_calc
    
    Dung_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2_emission_1cut <- Dung_N2_emission_1cut + Dung_N2_emission_1cut_calc
    
    Dung_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_Dung[,i]) * as.numeric(Dung_N_load[,i]))
    Dung_N2_emission_2cut <- Dung_N2_emission_2cut + Dung_N2_emission_2cut_calc
  }
  
  Dung_N2_emission <<- (Dung_N2_emission_grazing + Dung_N2_emission_1cut + Dung_N2_emission_2cut)/ area_total
  
}

Slurry_Spreading_loss_N2 <- function() {
  
  Slurry_Spreading_N_load <- N_load_table[N_load_table[,1] == "Slurry spreading",]
  N2_EF_grazing_Slurry_Spreading <- N2_EF_grazing[N2_EF_grazing[,1] == "Slurry spreading", ] 
  N2_EF_1cut_Slurry_Spreading <- N2_EF_1cut[N2_EF_1cut[,1] == "Slurry spreading", ]
  N2_EF_2cut_Slurry_Spreading <- N2_EF_2cut[N2_EF_2cut[,1] == "Slurry spreading", ]
  Slurry_Spreading_N2_emission_grazing <- 0
  Slurry_Spreading_N2_emission_1cut <- 0
  Slurry_Spreading_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    Slurry_Spreading_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2_emission_grazing <- Slurry_Spreading_N2_emission_grazing + Slurry_Spreading_N2_emission_grazing_calc
    
    Slurry_Spreading_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2_emission_1cut <- Slurry_Spreading_N2_emission_1cut + Slurry_Spreading_N2_emission_1cut_calc
    
    Slurry_Spreading_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_Slurry_Spreading[,i]) * as.numeric(Slurry_Spreading_N_load[,i]))
    Slurry_Spreading_N2_emission_2cut <- Slurry_Spreading_N2_emission_2cut + Slurry_Spreading_N2_emission_2cut_calc
  }
  
  Slurry_Spreading_N2_emission <<- (Slurry_Spreading_N2_emission_grazing + Slurry_Spreading_N2_emission_1cut + Slurry_Spreading_N2_emission_2cut)/ area_total
  
}

Soiled_Water_Spreading_loss_N2 <- function() {
  
  Soiled_Water_Spreading_N_load <- N_load_table[N_load_table[,1] == "Soiled water spreading",]
  N2_EF_grazing_Soiled_Water_Spreading <- N2_EF_grazing[N2_EF_grazing[,1] == "Soiled water spreading", ] 
  N2_EF_1cut_Soiled_Water_Spreading <- N2_EF_1cut[N2_EF_1cut[,1] == "Soiled water spreading", ]
  N2_EF_2cut_Soiled_Water_Spreading <- N2_EF_2cut[N2_EF_2cut[,1] == "Soiled water spreading", ]
  Soiled_Water_Spreading_N2_emission_grazing <- 0
  Soiled_Water_Spreading_N2_emission_1cut <- 0
  Soiled_Water_Spreading_N2_emission_2cut <- 0
  
  for (i in 2:13) {
    Soiled_Water_Spreading_N2_emission_grazing_calc <- (area_grazing / area_total) * 
      (as.numeric(N2_EF_grazing_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2_emission_grazing <- Soiled_Water_Spreading_N2_emission_grazing + Soiled_Water_Spreading_N2_emission_grazing_calc
    
    Soiled_Water_Spreading_N2_emission_1cut_calc <- (area_1cut / area_total) * 
      (as.numeric(N2_EF_1cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2_emission_1cut <- Soiled_Water_Spreading_N2_emission_1cut + Soiled_Water_Spreading_N2_emission_1cut_calc
    
    Soiled_Water_Spreading_N2_emission_2cut_calc <- (area_2cut / area_total) * 
      (as.numeric(N2_EF_2cut_Soiled_Water_Spreading[,i]) * as.numeric(Soiled_Water_Spreading_N_load[,i]))
    Soiled_Water_Spreading_N2_emission_2cut <- Soiled_Water_Spreading_N2_emission_2cut + Soiled_Water_Spreading_N2_emission_2cut_calc
  }
  
  Soiled_Water_Spreading_N2_emission <<- (Soiled_Water_Spreading_N2_emission_grazing + Soiled_Water_Spreading_N2_emission_1cut + Soiled_Water_Spreading_N2_emission_2cut)/ area_total
  
}

Urine_loss_N2()
Urea_loss_N2()
CAN_loss_N2()
Dung_loss_N2()
Slurry_Spreading_loss_N2()
Soiled_Water_Spreading_loss_N2()
```
