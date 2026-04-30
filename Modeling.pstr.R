



######################## Only pstr #####################################
rm(list = ls())

setwd("~/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Recruits Sed Exposures")

library(tidyverse)

# port area (km2)
P.a = 1.8

# Average recruit density for southeast FL from Harper et al 2023 (Faviidae ind/km2)
R.d = 300000

# Average recruit density for southeast FL from Harper et al 2023 (all coral taxa ind/km2)
# R.d = 1000000


# starting recruit stock
Ri = P.a*R.d

# Proportions of species assemblages
P.pstr = 1.00
R.pstr = P.pstr*Ri

# Background mortality rate for each species at 1 month old (deaths/day)
# Proportions calculated from survfits for KM curves
Mb.pstr.1 = (0.18*R.pstr)/(10)

# Background mortality rate for each species at 2 months old (deaths/day)
# Calculated based on bakcground mortality of PSTR reported by Ritson-Williams et al. 2016
Mb.pstr.2 = (0.02*R.pstr)

# Background mortality rate for each species at 3 months old (deaths/day)
# Proportions calculated from survfits for KM curves
Mb.pstr.3 = (0.14*R.pstr)/(10)

# Hazard ratio of each species at 1 month old (4 mm of sediment)
HR.pstr.1 = 9

# hazard ratio of each species at 3 months old
HR.pstr.3 = 13



# Function for background mortality
Background_m1 <- function(p.c, p.u) {
  
  # Define Tx
  T1 <- seq(0, 30, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.c)*((R.pstr)-(Mb.pstr.1*x)))+((p.u)*((R.pstr)-(Mb.pstr.1*x)))
  }
  
  # Apply the function to Tx
  T1.test.c <- pstr.hr.1(T1)
  
  # Ensure non-negative values
  T1.test.c <- pmax(T1.test.c, 0)
  
  return(T1.test.c)
}

# Run different scenarios
pstr.0_30.100<-Background_m1(p.c = 1.00, p.u = 0.00)
print(pstr.0_30.100)
pstr.0_30.100<-as.data.frame(pstr.0_30.100)

pstr.0_30.75<-Background_m1(p.c = 0.75, p.u = 0.25)
print(pstr.0_30.75)
pstr.0_30.75<-as.data.frame(pstr.0_30.75)

pstr.0_30.50<-Background_m1(p.c = 0.50, p.u = 0.50)
print(pstr.0_30.50)
pstr.0_30.50<-as.data.frame(pstr.0_30.50)

pstr.0_30.25<-Background_m1(p.c = 0.25, p.u = 0.75)
print(pstr.0_30.25)
pstr.0_30.25<-as.data.frame(pstr.0_30.25)

pstr.0_30.0<-Background_m1(p.c = 0.00, p.u = 1.00)
print(pstr.0_30.0)
pstr.0_30.0<-as.data.frame(pstr.0_30.0)

# Generate vector of number of days
Days <- seq(0, 30, 1)
Days <- as.data.frame(Days)

# Bind population estimates with Days column
mortality0_30.0<-cbind(Days, pstr.0_30.0)
mortality0_30.25<-cbind(Days, pstr.0_30.25)
mortality0_30.50<-cbind(Days, pstr.0_30.50)
mortality0_30.75<-cbind(Days, pstr.0_30.75)
mortality0_30.100<-cbind(Days, pstr.0_30.100)

library(dplyr)
library(tidyverse)
library(tidyr)
# mortality0_30.0
# Add new column
mortality0_30.0 <- mortality0_30.0 %>%
  mutate(newcol = NA)

# Change column names
mortality0_30.0 <- mortality0_30.0 %>% 
  rename(
    Scenario = newcol
  )

mortality0_30.0 <- mortality0_30.0 %>% 
  rename(
    Population = pstr.0_30.0
  )

# Replace NAs with scenario name
mortality0_30.0$Scenario <- as.character(mortality0_30.0$Scenario)
mortality0_30.0 <-mortality0_30.0 %>% replace_na(list(Scenario = "pstr.0"))

####

# mortality0_30.25
# Add new column
mortality0_30.25 <- mortality0_30.25 %>%
  mutate(newcol = NA)

# Change column names
mortality0_30.25 <- mortality0_30.25 %>% 
  rename(
    Scenario = newcol
  )

mortality0_30.25 <- mortality0_30.25 %>% 
  rename(
    Population = pstr.0_30.25
  )

# Replace NAs with scenario name
mortality0_30.25$Scenario <- as.character(mortality0_30.25$Scenario)
mortality0_30.25 <-mortality0_30.25 %>% replace_na(list(Scenario = "pstr.25"))

####

# mortality0_30.50
# Add new column
mortality0_30.50 <- mortality0_30.50 %>%
  mutate(newcol = NA)

# Change column names
mortality0_30.50 <- mortality0_30.50 %>% 
  rename(
    Scenario = newcol
  )

mortality0_30.50 <- mortality0_30.50 %>% 
  rename(
    Population = pstr.0_30.50
  )

# Replace NAs with scenario name
mortality0_30.50$Scenario <- as.character(mortality0_30.50$Scenario)
mortality0_30.50 <-mortality0_30.50 %>% replace_na(list(Scenario = "pstr.50"))

####

# mortality0_30.75
# Add new column
mortality0_30.75 <- mortality0_30.75 %>%
  mutate(newcol = NA)

# Change column names
mortality0_30.75 <- mortality0_30.75 %>% 
  rename(
    Scenario = newcol
  )

mortality0_30.75 <- mortality0_30.75 %>% 
  rename(
    Population = pstr.0_30.75
  )

# Replace NAs with scenario name
mortality0_30.75$Scenario <- as.character(mortality0_30.75$Scenario)
mortality0_30.75 <-mortality0_30.75 %>% replace_na(list(Scenario = "pstr.75"))


####

# mortality0_30.100
# Add new column
mortality0_30.100 <- mortality0_30.100 %>%
  mutate(newcol = NA)

# Change column names
mortality0_30.100 <- mortality0_30.100 %>% 
  rename(
    Scenario = newcol
  )

mortality0_30.100 <- mortality0_30.100 %>% 
  rename(
    Population = pstr.0_30.100
  )

# Replace NAs with scenario name
mortality0_30.100$Scenario <- as.character(mortality0_30.100$Scenario)
mortality0_30.100 <-mortality0_30.100 %>% replace_na(list(Scenario = "pstr.100"))


mortality<-rbind(mortality0_30.0, mortality0_30.25, mortality0_30.50, mortality0_30.75, mortality0_30.100)

# Get population estimates by day 30 for each scenario
mortality.summary <- mortality %>%
  filter(Days==30)
mortality.summary


sedimentation.1.10 <- function(R.pstr.t, p.c, p.u, HR.pstr.1) {
  
  # Calculate background mortality rate
  Mb.pstr.1 <- (0.18*R.pstr.t)/(10)
  
  # Define Tx
  T10 <- seq(0, 10, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.u)*((R.pstr.t)-(Mb.pstr.1*(x))))+((p.c)*((R.pstr.t)-(HR.pstr.1*(Mb.pstr.1*(x)))))
  }
  
  # Apply the function to Tx
  T10.test.c <- pstr.hr.1(T10)
  
  # Ensure non-negative values
  T10.test.c <- pmax(T10.test.c, 0)
  
  return(T10.test.c)
  
}

# Run scenarios
pstr.31_40.0<-sedimentation.1.10(R.pstr.t = 248400, p.c = 0.00, p.u = 1.00, HR.pstr.1 = 9)
print(pstr.31_40.0)
pstr.31_40.0<-as.data.frame(pstr.31_40.0)

pstr.31_40.25<-sedimentation.1.10(R.pstr.t = 248400, p.c = 0.25, p.u = 0.75, HR.pstr.1 = 9)
print(pstr.31_40.25)
pstr.31_40.25<-as.data.frame(pstr.31_40.25)

pstr.31_40.50<-sedimentation.1.10(R.pstr.t = 248400, p.c = 0.50, p.u = 0.50, HR.pstr.1 = 9)
print(pstr.31_40.50)
pstr.31_40.50<-as.data.frame(pstr.31_40.50)

pstr.31_40.75<-sedimentation.1.10(R.pstr.t = 248400, p.c = 0.75, p.u = 0.25, HR.pstr.1 = 9)
print(pstr.31_40.75)
pstr.31_40.75<-as.data.frame(pstr.31_40.75)

pstr.31_40.100<-sedimentation.1.10(R.pstr.t = 248400, p.c = 1.00, p.u = 0.00, HR.pstr.1 = 9)
print(pstr.31_40.100)
pstr.31_40.100<-as.data.frame(pstr.31_40.100)

# Generate vector of number of days
Days.1 <- seq(30, 40, 1)
Days.1 <- as.data.frame(Days.1)

# Bind population estimates with Days column
pstr.31_40.0<-cbind(Days.1, pstr.31_40.0)
pstr.31_40.25<-cbind(Days.1, pstr.31_40.25)
pstr.31_40.50<-cbind(Days.1, pstr.31_40.50)
pstr.31_40.75<-cbind(Days.1, pstr.31_40.75)
pstr.31_40.100<-cbind(Days.1, pstr.31_40.100)


# sedimentation 1

####

# Add new column
pstr.31_40.0 <- pstr.31_40.0 %>%
  mutate(newcol = NA)

# Change column names
pstr.31_40.0 <- pstr.31_40.0 %>% 
  rename(
    Scenario = newcol
  )

pstr.31_40.0 <- pstr.31_40.0 %>% 
  rename(
    Population = pstr.31_40.0
  )

# Replace NAs with scenario name
pstr.31_40.0$Scenario <- as.character(pstr.31_40.0$Scenario)
pstr.31_40.0 <-pstr.31_40.0 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.31_40.0 <- pstr.31_40.0 %>% 
  rename(
    Days = Days.1
  )

####

# Add new column
pstr.31_40.25 <- pstr.31_40.25 %>%
  mutate(newcol = NA)

# Change column names
pstr.31_40.25 <- pstr.31_40.25 %>% 
  rename(
    Scenario = newcol
  )

pstr.31_40.25 <- pstr.31_40.25 %>% 
  rename(
    Population = pstr.31_40.25
  )

# Replace NAs with scenario name
pstr.31_40.25$Scenario <- as.character(pstr.31_40.25$Scenario)
pstr.31_40.25 <-pstr.31_40.25 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.31_40.25 <- pstr.31_40.25 %>% 
  rename(
    Days = Days.1
  )

####

# Add new column
pstr.31_40.50 <- pstr.31_40.50 %>%
  mutate(newcol = NA)

# Change column names
pstr.31_40.50 <- pstr.31_40.50 %>% 
  rename(
    Scenario = newcol
  )

pstr.31_40.50 <- pstr.31_40.50 %>% 
  rename(
    Population = pstr.31_40.50
  )

# Replace NAs with scenario name
pstr.31_40.50$Scenario <- as.character(pstr.31_40.50$Scenario)
pstr.31_40.50 <-pstr.31_40.50 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.31_40.50 <- pstr.31_40.50 %>% 
  rename(
    Days = Days.1
  )

####

# Add new column
pstr.31_40.75 <- pstr.31_40.75 %>%
  mutate(newcol = NA)

# Change column names
pstr.31_40.75 <- pstr.31_40.75 %>% 
  rename(
    Scenario = newcol
  )

pstr.31_40.75 <- pstr.31_40.75 %>% 
  rename(
    Population = pstr.31_40.75
  )

# Replace NAs with scenario name
pstr.31_40.75$Scenario <- as.character(pstr.31_40.75$Scenario)
pstr.31_40.75 <-pstr.31_40.75 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.31_40.75 <- pstr.31_40.75 %>% 
  rename(
    Days = Days.1
  )

####

# Add new column
pstr.31_40.100 <- pstr.31_40.100 %>%
  mutate(newcol = NA)

# Change column names
pstr.31_40.100 <- pstr.31_40.100 %>% 
  rename(
    Scenario = newcol
  )

pstr.31_40.100 <- pstr.31_40.100 %>% 
  rename(
    Population = pstr.31_40.100
  )

# Replace NAs with scenario name
pstr.31_40.100$Scenario <- as.character(pstr.31_40.100$Scenario)
pstr.31_40.100 <-pstr.31_40.100 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.31_40.100 <- pstr.31_40.100 %>% 
  rename(
    Days = Days.1
  )

mortality.1.10<-rbind(mortality, pstr.31_40.0, pstr.31_40.25, pstr.31_40.50, pstr.31_40.75, pstr.31_40.100)

# Get population estimates by day 40 for each scenario
mortality.summary.1 <- mortality.1.10 %>%
  filter(Days == 40)
mortality.summary.1

########################## Line plot pstr 1 month per coverage of recruits ###########################

se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}

mortality.1.10_summary <- mortality.1.10 %>%
  group_by(Days) %>%
  summarise(CT = mean(Population),
            spread = se(Population))

# Line plot
# Raw abundances
ggplot(mortality.1.10_summary, aes(x=Days,y=CT)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CT - spread, ymax = CT + spread), alpha = 0.3, color = NA)+
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50))+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

ggplot(mortality.1.10, aes(x=Days,y=Population, 
                           color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_hline(yintercept=0)+
  scale_x_continuous(breaks = seq(0,50, by = 5), limits = c(0,50))+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))+
  scale_color_manual(values = c("steelblue", "olivedrab", "goldenrod", "coral", "firebrick"))


legend_title<-"Percent of recruits buried by 4 mm of sediment"





# Function for background mortality after day 40
Background_m2 <- function(R.pstr.2, p.c, p.u) {
  
  # Define Tx
  T2 <- seq(41, 90, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.2 <- function(x) {
    ((p.c)*((R.pstr.2)-(Mb.pstr.1*(x-40))))+((p.u)*((R.pstr.2)-(Mb.pstr.1*(x-40))))
  }
  
  # Apply the function to Tx
  T2.test.c <- pstr.hr.2(T2)
  
  # Ensure non-negative values
  T2.test.c <- pmax(T2.test.c, 0)
  
  return(T2.test.c)
}

# Run different scenarios
pstr.41_90.100<-Background_m2(R.pstr.2 = 840000, p.c = 1.00, p.u = 0.00)
print(pstr.41_90.100)
pstr.41_90.100<-as.data.frame(pstr.41_90.100)


sedimentation.3 <- function(P.pstr, p.c, p.u, HR.pstr.3) {
  
  # Calculate R values
  R.pstr <- P.pstr * Ri
  
  # Calculate background mortality rate
  Mb.pstr.3 <- (0.14 * R.pstr) / 10
  
  # Define Tx
  T3 <- seq(91, 100, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.3 <- function(x) {
    (((p.u)*((R.pstr)-(Mb.pstr.3*x)))+((p.c)*((R.pstr)-(HR.pstr.3*(Mb.pstr.3*x)))))
  }
  
  # Apply the function to Tx
  T3.test.c <- pstr.hr.3(Tx)
  
  # Ensure non-negative values
  T3.test.c <- pmax(T3.test.c, 0)
  
  return(T3.test.c)
}




#################### Model with pstr where we do background mortality only for days 30 to 40 ################

# Beginning recruit stock
# Generate vector of number of days
Days.0 <- seq(0, 30, 1)
Days.0 <- as.data.frame(Days.0)

Days.0 <- Days.0 %>%
  mutate(newcol = 540000) %>% 
  rename(
    Population = newcol
  )%>% 
  rename(
    Days = Days.0
  ) %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  )

# Replace NAs with scenario name
Days.0$Scenario <- as.character(Days.0$Scenario)
Days.0 <-Days.0 %>% replace_na(list(Scenario = "pstr.0"))

# Filter background mortality for first 10 days, then convert to days 30 to 40
b.mortality.30_40 <- mortality %>%
  filter(Days<=10) %>%
  select(Days, Population, Scenario) %>%
  mutate(Days = Days +30)

b.mortality.30_40

# Run scenarios
pstr.41_50.0.10<-sedimentation.1.10(R.pstr.t = 442800, p.c = 0.00, p.u = 1.00, HR.pstr.1 = 9)
print(pstr.41_50.0.10)
pstr.41_50.0.10<-as.data.frame(pstr.41_50.0.10)

pstr.41_50.25.10<-sedimentation.1.10(R.pstr.t = 442800, p.c = 0.25, p.u = 0.75, HR.pstr.1 = 9)
print(pstr.41_50.25.10)
pstr.41_50.25.10<-as.data.frame(pstr.41_50.25.10)

pstr.41_50.50.10<-sedimentation.1.10(R.pstr.t = 442800, p.c = 0.50, p.u = 0.50, HR.pstr.1 = 9)
print(pstr.41_50.50.10)
pstr.41_50.50.10<-as.data.frame(pstr.41_50.50.10)

pstr.41_50.75.10<-sedimentation.1.10(R.pstr.t = 442800, p.c = 0.75, p.u = 0.25, HR.pstr.1 = 9)
print(pstr.41_50.75.10)
pstr.41_50.75.10<-as.data.frame(pstr.41_50.75.10)

pstr.41_50.100.10<-sedimentation.1.10(R.pstr.t = 442800, p.c = 1.00, p.u = 0.00, HR.pstr.1 = 9)
print(pstr.41_50.100.10)
pstr.41_50.100.10<-as.data.frame(pstr.41_50.100.10)

# Generate vector of number of days
Days.1 <- seq(40, 50, 1)
Days.1 <- as.data.frame(Days.1)

# Bind population estimates with Days column
pstr.41_50.0.10<-cbind(Days.1, pstr.41_50.0.10)
pstr.41_50.25.10<-cbind(Days.1, pstr.41_50.25.10)
pstr.41_50.50.10<-cbind(Days.1, pstr.41_50.50.10)
pstr.41_50.75.10<-cbind(Days.1, pstr.41_50.75.10)
pstr.41_50.100.10<-cbind(Days.1, pstr.41_50.100.10)

#### 0

# Add new column
pstr.41_50.0.10 <- pstr.41_50.0.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.0.10
  )

# Replace NAs with scenario name
pstr.41_50.0.10$Scenario <- as.character(pstr.41_50.0.10$Scenario)
pstr.41_50.0.10 <-pstr.41_50.0.10 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.41_50.0.10 <- pstr.41_50.0.10 %>% 
  rename(
    Days = Days.1
  )

#### 25

# Add new column
pstr.41_50.25.10 <- pstr.41_50.25.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.25.10
  )

# Replace NAs with scenario name
pstr.41_50.25.10$Scenario <- as.character(pstr.41_50.25.10$Scenario)
pstr.41_50.25.10 <-pstr.41_50.25.10 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.41_50.25.10 <- pstr.41_50.25.10 %>% 
  rename(
    Days = Days.1
  )

#### 50

# Add new column
pstr.41_50.50.10 <- pstr.41_50.50.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.50.10
  )

# Replace NAs with scenario name
pstr.41_50.50.10$Scenario <- as.character(pstr.41_50.50.10$Scenario)
pstr.41_50.50.10 <-pstr.41_50.50.10 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.41_50.50.10 <- pstr.41_50.50.10 %>% 
  rename(
    Days = Days.1
  )

#### 75

# Add new column
pstr.41_50.75.10 <- pstr.41_50.75.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.75.10
  )

# Replace NAs with scenario name
pstr.41_50.75.10$Scenario <- as.character(pstr.41_50.75.10$Scenario)
pstr.41_50.75.10<-pstr.41_50.75.10 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.41_50.75.10 <- pstr.41_50.75.10 %>% 
  rename(
    Days = Days.1
  )

#### 100

# Add new column
pstr.41_50.100.10 <- pstr.41_50.100.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.100.10
  )

# Replace NAs with scenario name
pstr.41_50.100.10$Scenario <- as.character(pstr.41_50.100.10$Scenario)
pstr.41_50.100.10 <-pstr.41_50.100.10 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.41_50.100.10 <- pstr.41_50.100.10 %>% 
  rename(
    Days = Days.1
  )


mortality.1.50<-rbind(Days.0, b.mortality.30_40, pstr.41_50.0.10, pstr.41_50.25.10, 
                      pstr.41_50.50.10, pstr.41_50.75.10, pstr.41_50.100.10)

mortality.1.50

# Add column specifying sedimentation event duration
mortality.1.50 <- mortality.1.50 %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )

mortality.1.50

# Get population estimates by day 50 for each scenario
mortality.summary.1.2 <- mortality.1.50 %>%
  filter(Days==50)
mortality.summary.1.2

##################### 7 day sedimentation event ######################
sedimentation.1.7 <- function(R.pstr.t, p.c, p.u, HR.pstr.1) {
  
  # Calculate background mortality rate
  Mb.pstr.1 <- (0.18*R.pstr.t)/(10)
  
  # Define Tx
  T7 <- seq(0, 7, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.u)*((R.pstr.t)-(Mb.pstr.1*(x))))+((p.c)*((R.pstr.t)-(HR.pstr.1*(Mb.pstr.1*(x)))))
  }
  
  # Apply the function to Tx
  T7.test.c <- pstr.hr.1(T7)
  
  # Ensure non-negative values
  T7.test.c <- pmax(T7.test.c, 0)
  
  return(T7.test.c)
  
}

442800.0-434829.6
299332.8-275421.6
# Run scenarios
pstr.41_50.0.7<-sedimentation.1.7(R.pstr.t = 442800, p.c = 0.00, p.u = 1.00, HR.pstr.1 = 9)
print(pstr.41_50.0.7)
pstr.41_50.0.7<-as.data.frame(pstr.41_50.0.7)

pstr.41_50.25.7<-sedimentation.1.7(R.pstr.t = 442800, p.c = 0.25, p.u = 0.75, HR.pstr.1 = 9)
print(pstr.41_50.25.7)
pstr.41_50.25.7<-as.data.frame(pstr.41_50.25.7)

pstr.41_50.50.7<-sedimentation.1.7(R.pstr.t = 442800, p.c = 0.50, p.u = 0.50, HR.pstr.1 = 9)
print(pstr.41_50.50.7)
pstr.41_50.50.7<-as.data.frame(pstr.41_50.50.7)

pstr.41_50.75.7<-sedimentation.1.7(R.pstr.t = 442800, p.c = 0.75, p.u = 0.25, HR.pstr.1 = 9)
print(pstr.41_50.75.7)
pstr.41_50.75.7<-as.data.frame(pstr.41_50.75.7)

pstr.41_50.100.7<-sedimentation.1.7(R.pstr.t = 442800, p.c = 1.00, p.u = 0.00, HR.pstr.1 = 9)
print(pstr.41_50.100.7)
pstr.41_50.100.7<-as.data.frame(pstr.41_50.100.7)

# Generate vector of number of days
Days.7 <- seq(40, 47, 1)
Days.7 <- as.data.frame(Days.7)

# Bind population estimates with Days column
pstr.41_50.0.7<-cbind(Days.7, pstr.41_50.0.7)
pstr.41_50.25.7<-cbind(Days.7, pstr.41_50.25.7)
pstr.41_50.50.7<-cbind(Days.7, pstr.41_50.50.7)
pstr.41_50.75.7<-cbind(Days.7, pstr.41_50.75.7)
pstr.41_50.100.7<-cbind(Days.7, pstr.41_50.100.7)

#### 0

# Add new column
pstr.41_50.0.7 <- pstr.41_50.0.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.0.7
  )

# Replace NAs with scenario name
pstr.41_50.0.7$Scenario <- as.character(pstr.41_50.0.7$Scenario)
pstr.41_50.0.7 <-pstr.41_50.0.7 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.41_50.0.7 <- pstr.41_50.0.7 %>% 
  rename(
    Days = Days.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 48:50, Population = 387007, Scenario = "pstr.0", Sed.event = 7)

#### 25

# Add new column
pstr.41_50.25.7 <- pstr.41_50.25.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.25.7
  )

# Replace NAs with scenario name
pstr.41_50.25.7$Scenario <- as.character(pstr.41_50.25.7$Scenario)
pstr.41_50.25.7 <-pstr.41_50.25.7 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.41_50.25.7 <- pstr.41_50.25.7 %>% 
  rename(
    Days = Days.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 48:50, Population = 275422, Scenario = "pstr.25", Sed.event = 7)

#### 50

# Add new column
pstr.41_50.50.7 <- pstr.41_50.50.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.50.7
  )

# Replace NAs with scenario name
pstr.41_50.50.7$Scenario <- as.character(pstr.41_50.50.7$Scenario)
pstr.41_50.50.7 <-pstr.41_50.50.7 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.41_50.50.7 <- pstr.41_50.50.7 %>% 
  rename(
    Days = Days.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 48:50, Population = 163836, Scenario = "pstr.50", Sed.event = 7)

### 75

# Add new column
pstr.41_50.75.7 <- pstr.41_50.75.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.75.7
  )

# Replace NAs with scenario name
pstr.41_50.75.7$Scenario <- as.character(pstr.41_50.75.7$Scenario)
pstr.41_50.75.7 <-pstr.41_50.75.7 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.41_50.75.7 <- pstr.41_50.75.7 %>% 
  rename(
    Days = Days.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 48:50, Population = 52250, Scenario = "pstr.75", Sed.event = 7)

### 100

# Add new column
pstr.41_50.100.7 <- pstr.41_50.100.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.100.7
  )

# Replace NAs with scenario name
pstr.41_50.100.7$Scenario <- as.character(pstr.41_50.100.7$Scenario)
pstr.41_50.100.7 <-pstr.41_50.100.7 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.41_50.100.7 <- pstr.41_50.100.7 %>% 
  rename(
    Days = Days.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  ) %>%
  add_row(Days = 48:50, Population = 0, Scenario = "pstr.100", Sed.event = 7)

# Bind with previous dataframe

mortality.7.50<-rbind(pstr.41_50.0.7, pstr.41_50.25.7, 
                        pstr.41_50.50.7, pstr.41_50.75.7, pstr.41_50.100.7)

# Get population estimates by day 50 for each scenario
mortality.summary.1.7 <- mortality.7.50 %>%
  filter(Days==47, Sed.event == 7)
mortality.summary.1.7

mortality.1.50.7<-rbind(mortality.1.50, mortality.7.50)


##################### 5 day sedimentation event ######################
sedimentation.1.5 <- function(R.pstr.t, p.c, p.u, HR.pstr.1) {
  
  # Calculate background mortality rate
  Mb.pstr.1 <- (0.18*R.pstr.t)/(10)
  
  # Define Tx
  T5 <- seq(0, 5, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.u)*((R.pstr.t)-(Mb.pstr.1*(x))))+((p.c)*((R.pstr.t)-(HR.pstr.1*(Mb.pstr.1*(x)))))
  }
  
  # Apply the function to Tx
  T5.test.c <- pstr.hr.1(T5)
  
  # Ensure non-negative values
  T5.test.c <- pmax(T5.test.c, 0)
  
  return(T5.test.c)
  
}



# Run scenarios
pstr.41_50.0.5<-sedimentation.1.5(R.pstr.t = 442800, p.c = 0.00, p.u = 1.00, HR.pstr.1 = 9)
print(pstr.41_50.0.5)
pstr.41_50.0.5<-as.data.frame(pstr.41_50.0.5)

pstr.41_50.25.5<-sedimentation.1.5(R.pstr.t = 442800, p.c = 0.25, p.u = 0.75, HR.pstr.1 = 9)
print(pstr.41_50.25.5)
pstr.41_50.25.5<-as.data.frame(pstr.41_50.25.5)

pstr.41_50.50.5<-sedimentation.1.5(R.pstr.t = 442800, p.c = 0.50, p.u = 0.50, HR.pstr.1 = 9)
print(pstr.41_50.50.5)
pstr.41_50.50.5<-as.data.frame(pstr.41_50.50.5)

pstr.41_50.75.5<-sedimentation.1.5(R.pstr.t = 442800, p.c = 0.75, p.u = 0.25, HR.pstr.1 = 9)
print(pstr.41_50.75.5)
pstr.41_50.75.5<-as.data.frame(pstr.41_50.75.5)

pstr.41_50.100.5<-sedimentation.1.5(R.pstr.t = 442800, p.c = 1.00, p.u = 0.00, HR.pstr.1 = 9)
print(pstr.41_50.100.5)
pstr.41_50.100.5<-as.data.frame(pstr.41_50.100.5)

# Generate vector of number of days
Days.5 <- seq(40, 45, 1)
Days.5 <- as.data.frame(Days.5)

# Bind population estimates with Days column
pstr.41_50.0.5<-cbind(Days.5, pstr.41_50.0.5)
pstr.41_50.25.5<-cbind(Days.5, pstr.41_50.25.5)
pstr.41_50.50.5<-cbind(Days.5, pstr.41_50.50.5)
pstr.41_50.75.5<-cbind(Days.5, pstr.41_50.75.5)
pstr.41_50.100.5<-cbind(Days.5, pstr.41_50.100.5)

#### 0

# Add new column
pstr.41_50.0.5 <- pstr.41_50.0.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.0.5
  )

# Replace NAs with scenario name
pstr.41_50.0.5$Scenario <- as.character(pstr.41_50.0.5$Scenario)
pstr.41_50.0.5 <-pstr.41_50.0.5 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.41_50.0.5 <- pstr.41_50.0.5 %>% 
  rename(
    Days = Days.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 46:50, Population = 402948, Scenario = "pstr.0", Sed.event = 5)

#### 25

# Add new column
pstr.41_50.25.5 <- pstr.41_50.25.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.25.5
  )

# Replace NAs with scenario name
pstr.41_50.25.5$Scenario <- as.character(pstr.41_50.25.5$Scenario)
pstr.41_50.25.5 <-pstr.41_50.25.5 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.41_50.25.5 <- pstr.41_50.25.5 %>% 
  rename(
    Days = Days.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 46:50, Population = 323244, Scenario = "pstr.25", Sed.event = 5)

#### 50

# Add new column
pstr.41_50.50.5 <- pstr.41_50.50.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.50.5
  )

# Replace NAs with scenario name
pstr.41_50.50.5$Scenario <- as.character(pstr.41_50.50.5$Scenario)
pstr.41_50.50.5 <-pstr.41_50.50.5 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.41_50.50.5 <- pstr.41_50.50.5 %>% 
  rename(
    Days = Days.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 46:50, Population = 243540, Scenario = "pstr.50", Sed.event = 5)

### 75

# Add new column
pstr.41_50.75.5 <- pstr.41_50.75.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.75.5
  )

# Replace NAs with scenario name
pstr.41_50.75.5$Scenario <- as.character(pstr.41_50.75.5$Scenario)
pstr.41_50.75.5 <-pstr.41_50.75.5 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.41_50.75.5 <- pstr.41_50.75.5 %>% 
  rename(
    Days = Days.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 46:50, Population = 163836, Scenario = "pstr.75", Sed.event = 5)

### 100

# Add new column
pstr.41_50.100.5 <- pstr.41_50.100.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.100.5
  )

# Replace NAs with scenario name
pstr.41_50.100.5$Scenario <- as.character(pstr.41_50.100.5$Scenario)
pstr.41_50.100.5 <-pstr.41_50.100.5 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.41_50.100.5 <- pstr.41_50.100.5 %>% 
  rename(
    Days = Days.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  ) %>%
  add_row(Days = 46:50, Population = 84132, Scenario = "pstr.100", Sed.event = 5)


# Bind with previous dataframe
mortality.1.50.7.5<-rbind(mortality.1.50.7, pstr.41_50.0.5, pstr.41_50.25.5, 
                        pstr.41_50.50.5, pstr.41_50.75.5, pstr.41_50.100.5)



##################### 3 day sedimentation event ######################
sedimentation.1.3 <- function(R.pstr.t, p.c, p.u, HR.pstr.1) {
  
  # Calculate background mortality rate
  Mb.pstr.1 <- (0.18*R.pstr.t)/(10)
  
  # Define Tx
  T3 <- seq(0, 3, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.u)*((R.pstr.t)-(Mb.pstr.1*(x))))+((p.c)*((R.pstr.t)-(HR.pstr.1*(Mb.pstr.1*(x)))))
  }
  
  # Apply the function to Tx
  T3.test.c <- pstr.hr.1(T3)
  
  # Ensure non-negative values
  T3.test.c <- pmax(T3.test.c, 0)
  
  return(T3.test.c)
  
}



# Run scenarios
pstr.41_50.0.3<-sedimentation.1.3(R.pstr.t = 442800, p.c = 0.00, p.u = 1.00, HR.pstr.1 = 9)
print(pstr.41_50.0.3)
pstr.41_50.0.3<-as.data.frame(pstr.41_50.0.3)

pstr.41_50.25.3<-sedimentation.1.3(R.pstr.t = 442800, p.c = 0.25, p.u = 0.75, HR.pstr.1 = 9)
print(pstr.41_50.25.3)
pstr.41_50.25.3<-as.data.frame(pstr.41_50.25.3)

pstr.41_50.50.3<-sedimentation.1.3(R.pstr.t = 442800, p.c = 0.50, p.u = 0.50, HR.pstr.1 = 9)
print(pstr.41_50.50.3)
pstr.41_50.50.3<-as.data.frame(pstr.41_50.50.3)

pstr.41_50.75.3<-sedimentation.1.3(R.pstr.t = 442800, p.c = 0.75, p.u = 0.25, HR.pstr.1 = 9)
print(pstr.41_50.75.3)
pstr.41_50.75.3<-as.data.frame(pstr.41_50.75.3)

pstr.41_50.100.3<-sedimentation.1.3(R.pstr.t = 442800, p.c = 1.00, p.u = 0.00, HR.pstr.1 = 9)
print(pstr.41_50.100.3)
pstr.41_50.100.3<-as.data.frame(pstr.41_50.100.3)

# Generate vector of number of days
Days.3 <- seq(40, 43, 1)
Days.3 <- as.data.frame(Days.3)

# Bind population estimates with Days column
pstr.41_50.0.3<-cbind(Days.3, pstr.41_50.0.3)
pstr.41_50.25.3<-cbind(Days.3, pstr.41_50.25.3)
pstr.41_50.50.3<-cbind(Days.3, pstr.41_50.50.3)
pstr.41_50.75.3<-cbind(Days.3, pstr.41_50.75.3)
pstr.41_50.100.3<-cbind(Days.3, pstr.41_50.100.3)

#### 0

# Add new column
pstr.41_50.0.3 <- pstr.41_50.0.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.0.3
  )

# Replace NAs with scenario name
pstr.41_50.0.3$Scenario <- as.character(pstr.41_50.0.3$Scenario)
pstr.41_50.0.3 <-pstr.41_50.0.3 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.41_50.0.3 <- pstr.41_50.0.3 %>% 
  rename(
    Days = Days.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 44:50, Population = 418889, Scenario = "pstr.0", Sed.event = 3)

#### 25

# Add new column
pstr.41_50.25.3 <- pstr.41_50.25.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.25.3
  )

# Replace NAs with scenario name
pstr.41_50.25.3$Scenario <- as.character(pstr.41_50.25.3$Scenario)
pstr.41_50.25.3 <-pstr.41_50.25.3 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.41_50.25.3 <- pstr.41_50.25.3 %>% 
  rename(
    Days = Days.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 44:50, Population = 371066, Scenario = "pstr.25", Sed.event = 3)

#### 50

# Add new column
pstr.41_50.50.3 <- pstr.41_50.50.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.50.3
  )

# Replace NAs with scenario name
pstr.41_50.50.3$Scenario <- as.character(pstr.41_50.50.3$Scenario)
pstr.41_50.50.3 <-pstr.41_50.50.3 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.41_50.50.3 <- pstr.41_50.50.3 %>% 
  rename(
    Days = Days.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 44:50, Population = 323244, Scenario = "pstr.50", Sed.event = 3)

### 75

# Add new column
pstr.41_50.75.3 <- pstr.41_50.75.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.75.3
  )

# Replace NAs with scenario name
pstr.41_50.75.3$Scenario <- as.character(pstr.41_50.75.3$Scenario)
pstr.41_50.75.3 <-pstr.41_50.75.3 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.41_50.75.3 <- pstr.41_50.75.3 %>% 
  rename(
    Days = Days.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )  %>%
  add_row(Days = 44:50, Population = 275422, Scenario = "pstr.75", Sed.event = 3)

### 100

# Add new column
pstr.41_50.100.3 <- pstr.41_50.100.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.100.3
  )

# Replace NAs with scenario name
pstr.41_50.100.3$Scenario <- as.character(pstr.41_50.100.3$Scenario)
pstr.41_50.100.3 <-pstr.41_50.100.3 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.41_50.100.3 <- pstr.41_50.100.3 %>% 
  rename(
    Days = Days.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  ) %>%
  add_row(Days = 44:50, Population = 227599, Scenario = "pstr.100", Sed.event = 3)


# Bind with previous dataframe
mortality.1.50.7.5.3<-rbind(mortality.1.50.7.5, pstr.41_50.0.3, pstr.41_50.25.3, 
                        pstr.41_50.50.3, pstr.41_50.75.3, pstr.41_50.100.3)


# Get population estimates by day 50 for each scenario
#mortality.summary.1.5 <- mortality.1.50.5 %>%
#  filter(Days==45, Sed.event == 5)
#mortality.summary.1.5


Background_m2 <- function(R.pstr.t, p.c, p.u) {
  
  # Define Tx
  T1 <- seq(0, 10, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.1 <- function(x) {
    ((p.c)*((R.pstr.t)-(Mb.pstr.1*x)))+((p.u)*((R.pstr.t)-(Mb.pstr.1*x)))
  }
  
  # Apply the function to Tx
  T1.test.c <- pstr.hr.1(T1)
  
  # Ensure non-negative values
  T1.test.c <- pmax(T1.test.c, 0)
  
  return(T1.test.c)
}

# Run scenarios
pstr.41_50.0.0 <- Background_m2(R.pstr.t = 442800, p.c = 0.00, p.u = 1.00)
print(pstr.41_50.0.0)
pstr.41_50.0.0 <- as.data.frame(pstr.41_50.0.0)

# Generate vector of number of days
Days.0 <- seq(40, 50, 1)
Days.0 <- as.data.frame(Days.0)

# Bind population estimates with Days column
pstr.41_50.0.0<-cbind(Days.0, pstr.41_50.0.0)


# Add new column
pstr.41_50.0.0 <- pstr.41_50.0.0 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.41_50.0.0
  )

# Replace NAs with scenario name
pstr.41_50.0.0$Scenario <- as.character(pstr.41_50.0.0$Scenario)
pstr.41_50.0.0 <-pstr.41_50.0.0 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.41_50.0.0 <- pstr.41_50.0.0 %>% 
  rename(
    Days = Days.0
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Review code for first 30 days, change value of Sed.event column to 0
# Get rid of 0 sed events for pstr 25 and up 

write.csv(mortality.1.50.7.5.3, "mortality.1.50.7.5.3_R.t_V2.csv", row.names=FALSE)


######################### Background mortality 60 -- 90 ######################

Background_m2.2 <- function(R.pstr.t, p.c, p.u) {
  
  Mb.pstr.2 <- (0.02*R.pstr.t)
  
  # Define Tx
  T2 <- seq(1, 30, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.2 <- function(x) {
    ((p.c)*((R.pstr.t)-(Mb.pstr.2*x)))+((p.u)*((R.pstr.t)-(Mb.pstr.2*x)))
  }
  
  # Apply the function to Tx
  T2.test.c <- pstr.hr.2(T2)
  
  # Ensure non-negative values
  T2.test.c <- pmax(T2.test.c, 0)
  
  return(T2.test.c)
}

# Get population estimates by day 50 for each scenario
mortality.by.50 <- mortality.1.50.7.5.3 %>%
  filter(Days==50)
mortality.by.50 

# Days Population Scenario Sed.event
#  50     363096   pstr.0        10
#  50     203688  pstr.25        10
#  50      44280  pstr.50        10
#  50          0  pstr.75        10
#  50          0 pstr.100        10
#  50     275422  pstr.25         7
#  50     163836  pstr.50         7
#  50      52250  pstr.75         7
#  50          0 pstr.100         7
#  50     323244  pstr.25         5
#  50     243540  pstr.50         5
#  50     163836  pstr.75         5
#  50      84132 pstr.100         5
#  50     371066  pstr.25         3
#  50     323244  pstr.50         3
#  50     275422  pstr.75         3
#  50     227599 pstr.100         3

# Run scenarios
pstr.60_90.0.3 <- Background_m2.2(R.pstr.t = 363096, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.0.3)
pstr.60_90.0.3 <- as.data.frame(pstr.60_90.0.3)

pstr.60_90.0.5 <- Background_m2.2(R.pstr.t = 363096, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.0.5)
pstr.60_90.0.5 <- as.data.frame(pstr.60_90.0.5)

pstr.60_90.0.7 <- Background_m2.2(R.pstr.t = 363096, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.0.7)
pstr.60_90.0.7 <- as.data.frame(pstr.60_90.0.7)

pstr.60_90.0.10 <- Background_m2.2(R.pstr.t = 363096, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.0.10)
pstr.60_90.0.10 <- as.data.frame(pstr.60_90.0.10)

pstr.60_90.25.3 <- Background_m2.2(R.pstr.t = 315276, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.25.3)
pstr.60_90.25.3 <- as.data.frame(pstr.60_90.25.3)

pstr.60_90.25.5 <- Background_m2.2(R.pstr.t = 283394, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.25.5)
pstr.60_90.25.5 <- as.data.frame(pstr.60_90.25.5)

pstr.60_90.25.7 <- Background_m2.2(R.pstr.t = 251512, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.25.7)
pstr.60_90.25.7 <- as.data.frame(pstr.60_90.25.7)

pstr.60_90.25.10 <- Background_m2.2(R.pstr.t = 203688, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.25.10)
pstr.60_90.25.10 <- as.data.frame(pstr.60_90.25.10)

pstr.60_90.50.3 <- Background_m2.2(R.pstr.t = 267454, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.50.3)
pstr.60_90.50.3 <- as.data.frame(pstr.60_90.50.3)

pstr.60_90.50.5 <- Background_m2.2(R.pstr.t = 203690, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.50.5)
pstr.60_90.50.5 <- as.data.frame(pstr.60_90.50.5)

pstr.60_90.50.7 <- Background_m2.2(R.pstr.t = 139926, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.50.7)
pstr.60_90.50.7 <- as.data.frame(pstr.60_90.50.7)

pstr.60_90.50.10 <- Background_m2.2(R.pstr.t = 44280, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.50.10)
pstr.60_90.50.10 <- as.data.frame(pstr.60_90.50.10)

pstr.60_90.75.3 <- Background_m2.2(R.pstr.t = 219632, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.75.3)
pstr.60_90.75.3 <- as.data.frame(pstr.60_90.75.3)

pstr.60_90.75.5 <- Background_m2.2(R.pstr.t = 123986, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.75.5)
pstr.60_90.75.5 <- as.data.frame(pstr.60_90.75.5)

pstr.60_90.75.7 <- Background_m2.2(R.pstr.t = 28340, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.75.7)
pstr.60_90.75.7 <- as.data.frame(pstr.60_90.75.7)

pstr.60_90.75.10 <- Background_m2.2(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.75.10)
pstr.60_90.75.10 <- as.data.frame(pstr.60_90.75.10)

pstr.60_90.100.3 <- Background_m2.2(R.pstr.t = 171809, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.100.3)
pstr.60_90.100.3 <- as.data.frame(pstr.60_90.100.3)

pstr.60_90.100.5 <- Background_m2.2(R.pstr.t = 44282, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.100.5)
pstr.60_90.100.5 <- as.data.frame(pstr.60_90.100.5)

pstr.60_90.100.7 <- Background_m2.2(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.100.7)
pstr.60_90.100.7 <- as.data.frame(pstr.60_90.100.7)

pstr.60_90.100.10 <- Background_m2.2(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.60_90.100.10)
pstr.60_90.100.10 <- as.data.frame(pstr.60_90.100.10)

# Generate vector of number of days
Days.6.9 <- seq(61, 90, 1)
Days.6.9 <- as.data.frame(Days.6.9)

# Bind population estimates with Days column
pstr.60_90.0.3<-cbind(Days.6.9, pstr.60_90.0.3)
pstr.60_90.0.5<-cbind(Days.6.9, pstr.60_90.0.5)
pstr.60_90.0.7<-cbind(Days.6.9, pstr.60_90.0.7)
pstr.60_90.0.10<-cbind(Days.6.9, pstr.60_90.0.10)
pstr.60_90.25.3<-cbind(Days.6.9, pstr.60_90.25.3)
pstr.60_90.25.5<-cbind(Days.6.9, pstr.60_90.25.5)
pstr.60_90.25.7<-cbind(Days.6.9, pstr.60_90.25.7)
pstr.60_90.25.10<-cbind(Days.6.9, pstr.60_90.25.10)
pstr.60_90.50.3<-cbind(Days.6.9, pstr.60_90.50.3)
pstr.60_90.50.5<-cbind(Days.6.9, pstr.60_90.50.5)
pstr.60_90.50.7<-cbind(Days.6.9, pstr.60_90.50.7)
pstr.60_90.50.10<-cbind(Days.6.9, pstr.60_90.50.10)
pstr.60_90.75.3<-cbind(Days.6.9, pstr.60_90.75.3)
pstr.60_90.75.5<-cbind(Days.6.9, pstr.60_90.75.5)
pstr.60_90.75.7<-cbind(Days.6.9, pstr.60_90.75.7)
pstr.60_90.75.10<-cbind(Days.6.9, pstr.60_90.75.10)
pstr.60_90.100.3<-cbind(Days.6.9, pstr.60_90.100.3)
pstr.60_90.100.5<-cbind(Days.6.9, pstr.60_90.100.5)
pstr.60_90.100.7<-cbind(Days.6.9, pstr.60_90.100.7)
pstr.60_90.100.10<-cbind(Days.6.9, pstr.60_90.100.10)


#### 0

# Add new column
pstr.60_90.0.3 <- pstr.60_90.0.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.0.3
  )

# Replace NAs with scenario name
pstr.60_90.0.3$Scenario <- as.character(pstr.60_90.0.3$Scenario)
pstr.60_90.0.3 <-pstr.60_90.0.3 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.60_90.0.3 <- pstr.60_90.0.3 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.0.5 <- pstr.60_90.0.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.0.5
  )

# Replace NAs with scenario name
pstr.60_90.0.5$Scenario <- as.character(pstr.60_90.0.5$Scenario)
pstr.60_90.0.5 <-pstr.60_90.0.5 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.60_90.0.5 <- pstr.60_90.0.5 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.0.7 <- pstr.60_90.0.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.0.7
  )

# Replace NAs with scenario name
pstr.60_90.0.7$Scenario <- as.character(pstr.60_90.0.7$Scenario)
pstr.60_90.0.7 <-pstr.60_90.0.7 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.60_90.0.7 <- pstr.60_90.0.7 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.0.10 <- pstr.60_90.0.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.0.10
  )

# Replace NAs with scenario name
pstr.60_90.0.10$Scenario <- as.character(pstr.60_90.0.10$Scenario)
pstr.60_90.0.10 <-pstr.60_90.0.10 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.60_90.0.10 <- pstr.60_90.0.10 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.25.3 <- pstr.60_90.25.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.25.3
  )

# Replace NAs with scenario name
pstr.60_90.25.3$Scenario <- as.character(pstr.60_90.25.3$Scenario)
pstr.60_90.25.3 <-pstr.60_90.25.3 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.60_90.25.3 <- pstr.60_90.25.3 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.25.5 <- pstr.60_90.25.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.25.5
  )

# Replace NAs with scenario name
pstr.60_90.25.5$Scenario <- as.character(pstr.60_90.25.5$Scenario)
pstr.60_90.25.5 <-pstr.60_90.25.5 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.60_90.25.5 <- pstr.60_90.25.5 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.25.7 <- pstr.60_90.25.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.25.7
  )

# Replace NAs with scenario name
pstr.60_90.25.7$Scenario <- as.character(pstr.60_90.25.7$Scenario)
pstr.60_90.25.7 <-pstr.60_90.25.7 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.60_90.25.7 <- pstr.60_90.25.7 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.25.10 <- pstr.60_90.25.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.25.10
  )

# Replace NAs with scenario name
pstr.60_90.25.10$Scenario <- as.character(pstr.60_90.25.10$Scenario)
pstr.60_90.25.10 <-pstr.60_90.25.10 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.60_90.25.10 <- pstr.60_90.25.10 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.50.3 <- pstr.60_90.50.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.50.3
  )

# Replace NAs with scenario name
pstr.60_90.50.3$Scenario <- as.character(pstr.60_90.50.3$Scenario)
pstr.60_90.50.3 <-pstr.60_90.50.3 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.60_90.50.3 <- pstr.60_90.50.3 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.50.5 <- pstr.60_90.50.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.50.5
  )

# Replace NAs with scenario name
pstr.60_90.50.5$Scenario <- as.character(pstr.60_90.50.5$Scenario)
pstr.60_90.50.5 <-pstr.60_90.50.5 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.60_90.50.5 <- pstr.60_90.50.5 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.50.7 <- pstr.60_90.50.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.50.7
  )

# Replace NAs with scenario name
pstr.60_90.50.7$Scenario <- as.character(pstr.60_90.50.7$Scenario)
pstr.60_90.50.7 <-pstr.60_90.50.7 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.60_90.50.7 <- pstr.60_90.50.7 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.50.10 <- pstr.60_90.50.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.50.10
  )

# Replace NAs with scenario name
pstr.60_90.50.10$Scenario <- as.character(pstr.60_90.50.10$Scenario)
pstr.60_90.50.10 <-pstr.60_90.50.10 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.60_90.50.10 <- pstr.60_90.50.10 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.75.3 <- pstr.60_90.75.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.75.3
  )

# Replace NAs with scenario name
pstr.60_90.75.3$Scenario <- as.character(pstr.60_90.75.3$Scenario)
pstr.60_90.75.3 <-pstr.60_90.75.3 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.60_90.75.3 <- pstr.60_90.75.3 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.75.5 <- pstr.60_90.75.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.75.5
  )

# Replace NAs with scenario name
pstr.60_90.75.5$Scenario <- as.character(pstr.60_90.75.5$Scenario)
pstr.60_90.75.5 <-pstr.60_90.75.5 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.60_90.75.5 <- pstr.60_90.75.5 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.75.7 <- pstr.60_90.75.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.75.7
  )

# Replace NAs with scenario name
pstr.60_90.75.7$Scenario <- as.character(pstr.60_90.75.7$Scenario)
pstr.60_90.75.7 <-pstr.60_90.75.7 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.60_90.75.7 <- pstr.60_90.75.7 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.75.10 <- pstr.60_90.75.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.75.10
  )

# Replace NAs with scenario name
pstr.60_90.75.10$Scenario <- as.character(pstr.60_90.75.10$Scenario)
pstr.60_90.75.10 <-pstr.60_90.75.10 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.60_90.75.10 <- pstr.60_90.75.10 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.100.3 <- pstr.60_90.100.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.100.3
  )

# Replace NAs with scenario name
pstr.60_90.100.3$Scenario <- as.character(pstr.60_90.100.3$Scenario)
pstr.60_90.100.3 <-pstr.60_90.100.3 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.60_90.100.3 <- pstr.60_90.100.3 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.100.5 <- pstr.60_90.100.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.100.5
  )

# Replace NAs with scenario name
pstr.60_90.100.5$Scenario <- as.character(pstr.60_90.100.5$Scenario)
pstr.60_90.100.5 <-pstr.60_90.100.5 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.60_90.100.5 <- pstr.60_90.100.5 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.100.7 <- pstr.60_90.100.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.100.7
  )

# Replace NAs with scenario name
pstr.60_90.100.7$Scenario <- as.character(pstr.60_90.100.7$Scenario)
pstr.60_90.100.7 <-pstr.60_90.100.7 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.60_90.100.7 <- pstr.60_90.100.7 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.60_90.100.10 <- pstr.60_90.100.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.60_90.100.10
  )

# Replace NAs with scenario name
pstr.60_90.100.10$Scenario <- as.character(pstr.60_90.100.10$Scenario)
pstr.60_90.100.10 <-pstr.60_90.100.10 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.60_90.100.10 <- pstr.60_90.100.10 %>% 
  rename(
    Days = Days.6.9
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


mortality.60_90 <- rbind(pstr.60_90.0.3, pstr.60_90.0.5, pstr.60_90.0.7, pstr.60_90.0.10,
                         pstr.60_90.25.3, pstr.60_90.25.5, pstr.60_90.25.7, pstr.60_90.25.10,
                         pstr.60_90.50.3, pstr.60_90.50.5, pstr.60_90.50.7, pstr.60_90.50.10,
                         pstr.60_90.75.3, pstr.60_90.75.5, pstr.60_90.75.7, pstr.60_90.75.10,
                         pstr.60_90.100.3, pstr.60_90.100.5, pstr.60_90.100.7, pstr.60_90.100.10)


# Get population estimates by day 100 for each scenario
mortality.by.90 <- mortality.60_90 %>%
  filter(Days==90)
mortality.by.90 


######################## Background mortality 91 -- 100 #####################

# mortality.0_90<-read.csv("mortality.1.50.7.5.3_V2.csv")
setwd("~/Library/CloudStorage/OneDrive-UniversityofMiami/CIMAS/Recruits Sed Exposures")

#mortality.0_90<-read.csv("mortality.0_90.R.t.csv")

#mortality.0_90.2<-read.csv("mortality.0_90_R.t_V2.csv")


Background_m3 <- function(R.pstr.t, p.c, p.u) {
  
  Mb.pstr.3 <- (0.14*R.pstr.t)/(10)
  
  # Define Tx
  T3 <- seq(1, 10, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.3 <- function(x) {
    ((p.c)*((R.pstr.t)-(Mb.pstr.3*x)))+((p.u)*((R.pstr.t)-(Mb.pstr.3*x)))
  }
  
  # Apply the function to Tx
  T3.test.c <- pstr.hr.3(T3)
  
  # Ensure non-negative values
  T3.test.c <- pmax(T3.test.c, 0)
  
  return(T3.test.c)
}

# Get population estimates by day 90 for each scenario
#mortality.by.90 <- mortality.0_90.2 %>%
#  filter(Days==90)
#mortality.by.90 

# Run scenarios
pstr.91_100.0.3 <- Background_m3(R.pstr.t = 145238, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.0.3)
pstr.91_100.0.3 <- as.data.frame(pstr.91_100.0.3)

pstr.91_100.0.5 <- Background_m3(R.pstr.t = 145238, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.0.5)
pstr.91_100.0.5 <- as.data.frame(pstr.91_100.0.5)

pstr.91_100.0.7 <- Background_m3(R.pstr.t = 145238, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.0.7)
pstr.91_100.0.7 <- as.data.frame(pstr.91_100.0.7)

pstr.91_100.0.10 <- Background_m3(R.pstr.t = 145238, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.0.10)
pstr.91_100.0.10 <- as.data.frame(pstr.91_100.0.10)

pstr.91_100.25.3 <- Background_m3(R.pstr.t = 126110, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.25.3)
pstr.91_100.25.3 <- as.data.frame(pstr.91_100.25.3)

pstr.91_100.25.5 <- Background_m3(R.pstr.t = 113358, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.25.5)
pstr.91_100.25.5 <- as.data.frame(pstr.91_100.25.5)

pstr.91_100.25.7 <- Background_m3(R.pstr.t = 100605, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.25.7)
pstr.91_100.25.7 <- as.data.frame(pstr.91_100.25.7)

pstr.91_100.25.10 <- Background_m3(R.pstr.t = 81475, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.25.10)
pstr.91_100.25.10 <- as.data.frame(pstr.91_100.25.10)

pstr.91_100.50.3 <- Background_m3(R.pstr.t = 106982, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.50.3)
pstr.91_100.50.3 <- as.data.frame(pstr.91_100.50.3)

pstr.91_100.50.5 <- Background_m3(R.pstr.t = 81476, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.50.5)
pstr.91_100.50.5 <- as.data.frame(pstr.91_100.50.5)

pstr.91_100.50.7 <- Background_m3(R.pstr.t = 55970, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.50.7)
pstr.91_100.50.7 <- as.data.frame(pstr.91_100.50.7)

pstr.91_100.50.10 <- Background_m3(R.pstr.t = 17712, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.50.10)
pstr.91_100.50.10 <- as.data.frame(pstr.91_100.50.10)

pstr.91_100.75.3 <- Background_m3(R.pstr.t = 87853, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.75.3)
pstr.91_100.75.3 <- as.data.frame(pstr.91_100.75.3)

pstr.91_100.75.5 <- Background_m3(R.pstr.t = 49594, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.75.5)
pstr.91_100.75.5 <- as.data.frame(pstr.91_100.75.5)

pstr.91_100.75.7 <- Background_m3(R.pstr.t = 11336, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.75.7)
pstr.91_100.75.7 <- as.data.frame(pstr.91_100.75.7)

pstr.91_100.75.10 <- Background_m3(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.75.10)
pstr.91_100.75.10 <- as.data.frame(pstr.91_100.75.10)

pstr.91_100.100.3 <- Background_m3(R.pstr.t = 68724, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.100.3)
pstr.91_100.100.3 <- as.data.frame(pstr.91_100.100.3)

pstr.91_100.100.5 <- Background_m3(R.pstr.t = 17713, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.100.5)
pstr.91_100.100.5 <- as.data.frame(pstr.91_100.100.5)

pstr.91_100.100.7 <- Background_m3(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.100.7)
pstr.91_100.100.7 <- as.data.frame(pstr.91_100.100.7)

pstr.91_100.100.10 <- Background_m3(R.pstr.t = 0, p.c = 0.00, p.u = 1.00)
print(pstr.91_100.100.10)
pstr.91_100.100.10 <- as.data.frame(pstr.91_100.100.10)

# Generate vector of number of days
Days.0.3 <- seq(91, 100, 1)
Days.0.3 <- as.data.frame(Days.0.3)

# Bind population estimates with Days column
pstr.91_100.0.3<-cbind(Days.0.3, pstr.91_100.0.3)
pstr.91_100.0.5<-cbind(Days.0.3, pstr.91_100.0.5)
pstr.91_100.0.7<-cbind(Days.0.3, pstr.91_100.0.7)
pstr.91_100.0.10<-cbind(Days.0.3, pstr.91_100.0.10)
pstr.91_100.25.3<-cbind(Days.0.3, pstr.91_100.25.3)
pstr.91_100.25.5<-cbind(Days.0.3, pstr.91_100.25.5)
pstr.91_100.25.7<-cbind(Days.0.3, pstr.91_100.25.7)
pstr.91_100.25.10<-cbind(Days.0.3, pstr.91_100.25.10)
pstr.91_100.50.3<-cbind(Days.0.3, pstr.91_100.50.3)
pstr.91_100.50.5<-cbind(Days.0.3, pstr.91_100.50.5)
pstr.91_100.50.7<-cbind(Days.0.3, pstr.91_100.50.7)
pstr.91_100.50.10<-cbind(Days.0.3, pstr.91_100.50.10)
pstr.91_100.75.3<-cbind(Days.0.3, pstr.91_100.75.3)
pstr.91_100.75.5<-cbind(Days.0.3, pstr.91_100.75.5)
pstr.91_100.75.7<-cbind(Days.0.3, pstr.91_100.75.7)
pstr.91_100.75.10<-cbind(Days.0.3, pstr.91_100.75.10)
pstr.91_100.100.3<-cbind(Days.0.3, pstr.91_100.100.3)
pstr.91_100.100.5<-cbind(Days.0.3, pstr.91_100.100.5)
pstr.91_100.100.7<-cbind(Days.0.3, pstr.91_100.100.7)
pstr.91_100.100.10<-cbind(Days.0.3, pstr.91_100.100.10)


#### 0

# Add new column
pstr.91_100.0.3 <- pstr.91_100.0.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.0.3
  )

# Replace NAs with scenario name
pstr.91_100.0.3$Scenario <- as.character(pstr.91_100.0.3$Scenario)
pstr.91_100.0.3 <-pstr.91_100.0.3 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.91_100.0.3 <- pstr.91_100.0.3 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.0.5 <- pstr.91_100.0.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.0.5
  )

# Replace NAs with scenario name
pstr.91_100.0.5$Scenario <- as.character(pstr.91_100.0.5$Scenario)
pstr.91_100.0.5 <-pstr.91_100.0.5 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.91_100.0.5 <- pstr.91_100.0.5 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.0.7 <- pstr.91_100.0.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.0.7
  )

# Replace NAs with scenario name
pstr.91_100.0.7$Scenario <- as.character(pstr.91_100.0.7$Scenario)
pstr.91_100.0.7 <-pstr.91_100.0.7 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.91_100.0.7 <- pstr.91_100.0.7 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.0.10 <- pstr.91_100.0.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.0.10
  )

# Replace NAs with scenario name
pstr.91_100.0.10$Scenario <- as.character(pstr.91_100.0.10$Scenario)
pstr.91_100.0.10 <-pstr.91_100.0.10 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.91_100.0.10 <- pstr.91_100.0.10 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.25.3 <- pstr.91_100.25.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.25.3
  )

# Replace NAs with scenario name
pstr.91_100.25.3$Scenario <- as.character(pstr.91_100.25.3$Scenario)
pstr.91_100.25.3 <-pstr.91_100.25.3 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.91_100.25.3 <- pstr.91_100.25.3 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.25.5 <- pstr.91_100.25.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.25.5
  )

# Replace NAs with scenario name
pstr.91_100.25.5$Scenario <- as.character(pstr.91_100.25.5$Scenario)
pstr.91_100.25.5 <-pstr.91_100.25.5 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.91_100.25.5 <- pstr.91_100.25.5 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.25.7 <- pstr.91_100.25.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.25.7
  )

# Replace NAs with scenario name
pstr.91_100.25.7$Scenario <- as.character(pstr.91_100.25.7$Scenario)
pstr.91_100.25.7 <-pstr.91_100.25.7 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.91_100.25.7 <- pstr.91_100.25.7 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.25.10 <- pstr.91_100.25.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.25.10
  )

# Replace NAs with scenario name
pstr.91_100.25.10$Scenario <- as.character(pstr.91_100.25.10$Scenario)
pstr.91_100.25.10 <-pstr.91_100.25.10 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.91_100.25.10 <- pstr.91_100.25.10 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.50.3 <- pstr.91_100.50.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.50.3
  )

# Replace NAs with scenario name
pstr.91_100.50.3$Scenario <- as.character(pstr.91_100.50.3$Scenario)
pstr.91_100.50.3 <-pstr.91_100.50.3 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.91_100.50.3 <- pstr.91_100.50.3 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.50.5 <- pstr.91_100.50.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.50.5
  )

# Replace NAs with scenario name
pstr.91_100.50.5$Scenario <- as.character(pstr.91_100.50.5$Scenario)
pstr.91_100.50.5 <-pstr.91_100.50.5 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.91_100.50.5 <- pstr.91_100.50.5 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.50.7 <- pstr.91_100.50.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.50.7
  )

# Replace NAs with scenario name
pstr.91_100.50.7$Scenario <- as.character(pstr.91_100.50.7$Scenario)
pstr.91_100.50.7 <-pstr.91_100.50.7 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.91_100.50.7 <- pstr.91_100.50.7 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.50.10 <- pstr.91_100.50.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.50.10
  )

# Replace NAs with scenario name
pstr.91_100.50.10$Scenario <- as.character(pstr.91_100.50.10$Scenario)
pstr.91_100.50.10 <-pstr.91_100.50.10 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.91_100.50.10 <- pstr.91_100.50.10 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.75.3 <- pstr.91_100.75.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.75.3
  )

# Replace NAs with scenario name
pstr.91_100.75.3$Scenario <- as.character(pstr.91_100.75.3$Scenario)
pstr.91_100.75.3 <-pstr.91_100.75.3 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.91_100.75.3 <- pstr.91_100.75.3 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.75.5 <- pstr.91_100.75.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.75.5
  )

# Replace NAs with scenario name
pstr.91_100.75.5$Scenario <- as.character(pstr.91_100.75.5$Scenario)
pstr.91_100.75.5 <-pstr.91_100.75.5 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.91_100.75.5 <- pstr.91_100.75.5 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.75.7 <- pstr.91_100.75.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.75.7
  )

# Replace NAs with scenario name
pstr.91_100.75.7$Scenario <- as.character(pstr.91_100.75.7$Scenario)
pstr.91_100.75.7 <-pstr.91_100.75.7 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.91_100.75.7 <- pstr.91_100.75.7 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.75.10 <- pstr.91_100.75.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.75.10
  )

# Replace NAs with scenario name
pstr.91_100.75.10$Scenario <- as.character(pstr.91_100.75.10$Scenario)
pstr.91_100.75.10 <-pstr.91_100.75.10 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.91_100.75.10 <- pstr.91_100.75.10 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.100.3 <- pstr.91_100.100.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.100.3
  )

# Replace NAs with scenario name
pstr.91_100.100.3$Scenario <- as.character(pstr.91_100.100.3$Scenario)
pstr.91_100.100.3 <-pstr.91_100.100.3 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.91_100.100.3 <- pstr.91_100.100.3 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.100.5 <- pstr.91_100.100.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.100.5
  )

# Replace NAs with scenario name
pstr.91_100.100.5$Scenario <- as.character(pstr.91_100.100.5$Scenario)
pstr.91_100.100.5 <-pstr.91_100.100.5 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.91_100.100.5 <- pstr.91_100.100.5 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.100.7 <- pstr.91_100.100.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.100.7
  )

# Replace NAs with scenario name
pstr.91_100.100.7$Scenario <- as.character(pstr.91_100.100.7$Scenario)
pstr.91_100.100.7 <-pstr.91_100.100.7 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.91_100.100.7 <- pstr.91_100.100.7 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


# Add new column
pstr.91_100.100.10 <- pstr.91_100.100.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.91_100.100.10
  )

# Replace NAs with scenario name
pstr.91_100.100.10$Scenario <- as.character(pstr.91_100.100.10$Scenario)
pstr.91_100.100.10 <-pstr.91_100.100.10 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.91_100.100.10 <- pstr.91_100.100.10 %>% 
  rename(
    Days = Days.0.3
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


mortality.60_100 <- rbind(mortality.60_90, 
      pstr.91_100.0.3, pstr.91_100.0.5, pstr.91_100.0.7, pstr.91_100.0.10,
      pstr.91_100.25.3, pstr.91_100.25.5, pstr.91_100.25.7, pstr.91_100.25.10,
      pstr.91_100.50.3, pstr.91_100.50.5, pstr.91_100.50.7, pstr.91_100.50.10,
      pstr.91_100.75.3, pstr.91_100.75.5, pstr.91_100.75.7, pstr.91_100.75.10,
      pstr.91_100.100.3, pstr.91_100.100.5, pstr.91_100.100.7, pstr.91_100.100.10)


# Get population estimates by day 100 for each scenario
mortality.by.100 <- mortality.60_100 %>%
  filter(Days==100)
mortality.by.100 

##################### 3 day sedimentation event (3 mo) ######################


sedimentation.3.3 <- function(R.pstr.t, p.c, p.u, HR.pstr.3) {
  
  # Calculate background mortality rate
  Mb.pstr.3 <- (0.14 * R.pstr.t) / 10
  
  # Define Tx
  T3 <- seq(1, 10, 1)  # Extended range to include x > 3
  
  # Initialize a vector to store results
  results <- numeric(length(T3))
  
  # Variable to store R.pstr.t at x = 3
  R_pstr_3 <- NA
  
  # Define the function to correct hazard ratio
  for (i in seq_along(T3)) {
    x <- T3[i]
    
    if (x <= 3) {
      results[i] <- ((p.u) * ((R.pstr.t) - (Mb.pstr.3 * x))) + 
        ((p.c) * ((R.pstr.t) - (HR.pstr.3 * (Mb.pstr.3 * x))))
      
      # Store the result at x = 3
      if (x == 3) {
        R_pstr_3 <- results[i]
      }
      
    } else {
      # Use the stored R_pstr_3 value from x = 3
      results[i] <- ((p.u) * (R_pstr_3 - (Mb.pstr.3 * x))) + 
        ((p.c) * (R_pstr_3 - (Mb.pstr.3 * x)))
    }
  }

  # Ensure non-negative values
  results <- pmax(results, 0)
  
  return(results)
}

# Run scenarios
pstr.101_110.0.3<-sedimentation.3.3(R.pstr.t = 124905, p.c = 0.00, p.u = 1.00, HR.pstr.3 = 13)
print(pstr.101_110.0.3)
pstr.101_110.0.3<-as.data.frame(pstr.101_110.0.3)

pstr.101_110.25.3<-sedimentation.3.3(R.pstr.t = 108455, p.c = 0.25, p.u = 0.75, HR.pstr.3 = 13)
print(pstr.101_110.25.3)
pstr.101_110.25.3<-as.data.frame(pstr.101_110.25.3)

pstr.101_110.50.3<-sedimentation.3.3(R.pstr.t = 92005, p.c = 0.50, p.u = 0.50, HR.pstr.3 = 13)
print(pstr.101_110.50.3)
pstr.101_110.50.3<-as.data.frame(pstr.101_110.50.3)

pstr.101_110.75.3<-sedimentation.3.3(R.pstr.t = 75554, p.c = 0.75, p.u = 0.25, HR.pstr.3 = 13)
print(pstr.101_110.75.3)
pstr.101_110.75.3<-as.data.frame(pstr.101_110.75.3)

pstr.101_110.100.3<-sedimentation.3.3(R.pstr.t = 59103, p.c = 1.00, p.u = 0.00, HR.pstr.3 = 13)
print(pstr.101_110.100.3)
pstr.101_110.100.3<-as.data.frame(pstr.101_110.100.3)


# Generate vector of number of days
Days.3.3 <- seq(101, 110, 1)
Days.3.3 <- as.data.frame(Days.3.3)

# Bind population estimates with Days column
pstr.101_110.0.3<-cbind(Days.3.3, pstr.101_110.0.3)
pstr.101_110.25.3<-cbind(Days.3.3, pstr.101_110.25.3)
pstr.101_110.50.3<-cbind(Days.3.3, pstr.101_110.50.3)
pstr.101_110.75.3<-cbind(Days.3.3, pstr.101_110.75.3)
pstr.101_110.100.3<-cbind(Days.3.3, pstr.101_110.100.3)

#### 0

# Add new column
pstr.101_110.0.3 <- pstr.101_110.0.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.0.3
  )

# Replace NAs with scenario name
pstr.101_110.0.3$Scenario <- as.character(pstr.101_110.0.3$Scenario)
pstr.101_110.0.3 <-pstr.101_110.0.3 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.101_110.0.3 <- pstr.101_110.0.3 %>% 
  rename(
    Days = Days.3.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(Sed.event = newcol)


#### 25

# Add new column
pstr.101_110.25.3 <- pstr.101_110.25.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.25.3
  )

# Replace NAs with scenario name
pstr.101_110.25.3$Scenario <- as.character(pstr.101_110.25.3$Scenario)
pstr.101_110.25.3 <-pstr.101_110.25.3 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.101_110.25.3 <- pstr.101_110.25.3 %>% 
  rename(
    Days = Days.3.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


#### 50

# Add new column
pstr.101_110.50.3 <- pstr.101_110.50.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.50.3
  )

# Replace NAs with scenario name
pstr.101_110.50.3$Scenario <- as.character(pstr.101_110.50.3$Scenario)
pstr.101_110.50.3 <-pstr.101_110.50.3 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.101_110.50.3 <- pstr.101_110.50.3 %>% 
  rename(
    Days = Days.3.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


#### 75

# Add new column
pstr.101_110.75.3 <- pstr.101_110.75.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.75.3
  )

# Replace NAs with scenario name
pstr.101_110.75.3$Scenario <- as.character(pstr.101_110.75.3$Scenario)
pstr.101_110.75.3 <-pstr.101_110.75.3 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.101_110.75.3 <- pstr.101_110.75.3 %>% 
  rename(
    Days = Days.3.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


#### 100

# Add new column
pstr.101_110.100.3 <- pstr.101_110.100.3 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.100.3
  )

# Replace NAs with scenario name
pstr.101_110.100.3$Scenario <- as.character(pstr.101_110.100.3$Scenario)
pstr.101_110.100.3 <-pstr.101_110.100.3 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.101_110.100.3 <- pstr.101_110.100.3 %>% 
  rename(
    Days = Days.3.3
  ) %>%
  mutate(newcol = 3) %>% 
  rename(
    Sed.event = newcol
  )


##################### 5 day sedimentation event ######################


sedimentation.3.5 <- function(R.pstr.t, p.c, p.u, HR.pstr.3) {
  
  # Calculate background mortality rate
  Mb.pstr.3 <- (0.14 * R.pstr.t) / 10
  
  # Define Tx
  T5 <- seq(1, 10, 1)  # Extended range to include x > 5
  
  # Initialize a vector to store results
  results <- numeric(length(T5))
  
  # Variable to store R.pstr.t at x = 3
  R_pstr_3 <- NA
  
  # Define the function to correct hazard ratio
  for (i in seq_along(T5)) {
    x <- T5[i]
    
    if (x <= 5) {
      results[i] <- ((p.u) * ((R.pstr.t) - (Mb.pstr.3 * x))) + 
        ((p.c) * ((R.pstr.t) - (HR.pstr.3 * (Mb.pstr.3 * x))))
      
      # Store the result at x = 3
      if (x == 5) {
        R_pstr_3 <- results[i]
      }
      
    } else {
      # Use the stored R_pstr_3 value from x = 3
      results[i] <- ((p.u) * (R_pstr_3 - (Mb.pstr.3 * x))) + 
        ((p.c) * (R_pstr_3 - (Mb.pstr.3 * x)))
    }
  }
  
  # Ensure non-negative values
  results <- pmax(results, 0)
  
  return(results)
}

mortality.by.100

# Run scenarios
pstr.101_110.0.5<-sedimentation.3.5(R.pstr.t = 124905, p.c = 0.00, p.u = 1.00, HR.pstr.3 = 13)
print(pstr.101_110.0.5)
pstr.101_110.0.5<-as.data.frame(pstr.101_110.0.5)

pstr.101_110.25.5<-sedimentation.3.5(R.pstr.t = 97488, p.c = 0.25, p.u = 0.75, HR.pstr.3 = 13)
print(pstr.101_110.25.5)
pstr.101_110.25.5<-as.data.frame(pstr.101_110.25.5)

pstr.101_110.50.5<-sedimentation.3.5(R.pstr.t = 70069, p.c = 0.50, p.u = 0.50, HR.pstr.3 = 13)
print(pstr.101_110.50.5)
pstr.101_110.50.5<-as.data.frame(pstr.101_110.50.5)

pstr.101_110.75.5<-sedimentation.3.5(R.pstr.t = 42651, p.c = 0.75, p.u = 0.25, HR.pstr.3 = 13)
print(pstr.101_110.75.5)
pstr.101_110.75.5<-as.data.frame(pstr.101_110.75.5)

pstr.101_110.100.5<-sedimentation.3.5(R.pstr.t = 15233, p.c = 1.00, p.u = 0.00, HR.pstr.3 = 13)
print(pstr.101_110.100.5)
pstr.101_110.100.5<-as.data.frame(pstr.101_110.100.5)

# Generate vector of number of days
Days.3.5 <- seq(101, 110, 1)
Days.3.5 <- as.data.frame(Days.3.5)

# Bind population estimates with Days column
pstr.101_110.0.5<-cbind(Days.3.5, pstr.101_110.0.5)
pstr.101_110.25.5<-cbind(Days.3.5, pstr.101_110.25.5)
pstr.101_110.50.5<-cbind(Days.3.5, pstr.101_110.50.5)
pstr.101_110.75.5<-cbind(Days.3.5, pstr.101_110.75.5)
pstr.101_110.100.5<-cbind(Days.3.5, pstr.101_110.100.5)

### 0

# Add new column
pstr.101_110.0.5 <- pstr.101_110.0.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.0.5
  )

# Replace NAs with scenario name
pstr.101_110.0.5$Scenario <- as.character(pstr.101_110.0.5$Scenario)
pstr.101_110.0.5 <-pstr.101_110.0.5 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.101_110.0.5 <- pstr.101_110.0.5 %>% 
  rename(
    Days = Days.3.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


### 25

# Add new column
pstr.101_110.25.5 <- pstr.101_110.25.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.25.5
  )

# Replace NAs with scenario name
pstr.101_110.25.5$Scenario <- as.character(pstr.101_110.25.5$Scenario)
pstr.101_110.25.5 <-pstr.101_110.25.5 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.101_110.25.5 <- pstr.101_110.25.5 %>% 
  rename(
    Days = Days.3.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


### 50

# Add new column
pstr.101_110.50.5 <- pstr.101_110.50.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.50.5
  )

# Replace NAs with scenario name
pstr.101_110.50.5$Scenario <- as.character(pstr.101_110.50.5$Scenario)
pstr.101_110.50.5 <-pstr.101_110.50.5 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.101_110.50.5 <- pstr.101_110.50.5 %>% 
  rename(
    Days = Days.3.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


### 75

# Add new column
pstr.101_110.75.5 <- pstr.101_110.75.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.75.5
  )

# Replace NAs with scenario name
pstr.101_110.75.5$Scenario <- as.character(pstr.101_110.75.5$Scenario)
pstr.101_110.75.5 <-pstr.101_110.75.5 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.101_110.75.5 <- pstr.101_110.75.5 %>% 
  rename(
    Days = Days.3.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


### 100

# Add new column
pstr.101_110.100.5 <- pstr.101_110.100.5 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.100.5
  )

# Replace NAs with scenario name
pstr.101_110.100.5$Scenario <- as.character(pstr.101_110.100.5$Scenario)
pstr.101_110.100.5 <-pstr.101_110.100.5 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.101_110.100.5 <- pstr.101_110.100.5 %>% 
  rename(
    Days = Days.3.5
  ) %>%
  mutate(newcol = 5) %>% 
  rename(
    Sed.event = newcol
  )


##################### 7 day sedimentation event ######################
sedimentation.3.7 <- function(R.pstr.t, p.c, p.u, HR.pstr.3) {
  
  # Calculate background mortality rate
  Mb.pstr.3 <- (0.14 * R.pstr.t) / 10
  
  # Define Tx
  T7 <- seq(1, 10, 1)  # Extended range to include x > 7
  
  # Initialize a vector to store results
  results <- numeric(length(T7))
  
  # Variable to store R.pstr.t at x = 3
  R_pstr_3 <- NA
  
  # Define the function to correct hazard ratio
  for (i in seq_along(T7)) {
    x <- T7[i]
    
    if (x <= 7) {
      results[i] <- ((p.u) * ((R.pstr.t) - (Mb.pstr.3 * x))) + 
        ((p.c) * ((R.pstr.t) - (HR.pstr.3 * (Mb.pstr.3 * x))))
      
      # Store the result at x = 3
      if (x == 7) {
        R_pstr_7 <- results[i]
      }
      
    } else {
      # Use the stored R_pstr_7 value from x = 7
      results[i] <- ((p.u) * (R_pstr_7 - (Mb.pstr.3 * x))) + 
        ((p.c) * (R_pstr_7 - (Mb.pstr.3 * x)))
    }
  }
  
  # Ensure non-negative values
  results <- pmax(results, 0)
  
  return(results)
}

mortality.by.100


# Run scenarios
pstr.101_110.0.7<-sedimentation.3.7(R.pstr.t = 124905, p.c = 0.00, p.u = 1.00, HR.pstr.3 = 13)
print(pstr.101_110.0.7)
pstr.101_110.0.7<-as.data.frame(pstr.101_110.0.7)

pstr.101_110.25.7<-sedimentation.3.7(R.pstr.t = 86520, p.c = 0.25, p.u = 0.75, HR.pstr.3 = 13)
print(pstr.101_110.25.7)
pstr.101_110.25.7<-as.data.frame(pstr.101_110.25.7)

pstr.101_110.50.7<-sedimentation.3.7(R.pstr.t = 48134, p.c = 0.50, p.u = 0.50, HR.pstr.3 = 13)
print(pstr.101_110.50.7)
pstr.101_110.50.7<-as.data.frame(pstr.101_110.50.7)

pstr.101_110.75.7<-sedimentation.3.7(R.pstr.t = 9749, p.c = 0.75, p.u = 0.25, HR.pstr.3 = 13)
print(pstr.101_110.75.7)
pstr.101_110.75.7<-as.data.frame(pstr.101_110.75.7)

pstr.101_110.100.7<-sedimentation.3.7(R.pstr.t = 0, p.c = 1.00, p.u = 0.00, HR.pstr.3 = 13)
print(pstr.101_110.100.7)
pstr.101_110.100.7<-as.data.frame(pstr.101_110.100.7)

# Generate vector of number of days
Days.3.7 <- seq(101, 110, 1)
Days.3.7 <- as.data.frame(Days.3.7)

# Bind population estimates with Days column
pstr.101_110.0.7<-cbind(Days.3.7, pstr.101_110.0.7)
pstr.101_110.25.7<-cbind(Days.3.7, pstr.101_110.25.7)
pstr.101_110.50.7<-cbind(Days.3.7, pstr.101_110.50.7)
pstr.101_110.75.7<-cbind(Days.3.7, pstr.101_110.75.7)
pstr.101_110.100.7<-cbind(Days.3.7, pstr.101_110.100.7)




### 0

# Add new column
pstr.101_110.0.7 <- pstr.101_110.0.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.0.7
  )

# Replace NAs with scenario name
pstr.101_110.0.7$Scenario <- as.character(pstr.101_110.0.7$Scenario)
pstr.101_110.0.7 <-pstr.101_110.0.7 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.101_110.0.7 <- pstr.101_110.0.7 %>% 
  rename(
    Days = Days.3.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


### 25

# Add new column
pstr.101_110.25.7 <- pstr.101_110.25.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.25.7
  )

# Replace NAs with scenario name
pstr.101_110.25.7$Scenario <- as.character(pstr.101_110.25.7$Scenario)
pstr.101_110.25.7 <-pstr.101_110.25.7 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.101_110.25.7 <- pstr.101_110.25.7 %>% 
  rename(
    Days = Days.3.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


### 50

# Add new column
pstr.101_110.50.7 <- pstr.101_110.50.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.50.7
  )

# Replace NAs with scenario name
pstr.101_110.50.7$Scenario <- as.character(pstr.101_110.50.7$Scenario)
pstr.101_110.50.7 <-pstr.101_110.50.7 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.101_110.50.7 <- pstr.101_110.50.7 %>% 
  rename(
    Days = Days.3.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


### 75

# Add new column
pstr.101_110.75.7 <- pstr.101_110.75.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.75.7
  )

# Replace NAs with scenario name
pstr.101_110.75.7$Scenario <- as.character(pstr.101_110.75.7$Scenario)
pstr.101_110.75.7 <-pstr.101_110.75.7 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.101_110.75.7 <- pstr.101_110.75.7 %>% 
  rename(
    Days = Days.3.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )


### 100

# Add new column
pstr.101_110.100.7 <- pstr.101_110.100.7 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.100.7
  )

# Replace NAs with scenario name
pstr.101_110.100.7$Scenario <- as.character(pstr.101_110.100.7$Scenario)
pstr.101_110.100.7 <-pstr.101_110.100.7 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.101_110.100.7 <- pstr.101_110.100.7 %>% 
  rename(
    Days = Days.3.7
  ) %>%
  mutate(newcol = 7) %>% 
  rename(
    Sed.event = newcol
  )



##################### 10 day sedimentation event ######################
sedimentation.3.10 <- function(R.pstr.t, p.c, p.u, HR.pstr.3) {
  
  # Calculate background mortality rate
  Mb.pstr.3 <- (0.14*R.pstr.t)/(10)
  
  # Define Tx
  T10 <- seq(1, 10, 1)
  
  # Define the function to correct hazard ratio
  pstr.hr.3 <- function(x) {
    ((p.u)*((R.pstr.t)-(Mb.pstr.3*(x))))+((p.c)*((R.pstr.t)-(HR.pstr.3*(Mb.pstr.3*(x)))))
  }
  
  # Apply the function to Tx
  T10.3.test.c <- pstr.hr.3(T10)
  
  # Ensure non-negative values
  T10.3.test.c <- pmax(T10.3.test.c, 0)
  
  return(T10.3.test.c)
  
}

mortality.by.100

# Run scenarios
pstr.101_110.0.10<-sedimentation.3.10(R.pstr.t = 124905, p.c = 0.00, p.u = 1.00, HR.pstr.3 = 13)
print(pstr.101_110.0.10)
pstr.101_110.0.10<-as.data.frame(pstr.101_110.0.10)

pstr.101_110.25.10<-sedimentation.3.10(R.pstr.t = 70069, p.c = 0.25, p.u = 0.75, HR.pstr.3 = 13)
print(pstr.101_110.25.10)
pstr.101_110.25.10<-as.data.frame(pstr.101_110.25.10)

pstr.101_110.50.10<-sedimentation.3.10(R.pstr.t = 15232, p.c = 0.50, p.u = 0.50, HR.pstr.3 = 13)
print(pstr.101_110.50.10)
pstr.101_110.50.10<-as.data.frame(pstr.101_110.50.10)

pstr.101_110.75.10<-sedimentation.3.10(R.pstr.t = 0, p.c = 0.75, p.u = 0.25, HR.pstr.3 = 13)
print(pstr.101_110.75.10)
pstr.101_110.75.10<-as.data.frame(pstr.101_110.75.10)

pstr.101_110.100.10<-sedimentation.3.10(R.pstr.t = 0, p.c = 1.00, p.u = 0.00, HR.pstr.3 = 13)
print(pstr.101_110.100.10)
pstr.101_110.100.10<-as.data.frame(pstr.101_110.100.10)



# Generate vector of number of days
Days.3.10 <- seq(101, 110, 1)
Days.3.10 <- as.data.frame(Days.3.10)

# Bind population estimates with Days column
pstr.101_110.0.10<-cbind(Days.3.10, pstr.101_110.0.10)
pstr.101_110.25.10<-cbind(Days.3.10, pstr.101_110.25.10)
pstr.101_110.50.10<-cbind(Days.3.10, pstr.101_110.50.10)
pstr.101_110.75.10<-cbind(Days.3.10, pstr.101_110.75.10)
pstr.101_110.100.10<-cbind(Days.3.10, pstr.101_110.100.10)




### 0

# Add new column
pstr.101_110.0.10 <- pstr.101_110.0.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.0.10
  )

# Replace NAs with scenario name
pstr.101_110.0.10$Scenario <- as.character(pstr.101_110.0.10$Scenario)
pstr.101_110.0.10 <-pstr.101_110.0.10 %>% replace_na(list(Scenario = "pstr.0"))

# Fix Days columns
pstr.101_110.0.10 <- pstr.101_110.0.10 %>% 
  rename(
    Days = Days.3.10
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


### 25

# Add new column
pstr.101_110.25.10 <- pstr.101_110.25.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.25.10
  )

# Replace NAs with scenario name
pstr.101_110.25.10$Scenario <- as.character(pstr.101_110.25.10$Scenario)
pstr.101_110.25.10 <-pstr.101_110.25.10 %>% replace_na(list(Scenario = "pstr.25"))

# Fix Days columns
pstr.101_110.25.10 <- pstr.101_110.25.10 %>% 
  rename(
    Days = Days.3.10
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


### 50

# Add new column
pstr.101_110.50.10 <- pstr.101_110.50.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.50.10
  )

# Replace NAs with scenario name
pstr.101_110.50.10$Scenario <- as.character(pstr.101_110.50.10$Scenario)
pstr.101_110.50.10 <-pstr.101_110.50.10 %>% replace_na(list(Scenario = "pstr.50"))

# Fix Days columns
pstr.101_110.50.10 <- pstr.101_110.50.10 %>% 
  rename(
    Days = Days.3.10
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


### 75

# Add new column
pstr.101_110.75.10 <- pstr.101_110.75.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.75.10
  )

# Replace NAs with scenario name
pstr.101_110.75.10$Scenario <- as.character(pstr.101_110.75.10$Scenario)
pstr.101_110.75.10 <-pstr.101_110.75.10 %>% replace_na(list(Scenario = "pstr.75"))

# Fix Days columns
pstr.101_110.75.10 <- pstr.101_110.75.10 %>% 
  rename(
    Days = Days.3.10
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )


### 100

# Add new column
pstr.101_110.100.10 <- pstr.101_110.100.10 %>%
  mutate(newcol = NA) %>% 
  rename(
    Scenario = newcol
  ) %>% 
  rename(
    Population = pstr.101_110.100.10
  )

# Replace NAs with scenario name
pstr.101_110.100.10$Scenario <- as.character(pstr.101_110.100.10$Scenario)
pstr.101_110.100.10 <-pstr.101_110.100.10 %>% replace_na(list(Scenario = "pstr.100"))

# Fix Days columns
pstr.101_110.100.10 <- pstr.101_110.100.10 %>% 
  rename(
    Days = Days.3.10
  ) %>%
  mutate(newcol = 10) %>% 
  rename(
    Sed.event = newcol
  )

mortality.60_110<-rbind(mortality.60_100, 
                       pstr.101_110.0.3, pstr.101_110.25.3, pstr.101_110.50.3, pstr.101_110.75.3, pstr.101_110.100.3,
                       pstr.101_110.0.5, pstr.101_110.25.5, pstr.101_110.50.5, pstr.101_110.75.5, pstr.101_110.100.5,
                       pstr.101_110.0.7, pstr.101_110.25.7, pstr.101_110.50.7, pstr.101_110.75.7, pstr.101_110.100.7,
                       pstr.101_110.0.10, pstr.101_110.25.10, pstr.101_110.50.10, pstr.101_110.75.10, pstr.101_110.100.10)



write.csv(mortality.60_110, "mortality.60_110.Rt.csv", row.names=FALSE)

# mortality.0_110.V2<-read.csv( "mortality.0_110_V2.csv")

mortality.0_110.V2<-read.csv( "mortality.0_110.Rt_V2.2.csv")


# mortality.0_110.V2_FIXED<-rbind(mortality.0_110.V2, pstr.101_110.75.3,
#                                 pstr.101_110.75.5, pstr.101_110.75.7, pstr.101_110.75.10)

se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}

mortality.0_110.V2_summary_sed.events <- mortality.0_110.V2 %>%
  group_by(Sed.event, Days) %>%
  summarise(CT = mean(Population),
            spread = se(Population))


mortality.0_110.V2_summary_scenario <- mortality.0_110.V2 %>%
  group_by(Scenario, Days) %>%
  summarise(CT = mean(Population),
            spread = se(Population))

# Line plot
# Raw abundances

# Line plot
legend_title<-"Recruits (%) buried by 4 mm of sediment"
legend_title2<-"Days buried under 4 mm of sediment"

no_title<-""

mortality.0_110.V2_summary_scenario$Scenario<-as.factor(mortality.0_110.V2_summary_scenario$Scenario)

ggplot(mortality.0_110.V2_summary_scenario, 
       aes(x = Days,y = CT, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CT - spread, ymax = CT + spread, fill = Scenario), alpha = 0.3, color = NA)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(0,120, by = 10), limits = c(0,120))+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, by = 50000))+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12))+
  scale_color_manual(legend_title, values = c("steelblue", "olivedrab", "goldenrod", 
                                              "coral", "firebrick"))+
  scale_fill_manual(legend_title, values = c("steelblue", "olivedrab", "goldenrod", 
                                             "coral", "firebrick"))

mortality.0_110.V2_summary_sed.events$Sed.event<-as.factor(mortality.0_110.V2_summary_sed.events$Sed.event)

ggplot(mortality.0_110.V2_summary_sed.events, 
       aes(x = Days,y = CT, color = Sed.event, group = Sed.event)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CT - spread, ymax = CT + spread, fill = Sed.event), alpha = 0.3, color = NA)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(0,120, by = 10), limits = c(0,120))+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, by = 50000))+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))+
  scale_color_manual(legend_title2, values = c("steelblue", "olivedrab", "goldenrod", 
                                              "coral", "firebrick"))+
  scale_fill_manual(legend_title2, values = c("steelblue", "olivedrab", "goldenrod", 
                                             "coral", "firebrick"))



########### export final df into csv and add background mortality for events less than 10 in excel ############


mortality.0_110.V3<-read.csv( "mortality.0_110.Rt_V3.csv")

legend_title<-"Recruits (%) buried by 4 mm of sediment"
legend_title2<-"Days buried under 4 mm of sediment"

se<-function(x){sd(x,na.rm=T)/sqrt(sum(!is.na(x)))}

mortality.0_110.V3_summary_sed.events <- mortality.0_110.V3 %>%
  group_by(Sed.event, Days) %>%
  summarise(CT = mean(Population),
            spread = se(Population))


mortality.0_110.V3_summary_scenario <- mortality.0_110.V3 %>%
  group_by(Scenario, Days) %>%
  summarise(CT = mean(Population),
            spread = se(Population))

mortality.110.events <- mortality.0_110.V3_summary_sed.events %>%
  filter(Days==110)
mortality.110.events

mortality.110.scenario <- mortality.0_110.V3_summary_scenario %>%
  filter(Days==110)
mortality.110.scenario


mortality.110.events$Sed.event<-as.factor(mortality.110.events$Sed.event)
mortality.0_110.V3$Sed.event<-as.factor(mortality.0_110.V3$Sed.event)

plot_events_110<-ggplot(mortality.110.events, aes(x=Sed.event, y=CT, fill=Sed.event)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           alpha = 0.5) +
  geom_errorbar(aes(ymin=CT-spread, ymax=CT+spread), width=.15,
                position=position_dodge(.9))+
  geom_point(data = mortality.0_110.V3 %>% filter(Days == 110),
             aes(x = Sed.event, y = Population, color = Sed.event),
             position = position_jitter(width = 0.15),
             alpha = 0.6,
             size = 2,
             inherit.aes = FALSE)+
  scale_y_continuous(limits = c(-1000,120000), breaks = seq(0,120000, by = 20000),
                     labels = scales::scientific)+
  xlab("Days buried under 4 mm of sediment")+
  ylab("Coral recruit abundance (total no. of individuals")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none")+
  scale_fill_manual(legend_title2, values = c("steelblue", "goldenrod", "lightcoral", 
                                             "orangered2", "firebrick"))+
  scale_color_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                                "orangered2", "firebrick"))


mortality.0_110.V3 %>% filter(Days == 110)

plot_events_110_box <- ggplot(
  mortality.0_110.V3 %>% filter(Days == 110),
  aes(x = as.factor(Sed.event), y = Population, fill = as.factor(Sed.event))) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(Sed.event)),
              width = 0.15, alpha = 0.7, size = 2) +
  scale_y_continuous(limits = c(-100,115000), breaks = seq(0,115000, by = 20000))+
  xlab("Days buried under 4 mm of sediment")+
  ylab("Coral recruit abundance (total no. of individuals)")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none")+
  scale_fill_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                               "orangered2", "firebrick")) +
  scale_color_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                                "orangered2", "firebrick"))

mortality.110.scenario$Scenario<-as.factor(mortality.110.scenario$Scenario)
mortality.0_110.V3$Scenario<-as.factor(mortality.0_110.V3$Scenario)

plot_scenario_110<-ggplot(mortality.110.scenario, aes(x=Scenario, y=CT, fill=Scenario)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           alpha =0.5) +
  geom_errorbar(aes(ymin=CT-spread, ymax=CT+spread), width=.15,
                position=position_dodge(.9))+
  geom_point(
    data = mortality.0_110.V3 %>% filter(Days == 110),
    aes(x = Scenario, y = Population, color = Scenario),
    position = position_jitter(width = 0.1),  # small jitter, centered
    alpha = 0.6,
    size = 2,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = c(-1000,120000), breaks = seq(0,120000, by = 20000),
                     labels = scales::scientific)+
  xlab("Recruits (%) buried by 4 mm of sediment")+
  ylab("Coral recruit abundance (total no. of individuals")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none")+
  scale_fill_manual(legend_title2, values = c("steelblue", "goldenrod", "lightcoral", 
                                              "orangered2", "firebrick"))+
  scale_color_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                                "orangered2", "firebrick"))

plot_scenario_110_box <- ggplot(
  mortality.0_110.V3 %>% filter(Days == 110),
  aes(x = as.factor(Scenario), y = Population, fill = as.factor(Scenario))) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = as.factor(Scenario)),
              width = 0.15, alpha = 0.7, size = 2) +
  scale_y_continuous(limits = c(-100,110000), breaks = seq(0,110000, by = 20000))+
  xlab("Recruits (%) buried by 4 mm of sediment")+
  ylab("Coral recruit abundance (total no. of individuals)")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = "none")+
  scale_fill_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                               "orangered2", "firebrick")) +
  scale_color_manual(values = c("steelblue", "goldenrod", "lightcoral", 
                                "orangered2", "firebrick"))

ggsave("plot_events_110.tiff", plot = plot_events_110, width = 5, height = 5, units = "in", dpi = 300, bg = "white")
ggsave("plot_scenario_110.tiff", plot = plot_scenario_110, width = 5, height = 5, units = "in", dpi = 300, bg = "white")

ggsave("plot_events_110_box.tiff", plot = plot_events_110_box, width = 5, height = 5, units = "in", dpi = 300)
ggsave("plot_scenario_110_box.tiff", plot = plot_scenario_110_box, width = 5, height = 5, units = "in", dpi = 300)

# Line plot
# Raw abundances

# Line plot
legend_title<-"Recruits (%) buried by 4 mm of sediment"
legend_title2<-"Days buried under 4 mm of sediment"

no_title<-""

mortality.0_110.V3_summary_scenario$Scenario<-as.factor(mortality.0_110.V3_summary_scenario$Scenario)

scenario_plot<-ggplot(mortality.0_110.V3_summary_scenario, 
       aes(x = Days,y = CT, color = Scenario, group = Scenario)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CT - spread, ymax = CT + spread, fill = Scenario), alpha = 0.3, color = NA)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(0,120, by = 10), limits = c(0,120))+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, by = 100000),
                     labels = scales::scientific)+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12))+
  scale_color_manual(legend_title, values = c("steelblue", "goldenrod", "lightcoral", 
                                              "orangered2", "firebrick"))+
  scale_fill_manual(legend_title, values = c("steelblue", "goldenrod", "lightcoral", 
                                             "orangered2", "firebrick"))+
  annotate('rect', xmin=40, xmax=50, ymin=0, ymax=600000, alpha=.3, fill='grey25')+
  annotate('rect', xmin=100, xmax=110, ymin=0, ymax=600000, alpha=.3, fill='grey25')

mortality.0_110.V3_summary_sed.events$Sed.event<-as.factor(mortality.0_110.V3_summary_sed.events$Sed.event)

sed_e_plot<-ggplot(mortality.0_110.V3_summary_sed.events, 
       aes(x = Days,y = CT, color = Sed.event, group = Sed.event)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CT - spread, ymax = CT + spread, fill = Sed.event), alpha = 0.3, color = NA)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(0,120, by = 10), limits = c(0,120))+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, by = 100000),
                     labels = scales::scientific)+
  labs(x = "Days", y = "Coral recruit abundance")+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top",
        axis.ticks.length = unit(0.3, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))+
  scale_color_manual(legend_title2, values = c("steelblue", "goldenrod", "lightcoral", 
                                               "orangered2", "firebrick"))+
  scale_fill_manual(legend_title2, values = c("steelblue", "goldenrod", "lightcoral", 
                                              "orangered2", "firebrick"))+  
  annotate('rect', xmin=40, xmax=50, ymin=0, ymax=600000, alpha=.3, fill='grey25')+
  annotate('rect', xmin=100, xmax=110, ymin=0, ymax=600000, alpha=.3, fill='grey25')
  
library(ggpubr)

model<-ggarrange(scenario_plot + rremove("ylab") + rremove("xlab"), 
                 sed_e_plot + rremove("ylab") + rremove("xlab"),
                        ncol = 1, nrow = 2,
                 labels = c("A", "B"))

annotate_figure(model, left = text_grob("Coral recruit abundance (total no. of individuals)", 
                                              rot = 90, vjust = 0.75, size = 19),
                bottom = text_grob("Days", size = 19))

ggsave("pstr_model.tiff", units="in", width=7, height=8, dpi=300)
ggsave("pstr_model.jpg", units="in", width=7, height=8, dpi=300)


##### line plots and inset bar plots together

model<-ggarrange(scenario_plot + rremove("ylab") + rremove("xlab"),
                 plot_scenario_110 + rremove("ylab"),
                 sed_e_plot + rremove("ylab"),
                 plot_events_110 + rremove("ylab"),
                 widths = c(0.6, 0.4, 0.6, 0.4),
                 ncol = 2, nrow = 2,
                 labels = c("a", "b", "c", "d"))

annotate_figure(model, left = text_grob("Coral recruit abundance (total no. of individuals)", 
                                       rot = 90, vjust = 0.75, size = 15))

ggsave("pstr_model_V3.tiff", units="in", width=12, height=10, dpi=300, bg = "white")
ggsave("pstr_model_V3.jpg", units="in", width=12, height=10, dpi=300, bg = "white")

