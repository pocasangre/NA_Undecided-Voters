#-------------------------------------------
# Demographic Diffs in Competitive Districts
#-------------------------------------------

library(tidyverse)
library(scales)
library(ggridges)

rm(list=ls())

setwd("/Users/pocasangreo/Google Drive Streaming/My Drive/Swing districts/Swing22/")
df <- read.csv("Data/cd_census_acs.csv")
df <- df %>% mutate(year=2022)
df <- df %>% select(year, STUSAB, cd, type, margin_favor, margin, total_pop, hispanic_pop, white_nothisp, black_nothisp, 
                    mean_income, pop_25plus, bachelors_plus)
df <- df %>% rename(
  state_po = STUSAB, 
  white_nonhisp = white_nothisp, 
  black_nonhisp = black_nothisp)

df20 <- read.csv("Data/cds_20.csv")
df20 <- df20 %>% select(year, cd, type,  margin_favor, margin, state_po, total_pop, hispanic_pop, white_nonhisp, black_nonhisp, 
                        mean_income, pop_25plus, bachelors_plus)

df <- bind_rows(df, df20)


#-----------------------------------------
# Diffs b/w Competitive and others in 2022
#-----------------------------------------

df <- df %>% mutate(
  group_tossup = case_when(
    type=="Toss Up" ~ "Toss Up", 
    type=="Lean Dem" | type=="Likely Dem" | type=="D" ~ "D", 
    type=="Lean Rep" | type=="Likely Rep" | type=="R" ~ "R")
  )


df <- df %>% mutate(
  group_tossup = factor(group_tossup, levels=c("R", "Toss Up", "D")))


df <- df %>% select(year:margin_favor, margin, group_tossup, total_pop:bachelors_plus)



#-----------------------------------------------
# Variables: white, black, hispanic, income, edu
#-----------------------------------------------

df <- df %>% mutate(
  white = white_nonhisp/total_pop, 
  black = black_nonhisp/total_pop, 
  hispanic = hispanic_pop/total_pop, 
  ba_plus = bachelors_plus/pop_25plus, 
  income = rescale(as.numeric(as.character(mean_income)), to=c(0,1)))

df <- df %>% mutate(
  white_rs = scale(white, scale = F) %>% as.vector(), 
  income_rs = scale(income, scale=F) %>% as.vector(), 
  baplus_rs = scale(ba_plus, scale=F) %>% as.vector(), 
  hispanic_rs = scale(hispanic, scale=F) %>% as.vector()
)




#-----------------------------------------------
# Toss ups
# Save data for datawrapper
#  - For Scatterplots
#-----------------------------------------------

write.csv(df, file="Data/Datawrapper/for_scatterplots.csv", row.names = F)


#-----------------------------------------------
# Toss ups
# Save data for datawrapper
#  - For Range Plots
#-----------------------------------------------

df_dw1 <- df %>% group_by(year, group_tossup) %>% 
  summarise(white = mean(white), 
            black = mean(black), 
            hispanic = mean(hispanic), 
            baplus = mean(ba_plus), 
            income = mean(mean_income))

write.csv(df_dw1, file="Data/Datawrapper/for_rangeplots.csv", row.names = F)



#-----------------------------------------------
# Competitive 
# Save data for datawrapper
#  - For Range Plots
#-----------------------------------------------

df_dw2 <- df %>% group_by(year, group_comp) %>% 
  summarise(white = mean(white), 
            black = mean(black), 
            hispanic = mean(hispanic), 
            baplus = mean(ba_plus), 
            income = mean(mean_income))

write.csv(df_dw2, file="Data/Datawrapper/for_rangeplots_comp.csv", row.names = F)


#---------------------
# with Electoral Data
#---------------------

# Winners
df_elec <- df %>% select(year, cd, type, group_tossup, margin_favor, margin, white, mean_income)
df_elec <- df_elec %>% mutate(
  r_margin = ifelse(margin_favor=="D", 0-margin, margin), 
  winner_tossup = ifelse(group_tossup=="Toss Up", margin_favor, ifelse(group_tossup=="R", "Solid Republican", 
                                                                       ifelse(group_tossup=="D", "Solid Democrat", NA)))
)



write.csv(df_elec, file="Data/Datawrapper/df_winners.csv", row.names = F)



#-----------------------------------------------------
# Hispanic Changes - Redistricting or More Competitive?
#-----------------------------------------------------

c_ids20 <- read.csv("Data/consistent_ids20.csv")
c_ids22 <- read.csv("Data/consistent_ids22.csv")


hisp20 <- df %>% filter(year==2020) %>% select(state_po, cd, hispanic) %>% rename(hispanic20 = hispanic)
hisp22 <- df %>% filter(year==2022) %>% select(state_po, cd, hispanic) %>% rename(hispanic22 = hispanic)
  
# Election margins
cd20 <- read.csv("Data/cds_20.csv")
cd20 <- cd20 %>% select(year, cd, type, margin_favor, margin)
hisp20 <- merge(hisp20, cd20, by=c("cd"))


cd22 <- read.csv("Data/cds_22.csv")
cd22 <- cd22 %>% select(cd, type, margin_favor, margin)
hisp22 <- merge(hisp22, cd22, by=c("cd"))

## 
c_ids20 <- c_ids20 %>% rename(cd = Y2020)
c_ids22 <- c_ids22 %>% rename(cd = Y2022)



hisp20 <- merge(c_ids20, hisp20, by=c("cd"))
hisp22 <- merge(c_ids22, hisp22, by=c("cd"))


hisp <- merge(hisp20, hisp22, by=c("state_po", "consistent_ids"))


rd <- read.csv("Data/RD commissions.csv")
hisp <- merge(hisp, rd, by=c("state_po"))

hisp <- hisp %>% mutate(independent = case_when(
  independent==1 & state_po !="CA" ~ "Independent", 
  independent==1 & state_po=="CA" ~ "Independent (CA)", 
  independent==0 ~ "Not Independent"))



hisp <- hisp %>% mutate(
  r_margin_20 = ifelse(margin_favor.x=="D", 0-margin.x, margin.x),
  r_margin_22 = ifelse(margin_favor.y=="D", 0-margin.y, margin.y), 
  r_margin_change = (r_margin_22-r_margin_20)/r_margin_20,
  hisp_change = (hispanic22-hispanic20)/hispanic20
)


hisp <- hisp %>% mutate(
  group_tossup = case_when(
    type.y=="Toss Up" ~ "Toss Up", 
    type.y=="Lean Democrat" | type.y=="Likely Democrat" | type.y=="Solid Democrat" ~ "D", 
    type.y=="Lean Republican" | type.y=="Likely Republican" | type.y=="Solid Republican" ~ "R", 
    type.x=="Toss Up" ~ "Toss Up", 
    type.x=="Lean Democrat" | type.x=="Likely Democrat" | type.x=="Solid Democrat" ~ "D", 
    type.x=="Lean Republican" | type.x=="Likely Republican" | type.x=="Solid Republican" ~ "R")
)


hisp %>% 
  ggplot(aes(x=hisp_change, y = r_margin_change, color = group_tossup )) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlim(-0.5, 0.5) +
  ylim(-10, 20)




write.csv(hisp, file="Data/Datawrapper/hisp_redistricting_margins.csv", row.names = F)


