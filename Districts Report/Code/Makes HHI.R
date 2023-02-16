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
                    asian_nothisp, mean_income, pop_25plus, bachelors_plus)
df <- df %>% rename(
  state_po = STUSAB, 
  white_nonhisp = white_nothisp, 
  black_nonhisp = black_nothisp, 
  asian_nonhisp = asian_nothisp)

df <- df %>% mutate(
  margin = ifelse(margin_favor=="D", 0-margin, margin)
)

df20 <- read.csv("Data/cds_20.csv")
df20 <- df20 %>% select(year, cd, type, margin_favor, margin, state_po, total_pop, hispanic_pop, 
                        white_nonhisp, black_nonhisp, asian_nonhisp, 
                        mean_income, pop_25plus, bachelors_plus)

df20$margin <- (df20$margin)*100
df <- bind_rows(df, df20)
rm(df20)

#-----------------------------------------
# Diffs b/w Competitive and others in 2022
#-----------------------------------------


df <- df %>% mutate(
  group_tossup = case_when(
    type=="Toss Up" ~ "Decisive", 
    type=="Lean Dem" |  type=="Likely Dem" | type=="D" ~ "D", 
    type=="Lean Rep" | type=="Likely Rep" | type=="R" | type=="R" ~ "R")
)

df$group_tossup <- factor(df$group_tossup, levels = c("R", "Decisive", "D"))

df <- df %>% select(year:margin, group_tossup, total_pop:bachelors_plus)



#-----------------------------------------------
# Variables: white, black, hispanic, income, edu
#-----------------------------------------------

df <- df %>% mutate(
  other_pop = total_pop - white_nonhisp - black_nonhisp - hispanic_pop - asian_nonhisp 
)

df <- df %>% mutate(
  white = white_nonhisp/total_pop, 
  black = black_nonhisp/total_pop, 
  hispanic = hispanic_pop/total_pop,
  asian = asian_nonhisp/total_pop,
  other = other_pop/total_pop,
  ba_plus = bachelors_plus/pop_25plus, 
  income = rescale(as.numeric(as.character(mean_income)), to=c(0,1)))

df <- df %>% mutate(
  white_rs = scale(white, scale = F) %>% as.vector(),
  hispanic_rs = scale(hispanic, scale=F) %>% as.vector(),
  black_rs = scale(black, scale=F) %>% as.vector(),
  asian_rs = scale(asian, scale=F) %>% as.vector(),
  other_rs = scale(other, scale=F) %>% as.vector(),
  income_rs = scale(income, scale=F) %>% as.vector(), 
  baplus_rs = scale(ba_plus, scale=F) %>% as.vector()
)


# Herfindahl Index for Diversity 

# Steps: 
# (1) Square each share of ethnic group
# (2) Add squared terms
# (3) Inverse so that higher numbers are more diverse by subtracting the no diversity score

df <- df %>% mutate(
  white2 = (white*100)^2, 
  black2 = (black*100)^2, 
  asian2 = (asian*100)^2, 
  hispanic2 = (hispanic*100)^2, 
  other2 = (other*100)^2,
  sumsq = white2 + black2 + asian2 + hispanic2 + other2
)

# Subtract sum squares from 10000 and divide by 750
df <- df %>% mutate(
 hhi = (10000-sumsq)/800
)


# df %>% filter(year==2022) %>% 
#   ggplot(aes(x=margin, y=hhi, color=group_tossup)) +
#   geom_point() +
#   ylim(1, 10) +
#   geom_vline(xintercept = 0, linetype="dashed") +
#   geom_hline(yintercept = mean(df$hhi), linetype="dashed")
# 


df <- df %>% select(-c(white2:sumsq))

#write.csv(df, file = "Data/Datawrapper/hhi.csv", row.names = F)







