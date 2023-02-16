# Decisive Districts
# Reproduces Numbers and Figures in Decisive Districts report


rm(list=ls())

library(tidyverse)
library(haven)
library(scales)


setwd("/Users/pocasangreo/Google Drive Streaming/My Drive/Swing districts/")

df <- read.csv("Data/nationscape_cleaned_1020.csv") %>% select(-X)

#--------------
# Load CD data
#--------------

cds <- read.csv("Data/cook districts.csv")
cds$district_id[cds$district_id=="MTAL"] <- "MT00"
df <- df %>% rename(district_id = congress_district, 
                    state_po = state)

df1 <- merge(cds, df, by=c("district_id"), all.y=TRUE)
df1 <- df1 %>% filter(district_id != "")
df1 <- df1 %>% filter(district_id!="PR00")
df1 <- df1 %>% filter(district_id != "DC00")
df1 <- df1 %>% filter(district_id != "VI00")

# Unique ids
df1 <- df1 %>% mutate(id = 1:length(district_id)) %>% select(id, district_id:christian, interest)

# Create indices
df1 <- df1 %>% mutate(
  social = rowMeans(df1[ ,c("abortion", "immig", "race", "values")], na.rm=T), 
  econ = rowMeans(df1[ , c("hcare", "redist", "reg", "tax", "enviro")], na.rm=T), 
  ideology = rowMeans(df1[ , c("abortion", "immig", "race", "values", "hcare", "redist", "reg", "tax", "enviro")])
)

# Standardize Indices
df1 <- df1 %>% mutate(
  social_rs = scale(social, scale = T) %>% as.vector(), 
  econ_rs = scale(econ, scale=T) %>% as.vector(), 
  ideo_rs = scale(ideology, scale=T) %>% as.vector()
)

# Types of districts
df1 <- df1 %>% mutate(
  dtype = case_when(
    type=="Toss Up"  ~ "Swing", 
    type=="D" |  type=="Likely Dem" | type=="Lean Dem"  ~ "D", 
    type=="R" |  type=="Likely Rep" | type=="Lean Rep" ~ "R")
)




#-----------------------------
# Aggregate at district level
#-----------------------------

df_agg <- df1 %>% select(id, district_id, year, type, state_po, weight, weight_2020, abortion:pknow, interest, catholic:dtype)

df_agg <- df_agg %>% 
  mutate(abortion  = scale(abortion, scale=T), 
         guns   = scale(guns, scale=T),
         hcare  = scale(hcare, scale=T),
         immig  = scale(immig, scale=T),
         race   = scale(race, scale=T),
         redist = scale(redist, scale=T),
         reg    = scale(reg, scale=T),
         tax    = scale(tax, scale=T),
         values = scale(values, scale=T), 
         env = scale(enviro, scale=T))


df_agg <- df_agg %>% group_by(district_id, dtype) %>% 
  summarise(abortion = weighted.mean(abortion, weight, na.rm=T), 
            guns = weighted.mean(guns, weight, na.rm=T),
            hcare = weighted.mean(hcare, weight, na.rm=T),
            immig = weighted.mean(immig, weight, na.rm=T),
            race = weighted.mean(race, weight, na.rm=T),
            redist = weighted.mean(redist, weight, na.rm=T),
            reg = weighted.mean(reg, weight, na.rm=T),
            tax = weighted.mean(tax, weight, na.rm=T),
            values = weighted.mean(values, weight, na.rm=T), 
            env = weighted.mean(env, weight, na.rm=T),
            pknow = weighted.mean(pknow, weight, na.rm=T), 
            interest = weighted.mean(interest, weight, na.rm=T),
            christian = weighted.mean(christian, weight, na.rm=T), 
            social_agg = mean(social, na.rm=T), 
            econ_agg = mean(econ, na.rm=T))
  
# # Mean center
df_agg$social_agg <- scale(df_agg$social_agg, scale = F) %>% as.vector()
df_agg$econ_agg <- scale(df_agg$econ_agg, scale = F) %>% as.vector()



df_agg$dtype <- factor(df_agg$dtype, levels = c("R", "Swing", "D"))

df_agg %>% 
  ggplot(aes(x=econ_agg, y=social_agg, color=dtype)) +
  geom_point() +
  geom_vline(xintercept = median(df_agg$econ_agg[df_agg$dtype=="D"]), linetype='dashed', color="blue", size=1) +
  geom_vline(xintercept = median(df_agg$econ_agg[df_agg$dtype=="R"]), linetype='dashed', color="red", size=1) +
  geom_hline(yintercept = median(df_agg$social_agg[df_agg$dtype=="D"]), linetype='dashed', color="blue", size=1) +
  geom_hline(yintercept = median(df_agg$social_agg[df_agg$dtype=="R"]), linetype='dashed', color="red", size=1)


#-----------------
# For Datawrapper 
#-----------------

pol_agg <- df_agg %>% group_by(dtype) %>% 
  summarise(abortion = mean(abortion), 
            guns = mean(guns), 
            hcare = mean(hcare), 
            immig = mean(immig), 
            race = mean(race), 
            redist = mean(redist), 
            reg = mean(reg), 
            tax = mean(tax), 
            values = mean(values), 
            env = mean(env))

write.csv(pol_agg, file="Swing22/Data/Datawrapper/agg_policies.csv", row.names = F)

#---------
# Indices
#---------


indices <- df_agg %>% select(district_id, dtype, social_agg, econ_agg)

write.csv(indices, file="Swing22/Data/Datawrapper/indices_agg.csv", row.names = F)

# With Election Results 

cd20 <- read.csv("Swing22/Data/cds_20.csv")
cd20 <- cd20 %>% rename(district_id = cd) 
cd20 <- cd20 %>% select(year, district_id, type, margin_favor, margin)

ind_elec <- merge(cd20, indices, by=c('district_id'))
ind_elec <- ind_elec %>% mutate(
  r_margin = ifelse(margin_favor=="D", 0-margin, margin)
)

write.csv(ind_elec, file="Swing22/Data/Datawrapper/indices_margins.csv", row.names = F)

#-----------------------
# District Heterogeneity
#-----------------------

het <- df1 %>% select(id, district_id, year, type, state_po, weight, weight_2020, abortion:pknow, interest, catholic:dtype, 
                      ideology, ideo_rs)

het2 <- het %>% group_by(district_id, dtype) %>% 
  summarise(
  sd_abortion = sd(abortion, na.rm = T), 
  sd_immig = sd(immig, na.rm=T), 
  sd_guns = sd(guns, na.rm = T),
  sd_race = sd(race, na.rm=T), 
  sd_values = sd(values, na.rm=T),
  sd_enviro = sd(enviro, na.rm=T), 
  sd_hcare = sd(hcare, na.rm=T), 
  sd_redist = sd(redist, na.rm=T), 
  sd_reg = sd(reg, na.rm=T), 
  sd_tax = sd(tax, na.rm=T),
  sd_social = sd(social_rs, na.rm=T), 
  sd_econ = sd(econ_rs, na.rm=T), 
  sd_ideo = sd(ideo_rs, na.rm=T),
  mean_social = weighted.mean(social_rs, na.rm=T, w = weight), 
  mean_econ = weighted.mean(econ_rs, na.rm=T, w=weight), 
  mean_ideo = weighted.mean(ideo_rs, na.rm=T, w=weight), 
  median_ideo = median(ideo_rs, na.rm = T)
)

het2$dtype <- as.factor(het2$dtype)

# margins 
cd20 <- read.csv("Swing22/Data/cds_20.csv")
cd20 <- cd20 %>% rename(district_id = cd) 
cd20 <- cd20 %>% select(year, district_id, type, margin_favor, margin)

het2 <- merge(cd20, het2, by=c('district_id'))
het2 <- het2 %>% mutate(
  r_margin = ifelse(margin_favor=="D", 0-margin, margin)
)

het2 %>% 
  ggplot(aes(x=margin, y=sd_ideo, color=dtype)) +
  geom_point()





write.csv(het2, file="Swing22/Data/Datawrapper/het.csv", row.names = F)

