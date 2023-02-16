# Decisive Districts
# Reproduces Numbers and Figures in Decisive Districts report

# Weighted only undecisive voters part


rm(list=ls())

library(tidyverse)
library(haven)
library(scales)
library(stringr)
library(vtable)
library(ggpubr)
library(GGally)
library(ggrepel)
library(ggalluvial)
library(weights)
library(Hmisc)
library(png)
library(pollster)
library(srvyr)
library(modelsummary)


setwd("/Users/pocasangreo/Google Drive Streaming/My Drive/Swing districts/")

df <- read.csv("Data/nationscape_cleaned_1020.csv") %>% select(-X)


# Load CD data
# District data
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
df1 <- df1 %>% mutate(id = 1:length(district_id)) %>% select(id, district_id:voted16)

# Create indices
df1 <- df1 %>% mutate(
  social = rowMeans(df1[ ,c("abortion", "immig", "race", "values")], na.rm=T), 
  econ = rowMeans(df1[ , c("hcare", "redist", "reg", "tax")], na.rm=T)
)

# Standardize Indices
df1 <- df1 %>% mutate(
  social_rs = scale(social, scale = T) %>% as.vector(), 
  econ_rs = scale(econ, scale=T) %>% as.vector()
)

df1 <- df1 %>% mutate(
  dtype = case_when(
    type=="Toss Up" | type=="Lean Dem" | type=="Lean Rep"  ~ "Swing", 
    type=="D" |  type=="Likely Dem" ~ "D", 
    type=="R" |  type=="Likely Rep" ~ "R")
)





### DEMOGRAPHICS 
###
### How Swing voters are different
###

und <- df1 %>% select(id, weight, housevote, dtype, pknow, interest, unemployed, collegeplus,
                      lowincome, highincome_100, highincome_125, highincome_200, black, 
                      female, age, white, hispanic) %>% filter(!is.na(housevote))

und <- und %>% group_by(housevote) %>% summarise(
  pknow =  weighted.mean(pknow, weight, na.rm=T), 
  interest = weighted.mean(interest, weight, na.rm=T), 
  unemployed = weighted.mean(unemployed, weight, na.rm=T), 
  collegeplus = weighted.mean(collegeplus, weight, na.rm=T), 
  lowincome = weighted.mean(lowincome, weight, na.rm=T), 
  highincome_100 = weighted.mean(highincome_100, weight, na.rm=T),
  highincome_125 = weighted.mean(highincome_125, weight, na.rm=T), 
  highincome_200 = weighted.mean(highincome_200, weight, na.rm=T),
  black = weighted.mean(black, weight, na.rm=T), 
  white = weighted.mean(white, weight, na.rm=T), 
  hispanic = weighted.mean(hispanic, weight, na.rm=T), 
  age = weighted.mean(age, weight, na.rm=T), 
  female = weighted.mean(female, weight, na.rm=T)
)



#write.csv(und, file="Final/House Undecided/undecided_dem_diffs.csv", row.names = F)


## In Swing districts
und <- df1 %>% select(weight, housevote, dtype, pknow, interest, unemployed, collegeplus,
                      lowincome, highincome_100, highincome_125, highincome_200, black, 
                      female, age, white, hispanic) %>% 
  filter(!is.na(housevote)) %>% 
  filter(dtype=="Swing")

und <- und %>% group_by(housevote) %>% summarise(
  pknow =  weighted.mean(pknow, weight, na.rm=T), 
  interest = weighted.mean(interest, weight, na.rm=T), 
  unemployed = weighted.mean(unemployed, weight, na.rm=T), 
  collegeplus = weighted.mean(collegeplus, weight, na.rm=T), 
  lowincome = weighted.mean(lowincome, weight, na.rm=T),
  highincome_100 = weighted.mean(highincome_100, weight, na.rm=T),
  highincome_125 = weighted.mean(highincome_125, weight, na.rm=T), 
  highincome_200 = weighted.mean(highincome_200, weight, na.rm=T),
  black = weighted.mean(black, weight, na.rm=T), 
  white = weighted.mean(white, weight, na.rm=T), 
  hispanic = weighted.mean(hispanic, weight, na.rm=T), 
  age = weighted.mean(age, weight, na.rm=T), 
  female = weighted.mean(female, weight, na.rm=T)
)


#write.csv(und, file="Final/House Undecided/undecided_dem_diffs_inswing.csv", row.names = F)

##
###Policy positions
##

df_agg_pol <- df1 %>%
  mutate(abortion  = scale(abortion, scale=T), 
         guns   = scale(guns, scale=T),
         hcare  = scale(hcare, scale=T),
         immig  = scale(immig, scale=T),
         race   = scale(race, scale=T),
         redist = scale(redist, scale=T),
         reg    = scale(reg, scale=T),
         tax    = scale(tax, scale=T),
         values = scale(values, scale=T))

df_agg_pol <- df_agg_pol %>% 
  group_by(housevote) %>% 
  filter(!is.na(housevote)) %>% 
  summarise(abortion = weighted.mean(abortion, weight, na.rm=T), 
            guns = weighted.mean(guns, weight, na.rm=T),
            hcare = weighted.mean(hcare, weight, na.rm=T),
            immig = weighted.mean(immig, weight, na.rm=T),
            race = weighted.mean(race, weight, na.rm=T),
            redist = weighted.mean(redist, weight, na.rm=T),
            reg = weighted.mean(reg, weight, na.rm=T),
            tax = weighted.mean(tax, weight, na.rm=T),
            values = weighted.mean(values, weight, na.rm=T))

#write.csv(df_agg_pol, file="Final/Datawrapper/agg_policy_positions_rs.csv", row.names = F)

# IN swing districts
df_agg_pol <- df1 %>%
  mutate(abortion  = scale(abortion, scale=T), 
         guns   = scale(guns, scale=T),
         hcare  = scale(hcare, scale=T),
         immig  = scale(immig, scale=T),
         race   = scale(race, scale=T),
         redist = scale(redist, scale=T),
         reg    = scale(reg, scale=T),
         tax    = scale(tax, scale=T),
         values = scale(values, scale=T))

df_agg_pol <- df_agg_pol %>% 
  filter(dtype=="Swing") %>% 
  filter(!is.na(housevote)) %>% 
  group_by(housevote) %>% 
  summarise(abortion = weighted.mean(abortion, weight, na.rm=T), 
            guns = weighted.mean(guns, weight, na.rm=T),
            hcare = weighted.mean(hcare, weight, na.rm=T),
            immig = weighted.mean(immig, weight, na.rm=T),
            race = weighted.mean(race, weight, na.rm=T),
            redist = weighted.mean(redist, weight, na.rm=T),
            reg = weighted.mean(reg, weight, na.rm=T),
            tax = weighted.mean(tax, weight, na.rm=T),
            values = weighted.mean(values, weight, na.rm=T))

#write.csv(df_agg_pol, file="Final/Datawrapper/swing_agg_polpositions.csv", row.names = F)




## HEAT MAP FOR SOCIAL / ECON POLICY POSITIONS
df_es <- df1 %>% select(id, weight, dtype, housevote, social_rs, econ_rs) %>% 
  filter(!is.na(housevote)) %>% 
  filter(!is.na(social_rs)) %>% 
  filter(!is.na(econ_rs))

# Percentages fod undecided
df_und <- df_es %>% filter(housevote=="Undecided") %>% mutate(count=1)

df_dem <- df_es %>% filter(housevote=="D") %>% mutate(count=1)
sum(df_dem$count[df_dem$social_rs > 0 ])

rm(df_und)

####


df_es <- df_es %>%
  as_survey_design(weights = weight)

df_es1 <- df_es %>% 
  mutate(social_bins = cut(social_rs, breaks = 30), 
         econ_bins = cut(econ_rs, breaks = 30)) %>% 
  mutate(count = 1) %>% 
  group_by(social_bins, econ_bins) %>% 
  summarise(sum = sum(count))

mean_social <- mean(as.numeric(df_es1$social_bins))
mean_econ <- mean(as.numeric(df_es1$econ_bins))

all <- 
  df_es1  %>% 
  ggplot(aes(x=as.numeric(econ_bins), y = as.numeric(social_bins), fill=sum)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="black") +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed() +
  geom_vline(xintercept = mean_social, linetype="dashed") +
  geom_hline(yintercept = mean_econ, linetype="dashed") +
  ylim(0, 30) +
  xlim(0, 30)


# Democrats
df_es1 <- df_es %>% 
  filter(housevote=="D") %>% 
  mutate(social_bins = cut(social_rs, breaks = 30), 
         econ_bins = cut(econ_rs, breaks = 30)) %>% 
  mutate(count = 1) %>% 
  group_by(social_bins, econ_bins) %>% 
  summarise(sum = sum(count))


#write.csv(df_es1, file="Final/House Undecided/test_heatmap_dems.csv")


dems <- 
  df_es1  %>% 
  ggplot(aes(x=as.numeric(econ_bins), y = as.numeric(social_bins), fill=sum)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#0e6db6") +
  theme(
    panel.background = element_rect(fill = 'white'), 
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank()) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed() +
  geom_vline(xintercept = mean_social, linetype="dashed") +
  geom_hline(yintercept = mean_econ, linetype="dashed") +
  ylim(0, 30) +
  xlim(0, 30)


# Republicans
df_es1 <- df_es %>% 
  filter(housevote=="R") %>% 
  mutate(social_bins = cut(social_rs, breaks = 30), 
         econ_bins = cut(econ_rs, breaks = 30)) %>% 
  mutate(count = 1) %>% 
  group_by(social_bins, econ_bins) %>% 
  summarise(sum = sum(count))

#write.csv(df_es1, file="Final/House Undecided/test_heatmap_reps.csv")

reps <- 
  df_es1  %>% 
  ggplot(aes(x=as.numeric(econ_bins), y = as.numeric(social_bins), fill=sum)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#d83136") +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed() +
  geom_vline(xintercept = mean_social, linetype="dashed") +
  geom_hline(yintercept = mean_econ, linetype="dashed") +
  ylim(0, 30) +
  xlim(0, 30)

# Undecided
df_es1 <- df_es %>% 
  filter(housevote=="Undecided") %>% 
  mutate(social_bins = cut(social_rs, breaks = 30), 
         econ_bins = cut(econ_rs, breaks = 30)) %>% 
  mutate(count = 1) %>% 
  group_by(social_bins, econ_bins) %>% 
  summarise(sum = sum(count))

#write.csv(df_es1, file="Final/House Undecided/test_heatmap_und.csv")




und <- 
  df_es1  %>% 
  ggplot(aes(x=as.numeric(econ_bins), y = as.numeric(social_bins), fill=sum)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#743083") +
  theme(panel.background = element_rect(fill = 'white'), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed() +
  geom_vline(xintercept = mean_social, linetype="dashed") +
  geom_hline(yintercept = mean_econ, linetype="dashed") +
  ylim(0, 30) +
  xlim(0, 30)

ggarrange(all, dems, reps, und, 
          labels = c("All", "Democrats", "Republicans", "Undecided"),
          ncol = 2, nrow = 2)

#
##
## Issue agreement by D/R/U
##
#

## SOCIAL ISSUES
df1 <- df1 %>% mutate(
  abortion_rs = scale(abortion, scale = T) %>% as.vector(), 
  immig_rs = scale(immig, scale=T) %>% as.vector(), 
  guns_rs = scale(guns, scale=T) %>% as.vector(), 
  race_rs = scale(race, scale=T) %>% as.vector(), 
  values_rs = scale(values, scale=T) %>% as.vector()
)

abortion.60p <- quantile(df1$abortion_rs, 0.5, na.rm=T)
immig.60p <- quantile(df1$immig_rs, 0.5, na.rm=T)
race.60p <- quantile(df1$race_rs, 0.5, na.rm=T)
guns.60p <- quantile(df1$guns_rs, 0.5, na.rm=T)
values.60p <- quantile(df1$values_rs, 0.5, na.rm=T)

abortion.40p <- quantile(df1$abortion_rs, 0.5, na.rm=T)
immig.40p <- quantile(df1$immig_rs, 0.5, na.rm=T)
race.40p <- quantile(df1$race_rs, 0.5, na.rm=T)
guns.40p <- quantile(df1$guns_rs, 0.5, na.rm=T)
values.40p <- quantile(df1$values_rs, 0.5, na.rm=T)

df1 <- df1 %>% mutate(
  hcare_rs = scale(hcare, scale = T) %>% as.vector(), 
  redist_rs = scale(redist, scale = T) %>% as.vector(), 
  reg_rs = scale(reg, scale = T) %>% as.vector(), 
  tax_rs = scale(tax, scale = T) %>% as.vector(), 
  enviro_rs = scale(enviro, scale = T) %>% as.vector())

tax.60p <- quantile(df1$tax_rs, 0.5, na.rm=T)
rd.60p <- quantile(df1$redist_rs, 0.5, na.rm=T)
reg.60p <- quantile(df1$reg_rs, 0.5, na.rm=T)
hcare.60p <- quantile(df1$hcare_rs, 0.5, na.rm=T)
env.60p <- quantile(df1$enviro_rs, 0.5, na.rm = T)

tax.40p <- quantile(df1$tax_rs, 0.5, na.rm=T)
rd.40p <- quantile(df1$redist_rs, 0.5, na.rm=T)
reg.40p <- quantile(df1$reg_rs, 0.5, na.rm=T)
hcare.40p <- quantile(df1$hcare_rs, 0.5, na.rm=T)
env.40p <- quantile(df1$enviro_rs, 0.5, na.rm = T)


issue_agr <- df1 %>% select(id, dtype, housevote, weight, abortion_rs:values_rs)
issue_agr <- issue_agr %>% 
  mutate(
    abortion_agr = ifelse(abortion_rs < abortion.40p, "D", 
                          ifelse(abortion_rs > abortion.60p, "R", NA)), 
    immig_agr = ifelse(immig_rs < immig.40p, "D", 
                       ifelse(immig_rs > immig.60p, "R", NA)), 
    guns_agr = ifelse(guns_rs < guns.40p, "D", 
                      ifelse(guns_rs > guns.60p, "R", NA)), 
    race_agr = ifelse(race_rs < race.40p, "D", 
                      ifelse(race_rs > race.60p, "R", NA)), 
    values_agr = ifelse(values_rs < values.40p, "D", 
                        ifelse(values_rs > values.60p, "R", NA)))


issue_agr <- issue_agr %>% 
  mutate(
    abortion_agr = ifelse(is.na(abortion_agr) & !is.na(abortion_rs), "C", abortion_agr), 
    immig_agr = ifelse(is.na(immig_agr) & !is.na(immig_rs), "C", immig_agr),
    guns_agr = ifelse(is.na(guns_agr) & !is.na(guns_rs), "C", guns_agr),
    race_agr = ifelse(is.na(race_agr) & !is.na(race_rs), "C", race_agr),
    values_agr = ifelse(is.na(values_agr) & !is.na(values_rs), "C", values_agr)) 


issue_agr <- issue_agr %>% 
  mutate(total = paste(abortion_agr, immig_agr, guns_agr, race_agr, values_agr, sep="/"))
issue_agr <- issue_agr %>% 
  mutate(has_na = ifelse(str_detect(total, "NA"), 1, 0)) %>% 
  filter(has_na == 0)



abortion <- as.data.frame(crosstab(df = issue_agr, 
                                   x = housevote, 
                                   y = abortion_agr, 
                                   weight = weight, 
                                   format = "long")) %>% 
  mutate(var = "abortion") %>% 
  rename(support = abortion_agr)

immigration <- as.data.frame(crosstab(df = issue_agr, 
                                      x = housevote, 
                                      y = immig_agr, 
                                      weight = weight, 
                                      format = "long")) %>% 
  mutate(var= "immigration") %>% 
  rename(support = immig_agr)

guns <- as.data.frame(crosstab(df = issue_agr, 
                               x = housevote, 
                               y = guns_agr, 
                               weight = weight, 
                               format = "long")) %>% 
  mutate(var = "guns") %>% 
  rename(support = guns_agr)

race <- as.data.frame(crosstab(df = issue_agr, 
                               x = housevote, 
                               y = race_agr, 
                               weight = weight, 
                               format = "long")) %>% 
  mutate(var = "race") %>% 
  rename(support = race_agr)

values <- as.data.frame(crosstab(df = issue_agr, 
                                 x = housevote, 
                                 y = values_agr, 
                                 weight = weight, 
                                 format = "long")) %>% 
  mutate(var= "values") %>% 
  rename(support = values_agr)


social_issues <- bind_rows(abortion, immigration, guns, race, values)

#
###
#### Economic Issues
###
#

issue_agr <- df1 %>% select(id, dtype, housevote, weight,  hcare_rs:enviro_rs)
issue_agr <- issue_agr %>% 
  mutate(
    hcare_agr = ifelse(hcare_rs < hcare.40p, "D", 
                       ifelse(hcare_rs > hcare.60p, "R", NA)), 
    redist_agr = ifelse(redist_rs <= rd.40p, "D", 
                        ifelse(redist_rs > rd.60p, "R", NA)), 
    reg_agr = ifelse(reg_rs < reg.40p, "D", 
                     ifelse(reg_rs > reg.60p, "R", NA)), 
    tax_agr = ifelse(tax_rs < tax.40p, "D", 
                     ifelse(tax_rs > tax.60p, "R", NA)), 
    enviro_agr = ifelse(enviro_rs <= env.40p, "D", 
                        ifelse(enviro_rs > env.60p, "R", NA))
  )

issue_agr <- issue_agr %>% 
  mutate(
    hcare_agr = ifelse(is.na(hcare_agr) & !is.na(hcare_rs), "C", hcare_agr), 
    redist_agr = ifelse(is.na(redist_agr) & !is.na(redist_rs), "C", redist_agr),
    reg_agr = ifelse(is.na(reg_agr) & !is.na(reg_rs), "C", reg_agr),
    tax_agr = ifelse(is.na(tax_agr) & !is.na(tax_rs), "C", tax_agr),
    enviro_agr = ifelse(is.na(enviro_agr) & !is.na(enviro_rs), "C", enviro_agr)) 


issue_agr <- issue_agr %>% 
  mutate(total = paste(hcare_agr, redist_agr, reg_agr, tax_agr, enviro_agr, sep="/"))
issue_agr <- issue_agr %>% 
  mutate(has_na = ifelse(str_detect(total, "NA"), 1, 0)) %>% 
  filter(has_na == 0)


hcare <- as.data.frame(crosstab(df = issue_agr, 
                                x = housevote, 
                                y = hcare_agr, 
                                weight = weight, 
                                format = "long")) %>% 
  mutate(var = "hcare") %>% 
  rename(support = hcare_agr)

redist <- as.data.frame(crosstab(df = issue_agr, 
                                 x = housevote, 
                                 y = redist_agr, 
                                 weight = weight, 
                                 format = "long")) %>% 
  mutate(var= "redist") %>% 
  rename(support = redist_agr)

reg <- as.data.frame(crosstab(df = issue_agr, 
                              x = housevote, 
                              y = reg_agr, 
                              weight = weight, 
                              format = "long")) %>% 
  mutate(var = "reg") %>% 
  rename(support = reg_agr)

tax <- as.data.frame(crosstab(df = issue_agr, 
                              x = housevote, 
                              y = tax_agr, 
                              weight = weight, 
                              format = "long")) %>% 
  mutate(var = "tax" )%>% 
  rename(support = tax_agr)



env <- as.data.frame(crosstab(df = issue_agr, 
                              x = housevote, 
                              y = enviro_agr, 
                              weight = weight, 
                              format = "long")) %>% 
  mutate(var= "env")%>% 
  rename(support = enviro_agr)


econ_issues <- bind_rows(hcare, redist, reg, tax, env)

#write.csv(social_issues, file="Final/House Undecided/agree_socialissues.csv", row.names = F)
#write.csv(econ_issues, file="Final/House Undecided/agree_econissues.csv", row.names = F)

























###
### Cross pressures
###


# Cross pressures as cross party agreement score
# Social issues

# Social
xp_scores_social <- df1 %>% select(id, housevote, weight, abortion_rs:values_rs) %>% 
  filter(!is.na(housevote)) %>% 
  pivot_longer(abortion_rs:values_rs, names_to = "var", values_to = "value") %>% 
  mutate(
    dem_agree = case_when(
      var=="abortion_rs" & value < abortion.40p ~ 1, 
      var=="guns_rs"     & value < guns.40p ~ 1, 
      var=="immig_rs"    & value < immig.40p ~ 1, 
      var=="race_rs"     & value < race.40p ~ 1, 
      var=="values_rs"   & value < values.40p ~ 1
    ), 
    rep_agree = case_when(
      var=="abortion_rs" & value > abortion.60p ~ 1, 
      var=="guns_rs"     & value > guns.60p ~ 1, 
      var=="immig_rs"    & value > immig.60p ~ 1, 
      var=="race_rs"     & value > race.60p ~ 1, 
      var=="values_rs"   & value > values.60p ~ 1), 
    center_agree = case_when(
      is.na(dem_agree) & is.na(rep_agree) & var=="abortion_rs"  ~ 1,
      is.na(dem_agree) & is.na(rep_agree) & var=="immig_rs"   ~ 1, 
      is.na(dem_agree) & is.na(rep_agree) & var=="guns_rs" ~ 1,
      is.na(dem_agree) & is.na(rep_agree) & var=="race_rs"  ~ 1,
      is.na(dem_agree) & is.na(rep_agree) & var=="values_rs" ~ 1
    )
  )


xp_scores_social <- xp_scores_social %>% 
  filter(!is.na(value)) %>% 
  group_by(id, weight, housevote) %>% 
  mutate(dem_score = sum(dem_agree, na.rm = T), 
         rep_score = sum(rep_agree, na.rm=T),
         center_score = sum(center_agree, na.rm=T),
         complete = dem_score + rep_score) 




#Plots boxes
#####
#  DEM
dem <- xp_scores_social %>% 
  filter(housevote=="D") 

dem <- dem %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1) 

dem <- as.data.frame(crosstab(df = dem, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

dem <- dem %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)


dem %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(dem, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#0e6db6") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive social policies supported") +
  ylab("Conservative social policies supported") +
  coord_fixed() 



# REP  
rep <- xp_scores_social %>% 
  filter(housevote=="R") 


rep <- rep %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1)

rep <- as.data.frame(crosstab(df = rep, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

rep <- rep %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)

rep %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(rep, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#c01e22") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive social policies supported") +
  ylab("Conservative social policies supported") +
  coord_fixed() 


# Undecided
und <- xp_scores_social %>% 
  filter(housevote=="Undecided") 

und <- und %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1)

length(unique(und$id))

und <- as.data.frame(crosstab(df = und, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

und <- und %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)

und %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(und, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#8d4a9b") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive social policies supported") +
  ylab("Conservative social policies supported") +
  coord_fixed() 

#####




# Econ
xp_scores_econ <- df1 %>% select(id, weight, housevote, hcare_rs:enviro_rs) %>% 
  filter(!is.na(housevote)) %>% 
  pivot_longer(hcare_rs:enviro_rs, names_to = "var", values_to = "value") %>% 
  mutate(
    dem_agree = case_when(
      var=="enviro_rs"    & value < env.40p ~ 1, 
      var=="hcare_rs"     & value < hcare.40p ~ 1, 
      var=="redist_rs"    & value <= rd.40p ~ 1, 
      var=="reg_rs"       & value < reg.40p ~ 1, 
      var=="tax_rs"       & value < tax.40p ~ 1), 
    rep_agree = case_when(
      var=="enviro_rs" & value > env.60p ~ 1, 
      var=="hcare_rs"  & value > hcare.60p ~ 1, 
      var=="redist_rs" & value > rd.60p ~ 1, 
      var=="reg_rs"    & value > reg.60p ~ 1, 
      var=="tax_rs"    & value > tax.60p ~ 1), 
    center_agree = case_when(
      is.na(dem_agree) & var=="enviro_rs" & value > env.40p & value < env.60p ~ 1,
      is.na(dem_agree) & var=="hcare_rs" & value > hcare.40p & value < hcare.60p ~ 1, 
      is.na(dem_agree) & var=="redist_rs" & value > rd.40p & value < rd.60p ~ 1,
      is.na(dem_agree) & var=="reg_rs" & value > reg.40p & value < reg.60p ~ 1,
      is.na(dem_agree) & var=="tax_rs" & value > tax.40p & value < tax.60p ~ 1
    )
  )


xp_scores_econ <- xp_scores_econ %>% 
  filter(!is.na(value)) %>% 
  group_by(id, weight, housevote) %>% 
  mutate(dem_score = sum(dem_agree, na.rm = T), 
         rep_score = sum(rep_agree, na.rm=T), 
         complete = dem_score + rep_score) 




#Plots econ boxes 
#####
#  DEM
dem <- xp_scores_econ %>% 
  filter(housevote=="D") 

dem <- dem %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1)

dem <- as.data.frame(crosstab(df = dem, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

dem <- dem %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)

# d <- c("0")
# r <- c("0")
# prop <- c(0)
# 
# add <- data.frame(d, r, prop)
# dem <- bind_rows(dem, add)


dem %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(dem, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#0e6db6") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive economic policies supported") +
  ylab("Conservative economic policies supported") +
  coord_fixed() 



# REP  
rep <- xp_scores_econ %>% 
  filter(housevote=="R") 

rep <- rep %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1)

rep <- as.data.frame(crosstab(df = rep, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

rep <- rep %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)

rep %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(rep, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#c01e22") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive economic policies supported") +
  ylab("Conservative economic policies supported") +
  coord_fixed() 


# Undecided
und <- xp_scores_econ %>% 
  filter(housevote=="Undecided") 

und <- und %>% ungroup() %>% 
  group_by(id, weight) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(cat = paste(dem_score, rep_score, sep="/"), 
         count = 1)

und <- as.data.frame(crosstab(df = und, 
                              x =  cat, 
                              y =  count, 
                              weight = weight, 
                              format= "long"))

und <- und %>% ungroup() %>% 
  mutate(total = sum(n), 
         prop = round((n/total)*100, 0), 
         d = substr(cat, 1, 1), 
         r = substr(cat, 3, 3)) %>% 
  select(d, r, prop)

und %>% 
  ggplot(aes(x=d, y=r, fill=prop)) +
  geom_tile(color="black") +
  geom_text(data=subset(und, prop>0.01),
            aes(label=round(prop, 1)), size=3.5) +
  scale_fill_gradient(low="white", high="#8d4a9b") +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(legend.position = "none") +
  xlab("Progressive economic policies supported") +
  ylab("Conservative economic policies supported") +
  coord_fixed() 


#####
###XPS
###

xp_s <- xp_scores_social %>% select(id, housevote, weight, var, dem_agree, rep_agree, center_agree)
xp_s <- xp_s %>% 
  mutate(dem_agree = ifelse(dem_agree==1, "D", dem_agree), 
         rep_agree = ifelse(rep_agree==1, "R", rep_agree), 
         center_agree = ifelse(center_agree==1, "C", center_agree), 
         agree = coalesce(dem_agree, rep_agree, center_agree)) %>% 
  select(id, weight, housevote, var, agree) %>% 
  pivot_wider(names_from = var, values_from = agree)


xp_s <- xp_s %>% 
  mutate(xp = paste(abortion_rs, immig_rs, guns_rs, race_rs, values_rs, sep="/"))


xp_s <- xp_s %>% 
  mutate(has_na = str_detect(xp, "NA"))
xp_s <- xp_s %>% filter(has_na==FALSE)


dems <- xp_s %>% 
  filter(housevote=="D")

dems <- as.data.frame(round(wpct(dems$xp, dems$weight)*100, 2)) 

reps <- xp_s %>% 
  filter(housevote=="R")

reps <- as.data.frame(round(wpct(reps$xp, reps$weight)*100, 2))

und <- xp_s %>% 
  filter(housevote=="Undecided")

und <- as.data.frame(round(wpct(und$xp, und$weight)*100, 2))

dems <- dems %>% mutate(party="D") %>% filter(`round(wpct(dems$xp, dems$weight) * 100, 2)` > 2.7)
reps <- reps %>% mutate(party="R") %>% filter(`round(wpct(reps$xp, reps$weight) * 100, 2)` > 3.7)
und <- und %>% mutate(party="Undecided") %>% filter(`round(wpct(und$xp, und$weight) * 100, 2)` > 2.03)

all_s <- bind_rows(dems, reps, und)
write.csv(all_s, file="Final/House Undecided/xps_all_s.csv")

# ECON

xp_e <- xp_scores_econ %>% select(id, housevote, weight, var, dem_agree, rep_agree, center_agree)
xp_e <- xp_e %>% 
  mutate(dem_agree = ifelse(dem_agree==1, "D", dem_agree), 
         rep_agree = ifelse(rep_agree==1, "R", rep_agree), 
         center_agree = ifelse(center_agree==1, "C", center_agree), 
         agree = coalesce(dem_agree, rep_agree, center_agree)) %>% 
  select(id, weight, housevote, var, agree) %>% 
  pivot_wider(names_from = var, values_from = agree)

xp_e <- xp_e %>% 
  mutate(xp = paste(hcare_rs, redist_rs, reg_rs, tax_rs, enviro_rs, sep="/"))

xp_e <- xp_e %>% 
  mutate(has_na = str_detect(xp, "NA"))

xp_e <- xp_e %>% filter(has_na==FALSE)

dems <- xp_e %>% 
  filter(housevote=="D")


dems <- as.data.frame(round(wpct(dems$xp, dems$weight)*100, 2))

reps <- xp_e %>% 
  filter(housevote=="R")


reps <- as.data.frame(round(wpct(reps$xp, reps$weight)*100, 2))

und <- xp_e %>% 
  filter(housevote=="Undecided")


und <- as.data.frame(round(wpct(und$xp, und$weight)*100, 2))



dems <- dems %>% mutate(party="D") %>% filter(`round(wpct(dems$xp, dems$weight) * 100, 2)` > 4.6)
reps <- reps %>% mutate(party="R") %>% filter(`round(wpct(reps$xp, reps$weight) * 100, 2)` > 3.9)
und <- und %>% mutate(party="Undecided") %>% filter(`round(wpct(und$xp, und$weight) * 100, 2)` > 4.7)

all_e <- bind_rows(dems, reps, und)
write.csv(all_e, file="Final/House Undecided/xps_all_e.csv")








#
##
###Agreement with R/D on social/economic issues
##
#

most_rep_s <- max(df1$social_rs, na.rm=T)
most_rep_e <- max(df1$econ_rs, na.rm=T)

df1 <- df1 %>%  mutate(
  diff_maxrep_s =  social_rs - most_rep_s, 
  diff_maxrep_e = econ_rs - most_rep_e 
)

df1_reg <- df1 %>%
  filter(!is.na(diff_maxrep_e)) %>% 
  filter(!is.na(diff_maxrep_s)) %>% 
  filter(!is.na(housevote)) %>% 
  mutate(
    social_bins = cut(diff_maxrep_s, breaks = 10), 
    econ_bins = cut(diff_maxrep_e, breaks = 10)) %>% 
  mutate(count = 1)

df1_reg <- df1_reg %>% mutate(
  category = paste(social_bins, econ_bins, sep="/")
)


xp_scores_econ <- xp_scores_econ %>% group_by(id) %>% summarise(dem_score = sum(dem_score), rep_score = sum(rep_score))
xp_scores_social <- xp_scores_social %>% group_by(id) %>% summarise(dem_score = sum(dem_score), rep_score = sum(rep_score))

xpmerge1 <- xp_scores_econ %>% select(id, dem_score, rep_score) %>% rename(dem_score_e = dem_score, rep_score_e = rep_score)
xpmerge2 <- xp_scores_social %>% select(id,  dem_score, rep_score) %>% rename(dem_score_s = dem_score, rep_score_s = rep_score)
xpmerge <- merge(xpmerge1, xpmerge2, by= c('id'))

df2_reg <- merge(df1_reg, xpmerge, by=c("id"))

df1_reg <- df1_reg %>% 
  mutate(
    white_evangelical = ifelse(white==1 & is_evangelical==1, 1, 0), 
    dvote = ifelse(housevote=="D", 1, 0), 
    rvote = ifelse(housevote=="R", 1, 0), 
    uvote = ifelse(housevote=="Undecided", 1, 0)
    )



dem_reg <- df2_reg %>% filter(housevote=="D")
rep_reg <- df2_reg %>% filter(housevote=="R")
und_reg <- df2_reg %>% filter(housevote=="Undecided")

dem_reg <- dem_reg %>% select(weight, group_favorability_democrats, group_favorability_republicans, 
                              tax_rs, reg_rs, redist_rs,  hcare_rs,  enviro_rs,
                                values_rs, race_rs, guns_rs, immig_rs, abortion_rs, white_evangelical, 
                                catholic, collegeplus, lowincome, age, hispanic,
                                black, white, dem_score_s, rep_score_s)


dem_reg <- dem_reg %>% mutate(
  tax_rs = (tax_rs)*-1, 
  reg_rs = (reg_rs)*-1, 
  redist_rs = (redist_rs)*-1, 
  hcare_rs = (hcare_rs)*-1, 
  enviro_rs = (enviro_rs)*-1, 
  values_rs = (values_rs)*-1, 
  race_rs = (race_rs)*-1, 
  guns_rs = (guns_rs)*-1, 
  immig_rs = (immig_rs)*-1, 
  abortion_rs = (abortion_rs)*-1
)


und_reg_d <- und_reg %>% mutate(
  tax_rs = (tax_rs)*-1, 
  reg_rs = (reg_rs)*-1, 
  redist_rs = (redist_rs)*-1, 
  hcare_rs = (hcare_rs)*-1, 
  enviro_rs = (enviro_rs)*-1, 
  values_rs = (values_rs)*-1, 
  race_rs = (race_rs)*-1, 
  guns_rs = (guns_rs)*-1, 
  immig_rs = (immig_rs)*-1, 
  abortion_rs = (abortion_rs)*-1
)



models <- list(
  
  "Republicans" = lm(group_favorability_republicans ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                       values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                       catholic + collegeplus + lowincome  + hispanic +
                       black  + white,
                     rep_reg,
                     weights = rep_reg$weight),
  "Democrats" = lm(group_favorability_republicans ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                     values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                     catholic + collegeplus + lowincome  + hispanic +
                     black  + white,
                   dem_reg,
                   weights = dem_reg$weight)
)




cm <- c('values_rs' = "Christian/Traditional Values", 
        'race_rs' = "Structural Discrimination", 
        "guns_rs" = "Gun Control", 
        "immig_rs" = "Immigration", 
        "abortion_rs" = "Abortion", 
        "tax_rs" = "Taxation", 
        "reg_rs" = "Role of Govt in Economy", 
        "redist_rs" = "Debt Free College", 
        "hcare_rs" = "Role of Govt in Healthcare", 
        "enviro_rs" = "Environment", 
        'is_evangelical' = "Evangelical", 
        "white_evangelical" = "White Evangelical",
        'catholic' = 'Catholic', 
        'female' = "Female", 
        'collegeplus' = "College Degree or Higher", 
        'lowincome' = "Low Income", 
        'age' = "Age", 
        'hispanic' = 'Hispanic', 
        'black' = 'Black', 
        'white' = 'White')

modelplot(models, 
          coef_map = cm,
          coef_omit = 'Interc', 
          conf_level = .95) +
  geom_vline(xintercept = 0, linetype="dashed") +
  ggtitle("Favorability towards Republican Party") +
  xlim(-0.08, 0.08)


rep_fav_r <- as.data.frame(models$Republicans$coefficients)
rep_fav_d <- as.data.frame(models$Democrats$coefficients)
rep_fav <- bind_cols(rep_fav_r, rep_fav_d)

write.csv(rep_fav, file="Final/House Undecided/coefs_rep.csv", row.names = F)
modelsummary(models)


models <- list(
  
  "Republicans" = lm(group_favorability_democrats ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                       values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                       catholic + collegeplus + lowincome +  hispanic +
                       black  + white,
                     rep_reg,
                     weights = rep_reg$weight),
 
  "Democrats" = lm(group_favorability_democrats ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                     values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                     catholic + collegeplus + lowincome +  hispanic +
                     black  + white,
                   dem_reg,
                   weights = dem_reg$weight)
)




modelplot(models, 
          coef_map = cm,
          coef_omit = 'Interc', 
          conf_level = .95) +
  geom_vline(xintercept = 0, linetype="dashed") +
  ggtitle("Favorability towards Democratic Party") +
  xlim(-0.08, 0.08)


rep_fav_r <- as.data.frame(models$Republicans$coefficients)
rep_fav_d <- as.data.frame(models$Democrats$coefficients)
rep_fav <- bind_cols(rep_fav_r, rep_fav_d)

write.csv(rep_fav, file="Final/House Undecided/coefs_dem.csv", row.names = F)
modelsummary(models)


models <- list(
  
  "Democratic Party Favorability" = lm(group_favorability_democrats ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                       values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                       catholic + collegeplus + lowincome +  hispanic +
                       black  + white,
                     und_reg_d,
                     weights = und_reg_d$weight),
  
  "Republican Party Favorability" = lm(group_favorability_republicans ~ tax_rs +reg_rs  + redist_rs +  hcare_rs +  enviro_rs +
                     values_rs + race_rs + guns_rs + immig_rs + abortion_rs + white_evangelical + 
                     catholic + collegeplus + lowincome +  hispanic +
                     black  + white,
                   und_reg,
                   weights = und_reg$weight)
)


modelplot(models, 
          coef_map = cm,
          coef_omit = 'Interc', 
          conf_level = .95) +
  geom_vline(xintercept = 0, linetype="dashed") +
  ggtitle("Party Favorability among Undecided Voters") +
  xlim(-0.08, 0.08)



rep_fav_r <- as.data.frame(models$`Republican Party Favorability`$coefficients)
rep_fav_d <- as.data.frame(models$`Democratic Party Favorability`$coefficients)
rep_fav <- bind_cols(rep_fav_r, rep_fav_d)

write.csv(rep_fav, file="Final/House Undecided/coefs_und.csv", row.names = F)
modelsummary(models)





# id <- as.data.frame(models$Identity$coefficients)
# social <- as.data.frame(models$`Social Issues`$coefficients)
# econ <- as.data.frame(models$`Economic Issues`$coefficients)
# 
# coefs <- bind_rows(id, social, econ)
# write.csv(coefs, file="Final/Datawrapper/reg_coefs.csv", row.names = F)
# 



#####
#democrats
dem <- xpagr %>% 
  filter(housevote=="D")

dem <- as.data.frame(crosstab(df = dem, 
                              x = category, 
                              y = count, 
                              weight = weight, 
                              format = "long"))

dem <- dem %>% separate(category, c("social","econ"), sep = "/")
dem <- dem %>% mutate(
  social2 = as.factor(social), 
  econ2 = as.factor(econ)
)

dem %>% 
  ggplot(aes(x=as.numeric(fct_rev(econ2)), y = as.numeric(fct_rev(social2)), fill=n)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#0e6db6") +
  theme(panel.background = element_rect(fill = 'white'),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed()

#republicans
rep <- xpagr %>% 
  filter(housevote=="R")

rep <- as.data.frame(crosstab(df = rep, 
                              x = category, 
                              y = count, 
                              weight = weight, 
                              format = "long"))

rep <- rep %>% separate(category, c("social","econ"), sep = "/")
rep <- rep %>% mutate(
  social2 = as.factor(social), 
  econ2 = as.factor(econ)
)


rep %>% 
  ggplot(aes(x=as.numeric(fct_rev(econ2)), y = as.numeric(fct_rev(social2)), fill=n)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#d83136") +
  theme(panel.background = element_rect(fill = 'white'),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed()

#undecideds
und <- xpagr %>% 
  filter(housevote=="Undecided")

und <- as.data.frame(crosstab(df = und, 
                              x = category, 
                              y = count, 
                              weight = weight, 
                              format = "long"))

und <- und %>% separate(category, c("social","econ"), sep = "/")
und <- und %>% mutate(
  social2 = as.factor(social), 
  econ2 = as.factor(econ)
)

und %>% 
  ggplot(aes(x=as.numeric(fct_rev(econ2)), y = as.numeric(fct_rev(social2)), fill=n)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="#743083") +
  theme(panel.background = element_rect(fill = 'white'),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  theme(legend.position = "none") +
  xlab("Economic Issues") +
  ylab("Social Issues") +
  coord_fixed()





