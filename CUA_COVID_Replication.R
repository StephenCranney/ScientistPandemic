#install.packages('pander')
library(margins)
library(pander)
library(reshape)
#install.packages("xlsx")
library(xlsx)
#install.packages("srvyr")
#install.packages('jtools')
library(jtools)
library(fastDummies)
library(srvyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(weights)
library(anesrake)
library("readxl")
library(MASS)
library(tidyverse)
library(mice)
library(glmnet)
library(dplyr)
library(caret)
library(ggplot2)
library(survey)
library(readxl)
library(stringr)
library(maps)
library(sf)
library(forcats)
require("haven")
library(haven)
library(magrittr)
library(Hmisc)
library(mice)
library(pollster)
library(data.table)
library(stargazer)
library(ggplot2)
library(data.table)
library(mctest)
library(naniar)
getwd()
#install.packages("QuantPsyc")
library(QuantPsyc)
#install.packages("MBESS")
#install.packages("yhat")
library(yhat)
library(effectsize)
#install.packages("dummies")
library(dummies)
library("scales")

sdf <- as.data.frame(read_dta("/Users/stephencranney/Desktop/Regressions/CUA/wwb-analysis-svyset.dta"))

sdf$noaffiliation<-ifelse(sdf$rlgaff==1, 1, 0)
table(sdf$noaffiliation)
table(sdf$noaffiliation, sdf$rlgaff)

table(sdf$imprel, sdf$sprtlty)

table(sdf$imprel)
sdf$IMPREL<- 6-sdf$imprel

sdf$IMPREL<-ifelse(sdf$sprtlty==4, 1, sdf$IMPREL)

table(sdf$IMPREL, sdf$sprtlty)
table(sdf$IMPREL, sdf$noaffiliation)

table(sdf$sprtlty, sdf$imprel, useNA = "always")

table(sdf$IMPREL, sdf$imprel)
table(sdf$IMPREL, sdf$noaffiliation)

#set it to 2021
sdf$age<-2021-sdf$yob

sdf$SPRTLTY<- 5-sdf$sprtlty

sdf$HF<-sdf$hflifesat_1 + sdf$hfphysical_1 + sdf$hfmental_1 + sdf$hfworthwhile_1 + sdf$hfpromotegood_1 + sdf$hfrelsat_1 + sdf$hfexpenses_1
table(sdf$hflifesat_1) ; table(sdf$hfphysical_1) ; table(sdf$hfmental_1) ; table(sdf$hfworthwhile_1) ; table(sdf$hfpromotegood_1) ; table(sdf$hfrelsat_1) ; table(sdf$hfexpenses_1)

sdf$KESSLER<-sdf$kessler_1 + sdf$kessler_2 + sdf$kessler_3 + sdf$kessler_4 + sdf$kessler_5 + sdf$kessler_6
table(sdf$kessler_1) ; table(sdf$kessler_2) ; table(sdf$kessler_3) ; table(sdf$kessler_4) ; table(sdf$kessler_5) + table(sdf$kessler_6) 

#IMPREL
sdf$IMPREL_VI<-ifelse(sdf$IMPREL==5, 1, 0)
sdf$IMPREL_VIMI<-ifelse((sdf$IMPREL==5) | (sdf$IMPREL==4), 1, 0)

table(sdf$imprel, sdf$IMPREL_VI)
table(sdf$imprel, sdf$IMPREL_VIMI)


sdf$followrelig_spiritual<-ifelse(sdf$sprtlty==1, 1, 0)
sdf$followrelig_notspiritual<-ifelse(sdf$sprtlty==2, 1, 0)
sdf$nofollowrelig_spiritual<-ifelse(sdf$sprtlty==3, 1, 0)
sdf$nofollowrelig_nospiritual<-ifelse(sdf$sprtlty==4, 1, 0)

table(sdf$nofollowrelig_nospiritual, sdf$sprtlty)

table(sdf$rlgaff)
sdf$noaffiliation<-ifelse(sdf$rlgaff==1, 1, 0)
sdf$catholic<-ifelse(sdf$rlgaff==2, 1, 0)
sdf$protestant<-ifelse(sdf$rlgaff==3, 1, 0)
sdf$orthodox<-ifelse(sdf$rlgaff==4, 1, 0)
sdf$jewish<-ifelse(sdf$rlgaff==5, 1, 0)
sdf$muslim<-ifelse(sdf$rlgaff==6, 1, 0)
sdf$hindu<-ifelse(sdf$rlgaff==7, 1, 0)
sdf$buddhist<-ifelse(sdf$rlgaff==8, 1, 0)
sdf$sikh<-ifelse(sdf$rlgaff==9, 1, 0)
sdf$other<-ifelse(sdf$rlgaff==10, 1, 0)

table(sdf$other, sdf$rlgaff)

sdf$rlgaff_f<-factor(sdf$rlgaff)
table(sdf$rlgaff_f)

sdf$rlgaff_f2<-as.factor(ifelse(sdf$rlgaff==1, "Unaffiliated", 
                                ifelse(sdf$rlgaff==2, "Catholic", 
                                       ifelse(sdf$rlgaff==3, "Protestant",
                                              ifelse(sdf$rlgaff==4, "Orthodox", 
                                                     ifelse(sdf$rlgaff==5, "Jewish", 
                                                            ifelse(sdf$rlgaff==6, "Muslim",
                                                                   ifelse(sdf$rlgaff==7, "Hindu",
                                                                          ifelse(sdf$rlgaff==8, "Buddhist",
                                                                                 ifelse(sdf$rlgaff==9, "Sikh",
                                                                                        ifelse(sdf$rlgaff==10, "Other Religion", -1)))))))))))

table(sdf$rlgaff_f2, sdf$rlgaff)

prop.table(table(sdf$rlgaff, useNA = "always"))

table(sdf$rlgaff_f2, sdf$rlgaff)

sdf$country_f<-as.factor(ifelse(sdf$s_country==1, "USA", 
                                ifelse(sdf$s_country==2, "UK", 
                                       ifelse(sdf$s_country==3, "Italy",
                                              ifelse(sdf$s_country==4, "India", 
                                                     ifelse(sdf$s_country==9, "English", 
                                                            ifelse(sdf$s_country==16, "Italian", -1)))))))

table(sdf$country_f, sdf$s_country)

#s_country is different between Italy and India than country. Based on rlgaff, 
#in s_country India is 4 per the table, country looks like it's switched for some reason, so use s_country.  

table(sdf$rlgaff, sdf$s_country)
table(sdf$rlgaff, sdf$country)

sdf$SPRTLTY_F<-ifelse(sdf$sprtlty==1, "Religious and spiritual", 
                      ifelse(sdf$sprtlty==2, "Religious, not spiritual", 
                             ifelse(sdf$sprtlty==3, "Not religious, but spiritual",
                                    ifelse(sdf$sprtlty==4, "Not religious, not spiritual", -1))))
table(sdf$SPRTLTY_F, sdf$sprtlty)

table(sdf$female, sdf$gender)

sdf$female_f<-ifelse(sdf$female==0, "Male", 
                     ifelse(sdf$female==1, "Female", -1))

sdf$discipline_f<-ifelse(sdf$discipline==1, "Physics", 
                         ifelse(sdf$discipline==2, "Biology", 
                                ifelse(sdf$discipline==3, "Other", -1)))

table(sdf$discipline, sdf$discipline_f)

table(sdf$position_f, sdf$profpos)

sdf$position_f<-factor(sdf$position)
sdf$position_f<- ifelse(is.na(sdf$position_f), "Skipped position question", sdf$position_f) 
table(sdf$position_f, sdf$position, useNA = "always")

sdf$covid<-sdf$coveff_11 + sdf$coveff_13

table(sdf$covid)

##################
#################

SDFW<-svydesign(id=~n_Institution,strata=~inst_size_stratum, weights=~pweight, data=sdf, fpc=~pop_institutions)

#Regarding religious affiliation, it would be helpful to have the graphs for the same 6 variables but we can reduce religious affiliation to 
#Catholic, Protestant, Jewish, Muslim, Hindu, Other, Nonreligious.
sdf$rlgaff_f3<-as.factor(ifelse(sdf$rlgaff==1, "Unaffiliated", 
                                ifelse(sdf$rlgaff==2, "Catholic", 
                                       ifelse(sdf$rlgaff==3, "Protestant",
                                              ifelse(sdf$rlgaff==5, "Jewish", 
                                                     ifelse(sdf$rlgaff==6, "Muslim",
                                                            ifelse(sdf$rlgaff==7, "Hindu", 'Other')))))))

table(sdf$rlgaff_f3, sdf$rlgaff)

sdf_ab <- as.data.table(sdf[myvars])

row_names_df_to_remove<-c("pweight")

#####Factor means #ACTUALLY, JUST HARD CODE ALL FACTORS INTO DUMMIES AND INPUT INTO GENERAL, 
myvars_factor <- c('country_f', 'female_f', 'position_f' , 'discipline_f', 'SPRTLTY_F', 'rlgaff_f2') 
sdf_country<-dummy_cols(sdf$country_f)
sdf_position<-dummy_cols(sdf$position_f)
sdf_discipline<-dummy_cols(sdf$discipline_f)
sdf_SPRTLTY<-dummy_cols(sdf$SPRTLTY_F)
sdf_SPRTLTY$.data_NA_SPRTLTY<-sdf_SPRTLTY$.data_NA
sdf_rlgaff<-dummy_cols(sdf$rlgaff_f2)
sdf_rlgaff$.data_NA_rlgaff<-sdf_rlgaff$.data_NA

table(sdf$.data_NA_rlgaff, sdf$rlgaff, useNA="always")

wtd.table(sdf$rlgaff_f2, weights=sdf$pweight)

sdf=cbind(sdf, sdf_country, sdf_position, sdf_discipline, sdf_SPRTLTY, sdf_rlgaff)

#Rename different NAs
##########################
########################

#Hand checked HF, Kessler, IMPREL_VIMI, 'coveff_11', 'coveff_13', female, "countries", sdf_rlgaff$.data_NA_rlgaff, fields, religions, religiosity. 

myvars <- c('HF', 'KESSLER', 
            'IMPREL_VIMI' , 'coveff_11', 'coveff_13', 'female', '.data_India', '.data_Italy', '.data_UK', 
            '.data_USA', '.data_1', '.data_2', '.data_3', '.data_4', '.data_5', '.data_6', '.data_Skipped position question', '.data_NA_rlgaff',
            '.data_Biology', '.data_Other', '.data_Physics', '.data_Buddhist', '.data_Catholic', '.data_Hindu', 
            '.data_Jewish', '.data_Muslim', '.data_Orthodox', '.data_Other Religion', '.data_Protestant',
            '.data_Sikh', '.data_Unaffiliated', '.data_NA_SPRTLTY', '.data_Not religious, but spiritual',
            '.data_Not religious, not spiritual', '.data_Religious and spiritual', 
            '.data_Religious, not spiritual', 'pweight') 

#Two .data_others threw things off, but that's fixed now (just ".data_Other" is the discipline, "other" alone is the religion, '.data_Other Religion')

sdf_ab <- as.data.table(sdf[myvars])

###Numeric means (weighted)
sdf_sum <- sdf_ab[,lapply(.SD,weighted.mean,w=pweight, na.rm = TRUE)]
sdf_sum <- as.data.frame(t(sdf_sum))
colnames(sdf_sum)='mean'

#Numeric means (non-weighted)

sdf_unweightedmeans<-as.data.frame(sapply(sdf_ab, mean, na.rm=TRUE))
colnames(sdf_unweightedmeans)='Unweighted Mean'

sdf_n<-as.data.frame(sapply(sdf_ab, function(x) count(sdf_ab[!is.na(x),])))
sdf_n <- as.data.frame(t(sdf_n))
colnames(sdf_n)='n'

sdf_min<-as.data.frame(apply(sdf_ab, 2, min, na.rm=TRUE))
colnames(sdf_min)='min'

sdf_max<-as.data.frame(apply(sdf_ab, 2, max, na.rm=TRUE))
colnames(sdf_max)='max'

#Weighted SD
sdf_sd<-as.data.frame(sapply(sdf_ab, function(x) sqrt(wtd.var(x, sdf_ab$pweight))))
colnames(sdf_sd)='sd'
#sdf_n<-as.data.frame(sapply(sdf_ab, function(x) sum(sdf_ab[!is.na(x),]$pweight)))
#colnames(sdf_n)='n (from weights)'
Table1=cbind(sdf_sum, sdf_unweightedmeans, sdf_sd, sdf_max, sdf_min, sdf_n)

write.csv(Table1, "/Users/stephencranney/Desktop/Regressions/CUA/SummaryStats_replicate.csv")

#Hand check weighted means, means, totals, etc. 

wtd.mean(sdf_ab$HF, sdf_ab$pweight)
sum(!is.na(sdf_ab$HF))

####

#Hand checked catholic + protestant + orthodox + jewish + muslim + hindu + buddhist + sikh + other
#Hand checked nofollowrelig_spiritual+followrelig_notspiritual + followrelig_spiritual

sdf$STRESSOR<-ifelse(sdf$stressor==2, 0, 1)
table(sdf$stressor, sdf$STRESSOR)

model1<-lm(HF~IMPREL + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)
model2<-lm(KESSLER~IMPREL + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)

model3<-lm(HF~nofollowrelig_spiritual+followrelig_notspiritual + followrelig_spiritual + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)
model4<-lm(KESSLER~nofollowrelig_spiritual+followrelig_notspiritual + followrelig_spiritual + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)

model5<-lm(HF~catholic + protestant + orthodox + jewish + muslim + hindu + buddhist + sikh + other + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)
model6<-lm(KESSLER~catholic + protestant + orthodox + jewish + muslim + hindu + buddhist + sikh + other + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf, weights=pweight)

stargazer(model1, model2, model3, model4, model5, model6, title="Table 2: Religiosity and Well-Being", 
          align=TRUE, out="/Users/stephencranney/Desktop/Regressions/CUA/Table2_replicate.html", star.cutoffs = c(0.05, 0.01, 0.001))

#Hand checked that table 1 and table 2 outputted here matches in the ms. 
#Hand checked profpos/positio_f

#redo regressions without weights to make sure it's the same, mention the use of weights in regression. 
#One more careful read through and check with tables correspondence. 

#Do regressions without weights to see if they're sensitive. 

model1<-lm(HF~IMPREL + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)
model2<-lm(KESSLER~IMPREL + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)

model3<-lm(HF~nofollowrelig_spiritual+followrelig_notspiritual + followrelig_spiritual + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)
model4<-lm(KESSLER~nofollowrelig_spiritual+followrelig_notspiritual + followrelig_spiritual + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)

model5<-lm(HF~catholic + protestant + orthodox + jewish + muslim + hindu + buddhist + sikh + other + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)
model6<-lm(KESSLER~catholic + protestant + orthodox + jewish + muslim + hindu + buddhist + sikh + other + STRESSOR + country_f + female_f + position_f + discipline_f + coveff_11 + coveff_13 + age, data=sdf)

stargazer(model1, model2, model3, model4, model5, model6, title="Table 2: Religiosity and Well-Being (No weights)", 
          align=TRUE, out="/Users/stephencranney/Desktop/Regressions/CUA/Table2_noweights.html", star.cutoffs = c(0.05, 0.01, 0.001))


