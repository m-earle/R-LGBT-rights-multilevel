
rawdata <- read.csv("ILGA 2017 merged final.csv")

rawdata[rawdata==""] <- NA

library(dplyr)

rawdata$protectgayrights <- recode(rawdata$m1a_q02_rights_protections, 'strongly_disagree'= 1, 'somewhat_disagree' = 2,
                                   'neither_agree_disagree' = 3,
                                   'somewhat_agree' = 4, 'strongly_agree' = 5)

rawdata$gayworkrights <- recode(rawdata$m1a_q06_works_protected, 'strongly_disagree'= 1, 'somewhat_disagree' = 2, 
                                'neither_agree_disagree' = 3,
                                'somewhat_agree' = 4, 'strongly_agree' = 5)

#gaylegal is reverse coded
rawdata$gaylegal <- recode(rawdata$m1a_q07_charged_criminals, 'strongly_disagree'= 5, 'somewhat_disagree' = 4, 
                           'neither_agree_disagree' = 3,
                           'somewhat_agree' = 2, 'strongly_agree' = 1)

rawdata$protecttransrights <- recode(rawdata$m1c_q03_equal_rights, 'strongly_disagree'= 1, 'somewhat_disagree' = 2, 
                                     'neither_agree_disagree' = 3,
                                     'somewhat_agree' = 4, 'strongly_agree' = 5)

rawdata$transworkrights <- recode(rawdata$m1c_q07_workers_id_born_another, 'strongly_disagree'= 1, 'somewhat_disagree' = 2, 'neither_agree_disagree' = 3,
                                  'somewhat_agree' = 4, 'strongly_agree' = 5)

rawdata$translegal <- recode(rawdata$m1c_q08_adults_granted_recog, 'strongly_disagree'= 1, 'somewhat_disagree' = 2,
                             'neither_agree_disagree' = 3,
                             'somewhat_agree' = 4, 'strongly_agree' = 5)

#computing aggregate

rawdata$gayrights3item <- rowMeans(subset(rawdata, select = c(protectgayrights, gayworkrights, gaylegal)), na.rm = TRUE)
rawdata$gayrights2item <- rowMeans(subset(rawdata, select = c(protectgayrights, gayworkrights)), na.rm = TRUE)
rawdata$transrights <- rowMeans(subset(rawdata, select = c(protecttransrights, transworkrights, translegal)), na.rm = TRUE)

rawdata$gayrights3item[is.nan(rawdata$gayrights3item)] <- NA
rawdata$gayrights2item[is.nan(rawdata$gayrights2item)] <- NA
rawdata$transrights[is.nan(rawdata$transrights)] <- NA

rawdata$countrygayright <- rawdata$gay.rights
rawdata$countrytransright <- rawdata$trans.rights

#grand mean center country variables

rawdata$countrygayright.cent <- scale(rawdata$countrygayright, center = TRUE, scale = FALSE)
rawdata$countrytransright.cent <- scale(rawdata$countrytransright, center = TRUE, scale = FALSE)
rawdata$democracy.index.cent <- scale(rawdata$democracy.index, center = TRUE, scale = FALSE)
rawdata$GDP.per.capita.cent <- scale(rawdata$GDP.per.capita, center = TRUE, scale = FALSE)
rawdata$HDI.cent <- scale(rawdata$HDI, center = TRUE, scale = FALSE)

#group center gender

rawdata$gendernum<- recode(rawdata$gender, 'female'= 0, 'male' = 1,
                           'other' = 2)
rawdata$gendernum <- na_if(rawdata$gendernum, 2)

rawdata$gender.grpcent <- group.center(rawdata$gendernum, rawdata$country_name)

#group center age

rawdata$age.grpcent <- group.center(rawdata$age, rawdata$country_name)

##

rawdata$GDP10000 <- rawdata$GDP.per.capita/10000
rawdata$GDP10000.cent <- scale(rawdata$GDP10000, center = TRUE, scale = FALSE)

## create LGBT country rights ####

rawdata$LGBTcountryrights <- rawdata$countrygayright + rawdata$countrytransright

rawdata$LGBTcountryrights.cent <- scale(rawdata$LGBTcountryrights, center = TRUE, scale = FALSE)

library(lme4)
library(lmerTest)

#### add regional dummy codes####

##south and southeast asia
rawdata$DC1 <- recode(rawdata$country_name, 'Bangladesh'= 1, 'India' = 1, 'Pakistan'=1, 
                      'Sri Lanka' = 1, 'Indonesia' = 1, 'Malaysia' = 1, 
                      'Philippines' = 1, 'Singapore' = 1, 'Thailand' = 1, 'Vietnam' = 1,
                      .default= 0)

## north and west europe
rawdata$DC2 <- recode(rawdata$country_name, 'Finland'= 1, 'France' = 1, 'Germany'=1, 
                      'Italy' = 1, 'Netherlands' = 1, 'Portugal' = 1, 
                      'Spain' = 1, 'Sweden' = 1,
                      .default= 0)

## south america

rawdata$DC3 <- recode(rawdata$country_name, 'Argentina'= 1, 'Bolivia' = 1, 'Brazil'=1, 
                      'Chile' = 1, 'Colombia' = 1, 'Ecuador' = 1, 
                      'Peru' = 1, 'Venezuela' = 1,
                      .default= 0)

## eastern europe

rawdata$DC4 <- recode(rawdata$country_name, 'Albania'= 1, 'Azerbaijan' = 1, 'Bosnia and Herzegovina'=1, 
                      'Croatia' = 1, 'Hungary' = 1, 'Macedonia' = 1, 
                      'Poland' = 1, 'Romania' = 1, 'Russia' = 1, 'Serbia' = 1, 'Ukraine' = 1, 'Greece' = 1,
                      .default= 0)

## central america and carribean

rawdata$DC5 <- recode(rawdata$country_name, 'Jamaica'= 1, 'Trinidad and Tobago' = 1, 'Costa Rica'=1, 
                      'Dominican Republic' = 1, 'Mexico' = 1,
                      .default= 0)

## central and east asia

rawdata$DC6 <- recode(rawdata$country_name, 'Kazakhstan'= 1, 'Uzbekistan' = 1, 'China'=1, 
                      'Hong Kong' = 1, 'Japan' = 1, 'Korea, South' = 1, 'Taiwan' = 1,
                      .default= 0)

##anglo-saxon

rawdata$DC7 <- recode(rawdata$country_name, 'Ireland'= 1, 'United Kingdom' = 1, 'Australia'=1, 
                      'New Zealand' = 1, 'Canada' = 1, 'United States' = 1, 
                      .default= 0)

## middle east and north africa

rawdata$DC8 <- recode(rawdata$country_name, 'Algeria'= 1, 'Egypt' = 1, 'Morocco'=1, 
                      'Iraq' = 1, 'Iran' = 1, 'Israel' = 1, 'Jordan' = 1, 'Saudi Arabia' = 1,
                      'Turkey' = 1,
                      .default= 0)

##models that exclude 98 as age####

rawdata$age.r <- rawdata$age

rawdata$age.r[rawdata$age.r == 98] <- NA

rawdata$age.r.grpcent <- group.center(rawdata$age.r, rawdata$country_name)

##gay close

rawdata$gayknowDKNA.3 <- recode(rawdata$m1a_q05_personally_know, 'yes'= 1, 'no' = 0, 'dont_know' = 3)
rawdata$gaycloseDKNA.3 <- recode(rawdata$m1a_q05b_family_member, 'yes'= 1, 'no' = 0, 'dont_know' = 3)

gaycloseDKNA.new3 <- ifelse((rawdata$gayknowDKNA.3 == 0 | rawdata$gaycloseDKNA.3 == 0), 0,
                            ifelse((rawdata$gaycloseDKNA.3 == 1), 1,
                                   ifelse((rawdata$gayknowDKNA.3 ==3 | rawdata$gaycloseDKNA.3 == 3),3, NA)))

rawdata <- data.frame(rawdata, gaycloseDKNA.new3)

rawdata$gaycloseDKNA.new3[rawdata$gaycloseDKNA.new3== 3] <- NA

rawdata$gaycloseDKNA.new3.grpcent <- group.center(rawdata$gaycloseDKNA.new3, rawdata$country_name)

agg4 = aggregate(rawdata[,c("gaycloseDKNA.new3")],
                 by = list(rawdata$country_name), FUN = mean, na.rm = TRUE)

rawdata$gaycloseDKNA.new3.agg <- agg4$x[match(rawdata$country_name, agg4$Group.1)]

rawdata$gaycloseDKNA.new3.agg.cent <- scale(rawdata$gaycloseDKNA.new3.agg, center = TRUE, scale = FALSE)

## trans close ####

rawdata$transknowDKNA.3 <- recode(rawdata$m1c_q05_know_someone_id_born, 'yes'= 1, 'no' = 0, 'do_not_know' = 3)

rawdata$transcloseDKNA.3 <- recode(rawdata$m1c_q06_family_friends_id_born, 'yes'= 1, 'no' = 0, 'do_not_know' = 3)

transcloseDKNA.new3 <- ifelse((rawdata$transknowDKNA.3 == 0 | rawdata$transcloseDKNA.3 == 0), 0,
                              ifelse((rawdata$transcloseDKNA.3 == 1), 1,
                                     ifelse((rawdata$transknowDKNA.3 ==3 | rawdata$transcloseDKNA.3 == 3),3, NA)))

rawdata <- data.frame(rawdata, transcloseDKNA.new3)

rawdata$transcloseDKNA.new3[rawdata$transcloseDKNA.new3== 3] <- NA

rawdata$transcloseDKNA.new3.grpcent <- group.center(rawdata$transcloseDKNA.new3, rawdata$country_name)

agg5 = aggregate(rawdata[,c("transcloseDKNA.new3")],
                 by = list(rawdata$country_name), FUN = mean, na.rm = TRUE)

rawdata$transcloseDKNA.new3.agg <- agg5$x[match(rawdata$country_name, agg4$Group.1)]

rawdata$transcloseDKNA.new3.agg.cent <- scale(rawdata$transcloseDKNA.new3.agg, center = TRUE, scale = FALSE)


#creating variables for combined LGBT group

#LGBT rights (all items)

rawdata$LGBTrightsallitems <- rowMeans(subset(rawdata, select = c(protectgayrights, gayworkrights, 
                                                                  gaylegal, protecttransrights, transworkrights, translegal)),
                                       na.rm = TRUE)

#LGBT closeDKNA

rawdata$transcloseDKNA.4LGBT <- ifelse((rawdata$transknowDKNA.3 == 0 | rawdata$transcloseDKNA.3 == 0), 0,
                              ifelse((rawdata$transcloseDKNA.3 == 1), 1,
                                     ifelse((rawdata$transknowDKNA.3 ==3 | rawdata$transcloseDKNA.3 == 3),3, NA)))
rawdata$gaycloseDKNA.4LBGT <- ifelse((rawdata$gayknowDKNA.3 == 0 | rawdata$gaycloseDKNA.3 == 0), 0,
                            ifelse((rawdata$gaycloseDKNA.3 == 1), 1,
                                   ifelse((rawdata$gayknowDKNA.3 ==3 | rawdata$gaycloseDKNA.3 == 3),3, NA)))


rawdata$LGBTcloseDKNA.new <- ifelse((rawdata$gaycloseDKNA.4LBGT == 1) | (rawdata$transcloseDKNA.4LGBT == 1), 1,
                                    ifelse((rawdata$gaycloseDKNA.4LBGT == 0) & (rawdata$transcloseDKNA.4LGBT == 0), 0,
                                           ifelse((rawdata$gaycloseDKNA.4LBGT == 0) & (rawdata$transcloseDKNA.4LGBT == 3), 0,
                                                  ifelse((rawdata$gaycloseDKNA.4LBGT ==3) & (rawdata$transcloseDKNA.4LGBT == 0), 0, NA))))

table(rawdata$gaycloseDKNA.new3)
table(rawdata$transcloseDKNA.new3)
table(rawdata$LGBTcloseDKNA.new)

#group mean center LGBT (level-1) aggregates

library(robumeta)

rawdata$LGBTcloseDKNA.new.grpcent <- group.center(rawdata$LGBTcloseDKNA.new, rawdata$country_name)

#create contextual LGBT contact aggregates

agg2 = aggregate(rawdata[,c("LGBTcloseDKNA.new")],
                 by = list(rawdata$country_name), FUN = mean, na.rm = TRUE)

rawdata$LGBTcloseDKNA.new.agg <- agg2$x[match(rawdata$country_name, agg2$Group.1)]

rawdata$LGBTcloseDKNA.new.agg.cent <- scale(rawdata$LGBTcloseDKNA.new.agg, center = TRUE, scale = FALSE)



##models


#### A3.r2.reg- gay close 2 items, DK = NA, including 98 age, region control

A3.r2.reg<- lmer(gayrights2item ~ 1 + gaycloseDKNA.new3.grpcent +  
                   countrygayright.cent + 
                   countrygayright.cent*gaycloseDKNA.new3.grpcent +
                   gaycloseDKNA.new3.agg.cent +
                   GDP10000.cent + 
                   HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                   DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                   (1 + gaycloseDKNA.new3.grpcent|country_name),
                 data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A3.r2.reg)

#### A4.r2.reg- gay close 3 items, DK = NA, including 98 age, region control

A4.r2.reg <- lmer(gayrights3item ~ 1 + gaycloseDKNA.new3.grpcent +  
                    countrygayright.cent + 
                    countrygayright.cent*gaycloseDKNA.new3.grpcent +
                    gaycloseDKNA.new3.agg.cent +
                    GDP10000.cent + 
                    HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                    DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                    (1 + gaycloseDKNA.new3.grpcent|country_name),
                  data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A4.r2.reg)

### B2.r2.reg- trans close, DK = NA, including 98 age, region control

B2.r2.reg <- lmer(transrights ~ 1 + transcloseDKNA.new3.grpcent + 
                    countrytransright.cent + 
                    countrytransright.cent*transcloseDKNA.new3.grpcent +
                    transcloseDKNA.new3.agg.cent +
                    GDP10000.cent + 
                    HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                    DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                    (1 + transcloseDKNA.new3.grpcent|country_name),
                  data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(B2.r2.reg)

library(interactions)

sim_slopes(B2.r2.reg, pred = transcloseDKNA.new3.grpcent, modx = countrytransright.cent,
           johnson_neyman = FALSE)

#### models excluding age, region control ####

#### A3.r2.exage.reg- gay close 2 items, DK = NA, excluding 98 age, region control

A3.r2.exage.reg<- lmer(gayrights2item ~ 1 + gaycloseDKNA.new3.grpcent +  
                         countrygayright.cent + 
                         countrygayright.cent*gaycloseDKNA.new3.grpcent +
                         gaycloseDKNA.new3.agg.cent +
                         GDP10000.cent + 
                         HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                         DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                         (1 + gaycloseDKNA.new3.grpcent|country_name),
                       data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A3.r2.exage.reg)


#### A4.r2.exage.reg- gay close 3 items, DK = NA, excluding 98 age, region control

A4.r2.exage.reg <- lmer(gayrights3item ~ 1 + gaycloseDKNA.new3.grpcent +  
                          countrygayright.cent + 
                          countrygayright.cent*gaycloseDKNA.new3.grpcent +
                          gaycloseDKNA.new3.agg.cent +
                          GDP10000.cent + 
                          HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                          DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                          (1 + gaycloseDKNA.new3.grpcent|country_name),
                        data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A4.r2.exage.reg)

### B2.r2.exage.reg- trans close, DK = NA, excluding 98 age, region control

B2.r2.exage.reg <- lmer(transrights ~ 1 + transcloseDKNA.new3.grpcent + 
                          countrytransright.cent + 
                          countrytransright.cent*transcloseDKNA.new3.grpcent +
                          transcloseDKNA.new3.agg.cent +
                          GDP10000.cent + 
                          HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                          DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                          (1 + transcloseDKNA.new3.grpcent|country_name),
                        data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(B2.r2.exage.reg)

sim_slopes(B2.r2.exage.reg, pred = transcloseDKNA.new3.grpcent, modx = countrytransright.cent,
           johnson_neyman = FALSE)

### not controlling for region

#### A3.r2- gay close 2 items, DK = NA, including 98 age

A3.r2 <- lmer(gayrights2item ~ 1 + gaycloseDKNA.new3.grpcent +  
                countrygayright.cent + 
                countrygayright.cent*gaycloseDKNA.new3.grpcent +
                gaycloseDKNA.new3.agg.cent +
                GDP10000.cent + 
                HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                (1 + gaycloseDKNA.new3.grpcent|country_name),
              data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A3.r2)

#### A4.r2- gay close 3 items, DK = NA, including 98 age

A4.r2 <- lmer(gayrights3item ~ 1 + gaycloseDKNA.new3.grpcent +  
                countrygayright.cent + 
                countrygayright.cent*gaycloseDKNA.new3.grpcent +
                gaycloseDKNA.new3.agg.cent +
                GDP10000.cent + 
                HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                (1 + gaycloseDKNA.new3.grpcent|country_name),
              data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A4.r2)

### B2.r2- trans close, DK = NA, including 98 age

B2.r2 <- lmer(transrights ~ 1 + transcloseDKNA.new3.grpcent + 
                countrytransright.cent + 
                countrytransright.cent*transcloseDKNA.new3.grpcent +
                transcloseDKNA.new3.agg.cent +
                GDP10000.cent + 
                HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                (1 + transcloseDKNA.new3.grpcent|country_name),
              data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(B2.r2)

sim_slopes(B2.r2, pred = transcloseDKNA.new3.grpcent, modx = countrytransright.cent,
           johnson_neyman = FALSE)

#### models dropping three way interaction, excluding age ####

#### A3.r2.exage- gay close 2 items, DK = NA, excluding 98 age

A3.r2.exage <- lmer(gayrights2item ~ 1 + gaycloseDKNA.new3.grpcent +  
                      countrygayright.cent + 
                      countrygayright.cent*gaycloseDKNA.new3.grpcent +
                      gaycloseDKNA.new3.agg.cent +
                      GDP10000.cent + 
                      HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                      (1 + gaycloseDKNA.new3.grpcent|country_name),
                    data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A3.r2.exage)

#### A4.r2.exage- gay close 3 items, DK = NA, excluding 98 age

A4.r2.exage <- lmer(gayrights3item ~ 1 + gaycloseDKNA.new3.grpcent +  
                      countrygayright.cent + 
                      countrygayright.cent*gaycloseDKNA.new3.grpcent +
                      gaycloseDKNA.new3.agg.cent +
                      GDP10000.cent + 
                      HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                      (1 + gaycloseDKNA.new3.grpcent|country_name),
                    data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary (A4.r2.exage)


### B2.r2.exage- trans close, DK = NA, including 98 age

B2.r2.exage <- lmer(transrights ~ 1 + transcloseDKNA.new3.grpcent + 
                      countrytransright.cent + 
                      countrytransright.cent*transcloseDKNA.new3.grpcent +
                      transcloseDKNA.new3.agg.cent +
                      GDP10000.cent + 
                      HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                      (1 + transcloseDKNA.new3.grpcent|country_name),
                    data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(B2.r2.exage)

sim_slopes(B2.r2.exage, pred = transcloseDKNA.new3.grpcent, modx = countrytransright.cent,
           johnson_neyman = FALSE)



#### LGBT combined ####

#### C.r2- LGBT close, DK = NA, including 98 age

C.r2 <- lmer(LGBTrightsallitems ~ 1 + LGBTcloseDKNA.new.grpcent + LGBTcloseDKNA.new.agg.cent + 
               LGBTcountryrights.cent + 
               LGBTcountryrights.cent*LGBTcloseDKNA.new.grpcent +
               GDP10000.cent + 
               HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
               (1 + LGBTcloseDKNA.new.grpcent|country_name),
             data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(C.r2)

#### C.r2.exage- LGBT close, DK = NA, excluding 98 age

C.r2.exage <- lmer(LGBTrightsallitems ~ 1 + LGBTcloseDKNA.new.grpcent + LGBTcloseDKNA.new.agg.cent + 
                     LGBTcountryrights.cent + 
                     LGBTcountryrights.cent*LGBTcloseDKNA.new.grpcent +
                     GDP10000.cent + 
                     HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                     (1 + LGBTcloseDKNA.new.grpcent|country_name),
                   data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(C.r2.exage)


#### C.r2.reg- LGBT close, DK = NA, including 98 age, region control

C2.r2.reg <- lmer(LGBTrightsallitems ~ 1 + LGBTcloseDKNA.new.grpcent + LGBTcloseDKNA.new.agg.cent + 
                    LGBTcountryrights.cent + 
                    LGBTcountryrights.cent*LGBTcloseDKNA.new.grpcent +
                    GDP10000.cent + 
                    HDI.cent + democracy.index.cent + gender.grpcent + age.grpcent +
                    DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                    (1 + LGBTcloseDKNA.new.grpcent|country_name),
                  data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(C2.r2.reg)


#### C.r2.exage.reg- LGBT close, DK = NA, excluding 98 age, region control

C2.r2.exage.reg <- lmer(LGBTrightsallitems ~ 1 + LGBTcloseDKNA.new.grpcent + LGBTcloseDKNA.new.agg.cent + 
                          LGBTcountryrights.cent + 
                          LGBTcountryrights.cent*LGBTcloseDKNA.new.grpcent +
                          GDP10000.cent + 
                          HDI.cent + democracy.index.cent + gender.grpcent + age.r.grpcent +
                          DC1 + DC2 + DC3 + DC4 + DC5 + DC6 + DC7 + DC8 +
                          (1 + LGBTcloseDKNA.new.grpcent|country_name),
                        data = rawdata, control= lmerControl(optimizer = "Nelder_Mead"))
summary(C2.r2.exage.reg)




