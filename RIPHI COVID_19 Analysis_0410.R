#Load in data set
covid <- read.csv("~/Desktop/RADx COVID Data.csv")
covid_copy<-covid

library(dplyr)

#================ Data Management ====================
###Rename
covid_copy <- covid_copy %>%
  rename(appmt_date = Appointment.date,
         appmt_time = Appointment.time.,
         appmt_type = Appointment.Type.,
         appmt_other_reason = Other.reason.for.visit..please.specify.,
         age = Age,
         sex_birth = Sex.assigned.at.birth,
         gender_current = Current.gender.identity.,
         gender_specify = Please.specify.current.gender.identity.,
         ethnicity = Ethnicity,
         zipcode = Zip.code.of.residence.,
         consent_rapid = Did.the.patient.consent.to.COVID.19.rapid.testing.,
         symptoms = Did.the.patient.have.any.symptoms.,
         symptoms_cough = Specify.symptoms....choice.Cough.,
         symptoms_feverchills = Specify.symptoms....choice.Fever.chills.,
         symptoms_musclepain = Specify.symptoms....choice.Muscle.pain.,
         symptoms_sorethroat = Specify.symptoms....choice.Sore.throat.,
         symptoms_headache = Specify.symptoms....choice.Headache.,
         symptoms_vomit = Specify.symptoms....choice.Nausea.vomiting.,
         symptoms_diarrhea = Specify.symptoms....choice.Diarrhea.,
         symptoms_runnynose = Specify.symptoms....choice.Runny.nose.,
         symptoms_fatigue = Specify.symptoms....choice.Fatigue.,
         symptoms_congestion = Specify.symptoms....choice.Congestion.,
         symptoms_losstastesmell = Specify.symptoms....choice.Loss.of.taste.smell.,
         symptoms_other = Specify.symptoms....choice.Other.,
         symptoms_other_specify = Please.specify.other.symptoms.,
         positive_past = Has.the.patient.ever.tested.positive.for.COVID.19.,
         positive_apxdate = Approximate.date.s..patient.tested.positive.,
         closecontact_10 = In.the.last.10.days..has.this.patient.been.in.close.contact.with.someone.who.tested.positive.for.COVID.19.,
         closecontact_date = Date.of.last.contact.,
         travel_NewEngland_14 = Has.the.patient.traveled.outside.of.New.England.in.the.last.14.days.,
         travel_location = Location.of.travel.,
         travel_datereturn = Date.returned.from.travel.,
         test_last = When.was.the.patient.s.last.COVID.19.test.,
         vax_brand_1st = What.brand.did.the.patient.receive.for.their.1st.dose.,
         vax_brand_2nd = What.brand.did.the.patient.receive.for.their.2nd.dose.,
         vax_brand_booster = What.brand.did.the.patient.receive.for.their.booster.,
         vax_date_1st = Date.of.first.vaccination.,
         vax_date_2nd = Date.of.second.vaccination.,
         vax_date_booster = Date.of.booster.,
         novax_why_naturalimmune = If.not.vaccinated..why...choice.I.had.COVID.19.and.have.natural.immunity.,
         novax_why_notreq = If.not.vaccinated..why...choice.I.am.not.required.to.,
         novax_why_sideeffects = If.not.vaccinated..why...choice.I.am.worried.about.long.term.side.effects.,
         novax_why_infertility = If.not.vaccinated..why...choice.I.am.worried.about.infertility.,
         novax_why_other = If.not.vaccinated..why...choice.Other.reason.,
         novax_why_other_specify = If.other.reason.for.not.being.vaccinated..please.specify.,
         test_rapid = What.type.of.COVID.19.test.did.the.patient.get...choice.Rapid.,
         test_PCR = What.type.of.COVID.19.test.did.the.patient.get...choice.PCR.,
         test_none = What.type.of.COVID.19.test.did.the.patient.get...choice.None.,
         test_rapid_results = Rapid.Test.Results,
         test_PCR_results = PCR.Test.Results,
         recently_tested = Reason.why.patient.refused.COVID.19.testing...choice.Patient.was.recently.tested.,
         tested_positive_in_90_days = Reason.why.patient.refused.COVID.19.testing...choice.Patient.tested.positive.for.COVID.19.in.the.past.90.days.,
         no_symptoms = Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.no.signs.or.symptoms.,
         no_exposure = Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.not.been.exposed.,
         want_holiday_not_in_isolation = Reason.why.patient.refused.COVID.19.testing...choice.Patient.didnt.want.to.isolate.for.the.holidays.,
         think_no_need = Reason.why.patient.refused.COVID.19.testing...choice.Patient.did.not.think.they.needed.a.test.,
         other = Reason.why.patient.refused.COVID.19.testing...choice.Other.reason.)

##recode current gender identity
covid_copy$genderId_coded[covid_copy$gender_current==""] <-NA
covid_copy$genderId_coded[covid_copy$gender_current=="Man"] <- 1
covid_copy$genderId_coded[covid_copy$gender_current=="Woman"] <- 2
covid_copy$genderId_coded[covid_copy$gender_current=="Gender nonconforming"|
                            covid_copy$gender_current=="Genderqueer"|
                            covid_copy$gender_current=="Non-binary"|
                            covid_copy$gender_current=="Questioning/Unsure"|
                            covid_copy$Record.ID==68] <- 3
covid_copy$genderId_coded[ (covid_copy$gender_current=="Not listed" & covid_copy$Record.ID!=68)|
                             covid_copy$gender_current=="Choose not to disclose"] <- 4

#summary(covid_copy$genderId_coded)
#covid_copy$genderId_coded<-as.factor(covid_copy$genderId_coded)
##whether use as a factor - depends on regression model

###code race -- use race or ethnicity?
covid_copy$race_StringCoded[covid_copy$Race...choice.White.=="Checked"] <- "White"
covid_copy$race_StringCoded[covid_copy$Race...choice.African.American.=="Checked"] <- "African American"
covid_copy$race_StringCoded[covid_copy$Race...choice.Asian.=="Checked"] <- "Asian"
covid_copy$race_StringCoded[covid_copy$Race...choice.American.Indian.Pacific.Islander.=="Checked"] <- "American Indian Pacific Islander"
covid_copy$race_StringCoded[covid_copy$Record.ID==42|covid_copy$Record.ID==127|covid_copy$Record.ID==183] <- "Hispanic"
covid_copy$race_StringCoded[covid_copy$Race...choice.Other.Not.listed.=="Checked" &
                              covid_copy$Record.ID!=c(42,127,183)] <- "Not disclosed"

###code insurance
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.Medicaid.=="Checked"|covid_copy$Primary.Insurance.Payer...choice.Medicare.=="Checked"] <- "Public"
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.Private.=="Checked"] <- "Private"
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.None.=="Checked"] <- "None"

###code whether vaccinated
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.No.=="Checked"] <- "No"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...one.dose.=="Checked"] <- "1 dose"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...2.doses.=="Checked"|
                          covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.=="Checked"] <- "2 doses"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...Booster.=="Checked"] <- "Booster"

###code reason of refusing rapid test - > each reason is in its own column!!!! [MULTIPLE CHOICE/SELECTION QUESTION]
covid_copy$ReasonRefuse_RecentlyTested<-ifelse(covid_copy$recently_tested=="Checked"|
                                                 covid_copy$Record.ID %in% c(222,228,230,280,306,346),1,0)
covid_copy$ReasonRefuse_TestedPosIn90 <-ifelse(covid_copy$tested_positive_in_90_days=="Checked"|
                          covid_copy$Record.ID %in% c(220,342),1,0)
covid_copy$ReasonRefuse_NoSymp <- ifelse(covid_copy$no_symptoms=="Checked",1,0)
covid_copy$ReasonRefuse_NoExposure <- ifelse(covid_copy$no_exposure=="Checked"|covid_copy$Record.ID %in% c(279,284),1,0)
covid_copy$ReasonRefuse_Holiday <- ifelse(covid_copy$want_holiday_not_in_isolation=="Checked",1,0)
covid_copy$ReasonRefuse_PasstiveAttitude <- ifelse(covid_copy$think_no_need=="Checked"|
                          covid_copy$Record.ID %in% c(3,14,62,67,71,156,163,211,221,223,227,272,273,278,312,315,321),1,0)
covid_copy$ReasonRefuse_Vax <- ifelse(covid_copy$Please.specify.reason.why.patient.refused.testing.=="vaccinated"|
                          covid_copy$Record.ID %in% c(282,320),1,0)

###code Symptoms each variable in binary form
covid_copy$symptoms[covid_copy$symptoms == "Yes"]<-1
covid_copy$symptoms[covid_copy$symptoms == "No"]<-0
covid_copy$symptoms[covid_copy$symptoms == ""]<-NA
covid_copy$symptoms_cough[covid_copy$symptoms_cough == "Checked"]<-1
covid_copy$symptoms_cough[covid_copy$symptoms_cough == "Unchecked"]<-0
covid_copy$symptoms_feverchills[covid_copy$symptoms_feverchills == "Checked"]<-1
covid_copy$symptoms_feverchills[covid_copy$symptoms_feverchills == "Unchecked"]<-0
covid_copy$symptoms_musclepain[covid_copy$symptoms_musclepain == "Checked"]<-1
covid_copy$symptoms_musclepain[covid_copy$symptoms_musclepain == "Unchecked"]<-0
covid_copy$symptoms_sorethroat[covid_copy$symptoms_sorethroat == "Checked"]<-1
covid_copy$symptoms_sorethroat[covid_copy$symptoms_sorethroat== "Unchecked"]<-0
covid_copy$symptoms_headache[covid_copy$symptoms_headache == "Checked"]<-1
covid_copy$symptoms_headache[covid_copy$symptoms_headache == "Unchecked"]<-0
covid_copy$symptoms_vomit[covid_copy$symptoms_vomit == "Checked"]<-1
covid_copy$symptoms_vomit[covid_copy$symptoms_vomit == "Unchecked"]<-0
covid_copy$symptoms_diarrhea[covid_copy$symptoms_diarrhea == "Checked"]<-1
covid_copy$symptoms_diarrhea[covid_copy$symptoms_diarrhea == "Unchecked"]<-0
covid_copy$symptoms_runnynose[covid_copy$symptoms_runnynose == "Checked"]<-1
covid_copy$symptoms_runnynose[covid_copy$symptoms_runnynose == "Unchecked"]<-0
covid_copy$symptoms_fatigue[covid_copy$symptoms_fatigue == "Checked"]<-1
covid_copy$symptoms_fatigue[covid_copy$symptoms_fatigue == "Unchecked"]<-0
covid_copy$symptoms_congestion[covid_copy$symptoms_congestion == "Checked"]<-1
covid_copy$symptoms_congestion[covid_copy$symptoms_congestion == "Unchecked"]<-0
covid_copy$symptoms_losstastesmell[covid_copy$symptoms_losstastesmell == "Checked"]<-1
covid_copy$symptoms_losstastesmell[covid_copy$symptoms_losstastesmell == "Unchecked"]<-0
covid_copy$symptoms_other[covid_copy$symptoms_other == "Checked"]<-1
covid_copy$symptoms_other[covid_copy$symptoms_other == "Unchecked"]<-0


###Compile Symptom Score, a quantitative variable showing the number of symptoms each patient got
covid_copy %>% 
  mutate(symptoms_congestion=as.numeric(symptoms_congestion),
         symptoms_cough=as.numeric(symptoms_cough),
         symptoms_diarrhea=as.numeric(symptoms_diarrhea),
         symptoms_fatigue=as.numeric(symptoms_fatigue),
         symptoms_feverchills=as.numeric(symptoms_feverchills),
         symptoms_headache=as.numeric(symptoms_headache),
         symptoms_losstastesmell=as.numeric(symptoms_losstastesmell),
         symptoms_musclepain=as.numeric(symptoms_musclepain),
         symptoms_runnynose=as.numeric(symptoms_runnynose),
         symptoms_sorethroat=as.numeric(symptoms_sorethroat),
         symptoms_vomit=as.numeric(symptoms_vomit),
         symptoms_other=as.numeric(symptoms_other)) -> covid_copy

covid_copy$symp_scores<-covid_copy$symptoms_congestion+covid_copy$symptoms_cough+covid_copy$symptoms_diarrhea+
  covid_copy$symptoms_fatigue+covid_copy$symptoms_feverchills+covid_copy$symptoms_headache+covid_copy$symptoms_losstastesmell+
  covid_copy$symptoms_musclepain+covid_copy$symptoms_runnynose+covid_copy$symptoms_sorethroat+covid_copy$symptoms_vomit+
  covid_copy$symptoms_other

###delete redundant variables
covid_copy=subset(covid_copy,select = -c(Race...choice.White.,Race...choice.African.American.,Race...choice.Asian.,
                                         Race...choice.American.Indian.Pacific.Islander.,Race...choice.Other.Not.listed.,
                                         Primary.Insurance.Payer...choice.Medicaid.,Primary.Insurance.Payer...choice.Medicare.,
                                         Primary.Insurance.Payer...choice.Private.,Primary.Insurance.Payer...choice.None.,
                                         Race.other.not.listed..please.specify.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.No.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...one.dose.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...2.doses.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...Booster.,
                                         Please.specify.reason.why.patient.refused.testing.))


###code last Covid test
covid_copy$last_test[covid_copy$test_last==""]<-NA
covid_copy$last_test[covid_copy$test_last=="Last 7 days"]<-1
covid_copy$last_test[covid_copy$test_last=="1-2 weeks"]<-2
covid_copy$last_test[covid_copy$test_last=="3-4 weeks"]<-3
covid_copy$last_test[covid_copy$test_last=="1-2 months"]<-4
covid_copy$last_test[covid_copy$test_last=="More than 2 months ago"]<-5
covid_copy$last_test[covid_copy$test_last=="Never"]<-6

###code ReasonRefused each variable in binary form
covid_copy$recently_tested[covid_copy$recently_tested == "Checked"]<-1
covid_copy$recently_tested[covid_copy$recently_tested == "Unchecked"]<-0
covid_copy$tested_positive_in_90_days[covid_copy$tested_positive_in_90_days == "Checked"]<-1
covid_copy$tested_positive_in_90_days[covid_copy$tested_positive_in_90_days == "Unchecked"]<-0
covid_copy$no_symptoms[covid_copy$no_symptoms == "Checked"]<-1
covid_copy$no_symptoms[covid_copy$no_symptoms == "Unchecked"]<-0
covid_copy$want_holiday_not_in_isolation[covid_copy$want_holiday_not_in_isolation == "Checked"]<-1
covid_copy$want_holiday_not_in_isolation[covid_copy$want_holiday_not_in_isolation == "Unchecked"]<-0
covid_copy$think_no_need[covid_copy$think_no_need == "Checked"]<-1
covid_copy$think_no_need[covid_copy$think_no_need == "Unchecked"]<-0
covid_copy$no_exposure[covid_copy$no_exposure == "Checked"]<-1
covid_copy$no_exposure[covid_copy$no_exposure == "Unchecked"]<-0
covid_copy$other[covid_copy$other == "Checked"]<-1
covid_copy$other[covid_copy$other == "Unchecked"]<-0

###code Symptoms each variable in binary form
covid_copy$symptoms[covid_copy$symptoms == "Yes"]<-1
covid_copy$symptoms[covid_copy$symptoms == "No"]<-0
covid_copy$symptoms[covid_copy$symptoms == ""]<-NA
covid_copy$symptoms_cough[covid_copy$symptoms_cough == "Checked"]<-1
covid_copy$symptoms_cough[covid_copy$symptoms_cough == "Unchecked"]<-0
covid_copy$symptoms_feverchills[covid_copy$symptoms_feverchills == "Checked"]<-1
covid_copy$symptoms_feverchills[covid_copy$symptoms_feverchills == "Unchecked"]<-0
covid_copy$symptoms_musclepain[covid_copy$symptoms_musclepain == "Checked"]<-1
covid_copy$symptoms_musclepain[covid_copy$symptoms_musclepain == "Unchecked"]<-0
covid_copy$symptoms_sorethroat[covid_copy$symptoms_sorethroat == "Checked"]<-1
covid_copy$symptoms_sorethroat[covid_copy$symptoms_sorethroat== "Unchecked"]<-0
covid_copy$symptoms_headache[covid_copy$symptoms_headache == "Checked"]<-1
covid_copy$symptoms_headache[covid_copy$symptoms_headache == "Unchecked"]<-0
covid_copy$symptoms_vomit[covid_copy$symptoms_vomit == "Checked"]<-1
covid_copy$symptoms_vomit[covid_copy$symptoms_vomit == "Unchecked"]<-0
covid_copy$symptoms_diarrhea[covid_copy$symptoms_diarrhea == "Checked"]<-1
covid_copy$symptoms_diarrhea[covid_copy$symptoms_diarrhea == "Unchecked"]<-0
covid_copy$symptoms_runnynose[covid_copy$symptoms_runnynose == "Checked"]<-1
covid_copy$symptoms_runnynose[covid_copy$symptoms_runnynose == "Unchecked"]<-0
covid_copy$symptoms_fatigue[covid_copy$symptoms_fatigue == "Checked"]<-1
covid_copy$symptoms_fatigue[covid_copy$symptoms_fatigue == "Unchecked"]<-0
covid_copy$symptoms_congestion[covid_copy$symptoms_congestion == "Checked"]<-1
covid_copy$symptoms_congestion[covid_copy$symptoms_congestion == "Unchecked"]<-0
covid_copy$symptoms_losstastesmell[covid_copy$symptoms_losstastesmell == "Checked"]<-1
covid_copy$symptoms_losstastesmell[covid_copy$symptoms_losstastesmell == "Unchecked"]<-0
covid_copy$symptoms_other[covid_copy$symptoms_other == "Checked"]<-1
covid_copy$symptoms_other[covid_copy$symptoms_other == "Unchecked"]<-0

###code other symptoms
covid_copy$symptoms_runnynose[covid_copy$Record.ID %in% c(82)]<-1
covid_copy$symptoms_losstastesmell[covid_copy$Record.ID %in% c(172)]<-1
covid_copy$symptoms_musclepain[covid_copy$Record.ID %in% c(195)]<-1
covid_copy$symptoms_sorethroat[covid_copy$Record.ID %in% c(262)]<-1
covid_copy$symptoms_diarrhea[covid_copy$Record.ID %in% c(263)]<-1
#write.csv(covid_copy, "~/Desktop/Covid_copy") #Export dataset

###Compile Symptom Score, a quantitative variable showing the number of symptoms each patient got
covid_copy %>% 
  mutate(symptoms_congestion=as.numeric(symptoms_congestion),
         symptoms_cough=as.numeric(symptoms_cough),
         symptoms_diarrhea=as.numeric(symptoms_diarrhea),
         symptoms_fatigue=as.numeric(symptoms_fatigue),
         symptoms_feverchills=as.numeric(symptoms_feverchills),
         symptoms_headache=as.numeric(symptoms_headache),
         symptoms_losstastesmell=as.numeric(symptoms_losstastesmell),
         symptoms_musclepain=as.numeric(symptoms_musclepain),
         symptoms_runnynose=as.numeric(symptoms_runnynose),
         symptoms_sorethroat=as.numeric(symptoms_sorethroat),
         symptoms_vomit=as.numeric(symptoms_vomit),
         symptoms_other=as.numeric(symptoms_other)) -> covid_copy

covid_copy$symp_scores<-covid_copy$symptoms_congestion+covid_copy$symptoms_cough+covid_copy$symptoms_diarrhea+
  covid_copy$symptoms_fatigue+covid_copy$symptoms_feverchills+covid_copy$symptoms_headache+covid_copy$symptoms_losstastesmell+
  covid_copy$symptoms_musclepain+covid_copy$symptoms_runnynose+covid_copy$symptoms_sorethroat+covid_copy$symptoms_vomit+
  covid_copy$symptoms_other

###code NoVaxReason each variable in binary form
covid_copy$novax_why_naturalimmune[covid_copy$novax_why_naturalimmune == "Checked"]<-1
covid_copy$novax_why_naturalimmune[covid_copy$novax_why_naturalimmune == "Unchecked"]<-0
covid_copy$novax_why_notreq[covid_copy$novax_why_notreq == "Checked"]<-1
covid_copy$novax_why_notreq[covid_copy$novax_why_notreq == "Unchecked"]<-0
covid_copy$novax_why_sideeffects[covid_copy$novax_why_sideeffects == "Checked"]<-1
covid_copy$novax_why_sideeffects[covid_copy$novax_why_sideeffects == "Unchecked"]<-0
covid_copy$novax_why_infertility[covid_copy$novax_why_infertility == "Checked"]<-1
covid_copy$novax_why_infertility[covid_copy$novax_why_infertility == "Unchecked"]<-0
covid_copy$novax_why_other[covid_copy$novax_why_other == "Checked"]<-1
covid_copy$novax_why_other[covid_copy$novax_why_other == "Unchecked"]<-0

###code vaxtype
###code Travel_New_England
covid_copy$travel_NewEngland_14[covid_copy$travel_NewEngland_14 == "Yes"]<-1
covid_copy$travel_NewEngland_14[covid_copy$travel_NewEngland_14== "No"]<-0
covid_copy$travel_NewEngland_14[covid_copy$travel_NewEngland_14 == ""]<-NA
###code closecontact_10
covid_copy$closecontact_10[covid_copy$closecontact_10 == "Yes"]<-1
covid_copy$closecontact_10[covid_copy$closecontact_10== "No"]<-0
covid_copy$closecontact_10[covid_copy$closecontact_10 == ""]<-NA
###code positive_past
covid_copy$positive_past[covid_copy$positive_past == "Yes"]<-1
covid_copy$positive_past[covid_copy$positive_past == "No"]<-0
covid_copy$positive_past[covid_copy$positive_past == ""]<-NA
###code IfTested
covid_copy$IfTested[covid_copy$test_none=="Checked"]<-0
covid_copy$IfTested[covid_copy$test_none=="Unchecked"]<-1
covid_copy$IfTested[covid_copy$test_none==""]<-NA
###code test_PCR
covid_copy$test_PCR[covid_copy$test_PCR=="Checked"]<-1
covid_copy$test_PCR[covid_copy$test_PCR=="Unchecked"]<-0
covid_copy$test_PCR[covid_copy$test_PCR==""]<-NA
###code test_rapid
covid_copy$test_rapid[covid_copy$test_rapid=="Checked"]<-1
covid_copy$test_rapid[covid_copy$test_rapid=="Unchecked"]<-0
covid_copy$test_rapid[covid_copy$test_rapid==""]<-NA

###code the specified reasons for no vax into their matching categories
covid_copy$novax_why_scared<-ifelse(covid_copy$Record.ID %in% c(25,50,51,116,267,268),1,0)
covid_copy$novax_why_plannedlater<-ifelse(covid_copy$Record.ID %in% c(25,205,251,293),1,0)
covid_copy$novax_why_noreason<-ifelse(covid_copy$Record.ID %in% c(18,23,179,196,197,244,325),1,0)
covid_copy$novax_why_personal<-ifelse(covid_copy$Record.ID %in% c(52,186,289),1,0) #religion, age restriction
covid_copy$novax_why_passiveAtt<-ifelse(covid_copy$Record.ID %in% c(172,235,236,237,256),1,0) #don't trust, don't want


#=========================Descriptive Statistics and Graphs=================================

#######################  mosaic plot #####################   
library(ggplot2)
library(ggcorrplot)

# combine all Reasons to Refuse together



#create multiple tables with different combinations for attitude vars
atttbl1 <- xtabs(~ReasonRefuse + IfVaccinated , covid_copy)       
ftable(atttbl1)

atttbl2 <- xtabs(~ race_StringCoded + ReasonRefuse + genderId_coded, covid_copy)
ftable(atttbl2)

# create a mosaic plot from the tables
library(vcd)
mosaic(atttbl1, main = "Vaccination Status with Reason to Refuse Mosaic Plot")
mosaic(atttbl2, 
       labeling = labeling_values,
       labeling_args = list(set_varnames = c(ReasonRefuse = "Reason to Refuse Rapid Testing",
                                             race_StringCoded = "Race",
                                             genderId_coded = "Gender")),
       set_labels = list(ReasonRefuse = c("Vaxed","0Expos", "0Sym", "NoNeed", "RecentTest", "Pos90",  "NA" ),
                         race_StringCoded = c("AA", "AIPI", "A", "B", "H","0", "S", "W", "NA"),
                         genderId_coded = c("M", "F", "Q", "NA")),
       shade = TRUE,
       legend = TRUE,
       main = "Demographics with Reason to Refuse Mosaic Plot")


#######################  Basic EDA  #######################  

summary(covid_copy)

remotes::install_github("rkabacoff/eda", upgrades = "never", quiet = TRUE)
library("eda")

contents(covid_copy) # gives the pct of missing obs for each var
df_plot(covid_copy)
barcharts(covid_copy)



#######################  Bivariate Analysis  #######################  
# between consent_to_test and test_last
ggplot(covid_copy, 
       aes(x = consent_rapid, 
           fill = test_last)) + 
  geom_bar(position = "stack")

# between Ever_tested_positive and test_last
ggplot(covid_copy, 
       aes(x = positive_past, 
           fill = test_last)) + 
  geom_bar(position = "stack")

# between Travel and test_last
ggplot(covid_copy, 
       aes(x = travel_NewEngland_14, 
           fill = test_last)) + 
  geom_bar(position = "stack")

# between consent_to_test and Ever_tested_positive
ggplot(covid_copy, 
       aes(x = consent_rapid, 
           fill = positive_past)) + 
  geom_bar(position = "stack")

# between consent_to_test and travel
ggplot(covid_copy, 
       aes(x = consent_rapid, 
           fill = travel_NewEngland_14)) + 
  geom_bar(position = "stack")

# between Ever_tested_positive and travel
ggplot(covid_copy, 
       aes(x = travel_NewEngland_14, 
           fill = positive_past)) + 
  geom_bar(position = "stack")

# between Consent_testing and ReasonRefuse
ggplot(covid_copy, 
       aes(x = consent_rapid, 
           fill = ReasonRefuse)) + 
  geom_bar(position = "stack")

# between consent_to_test and IfVaccinated
ggplot(covid_copy, 
       aes(x = consent_rapid, 
           fill = IfVaccinated)) + 
  geom_bar(position = "stack")

# between Ever_tested_positive and ReasonRefuse
ggplot(covid_copy, 
       aes(x = positive_past, 
           fill = ReasonRefuse)) + 
  geom_bar(position = "stack")

# between Ever_tested_positive and IfVaccinated
ggplot(covid_copy, 
       aes(x = positive_past, 
           fill = IfVaccinated)) + 
  geom_bar(position = "stack")

# between ReasonRefuse and Travel
ggplot(covid_copy, 
       aes(x = ReasonRefuse, 
           fill = travel_NewEngland_14)) + 
  geom_bar(position = "stack")

# between ReasonRefuse and IfVaccinated
ggplot(covid_copy, 
       aes(x = ReasonRefuse, 
           fill = IfVaccinated)) + 
  geom_bar(position = "stack")

# between Travel and IfVaccinated
ggplot(covid_copy, 
       aes(x = travel_NewEngland_14, 
           fill = IfVaccinated)) + 
  geom_bar(position = "stack")

#######################  Univariate Analysis #######################  
library(ggplot2)
###bar graph of insurance
ggplot(covid_copy, aes(x=factor(insurance)))+
  geom_bar(stat= "count" , width=0.7, fill="steelblue")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("Insurance Type")

###bar graph of race
ggplot(covid_copy, aes(x=factor(race_StringCoded)))+
  geom_bar(stat = "count", width = 0.7, fill="steelblue")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("Race")+
  theme(axis.text.x = element_text(angle = 30))

###bar graph of gender identity
ggplot(covid_copy, aes(x=factor(genderId_coded)))+
  geom_bar(stat = "count", width = 0.7, fill="steelblue")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("Gender Identity")+
  scale_x_discrete(labels=c("Man","Woman","Gender Nonconforming", "No Disclosure","NA"))+
  theme(axis.text.x = element_text(angle = 30))

###bar graph of last test
ggplot(covid_copy, aes(x=factor(last_test)))+
  geom_bar(stat = "count", width = 0.7, fill="steelblue")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("Time Get Last Covid Test")+
  scale_x_discrete(labels=c("Last 7 days","1-2 weeks","3-4 weeks", "1-2 months",
                            "More than 2 months","Never","NA"))+
  theme(axis.text.x = element_text(angle = 30))

###bar graph of whether vaccinated
ggplot(covid_copy, aes(x=factor(IfVaccinated)))+
  geom_bar(stat = "count", width = 0.7, fill="darkolivegreen4")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("Vaccination Record")

###histogram & density plot of age
ggplot(covid_copy, aes(x=age)) + 
  geom_histogram(binwidth=1, fill="darkgoldenrod1")+
  xlab("Age")

ggplot(covid_copy, aes(x=age)) + 
  geom_density()

###histogram & density plot of symptom scores
ggplot(covid_copy, aes(x=symp_scores)) + 
  geom_histogram(binwidth=1, fill="darkgoldenrod1")+
  geom_text(aes(label = ..count..), stat = "count",vjust=-0.25,color="black")+
  xlab("How many symptoms")

ggplot(covid_copy, aes(x=symp_scores)) + 
  geom_density()

###boxplot of vaccination and age
ggplot(covid_copy,aes(x=factor(IfVaccinated), y=age))+
  geom_boxplot()+
  xlab("Vaccination Record")+
  ylab("Age")

###boxplot of last test and age
ggplot(covid_copy,aes(x=factor(last_test), y=age))+
  geom_boxplot()+
  xlab("Vaccination Record")+
  ylab("Age")+
  scale_x_discrete(labels=c("Last 7 days","1-2 weeks","3-4 weeks", "1-2 months",
                            "More than 2 months","Never","NA"))+
  theme(axis.text.x = element_text(angle = 30))

###boxplot of insurance and age
ggplot(covid_copy,aes(x=factor(insurance), y=age))+
  geom_boxplot()+
  xlab("Insurance Type")+
  ylab("Age")

### Univariate Analysis for 
#######################  Multiple selection questions #######################  

###Create ReasonRefused Bar Chart
ReasonRefuse=data.frame(covid_copy$recently_tested,covid_copy$tested_positive_in_90_days,covid_copy$no_symptoms,
                        covid_copy$want_holiday_not_in_isolation,covid_copy$think_no_need,covid_copy$no_exposure,
                        covid_copy$other)
HaveResponses=subset(covid_copy, covid_copy$recently_tested>0 | covid_copy$tested_positive_in_90_days>0 |
                       covid_copy$no_symptoms>0 | covid_copy$want_holiday_not_in_isolation>0 | 
                       covid_copy$think_no_need>0 | covid_copy$no_exposure>0 | covid_copy$other>0)
ReasonRefuse <- ReasonRefuse %>%
  rename(Recently_Tested = covid_copy.recently_tested,
         Tested_Positive_in_90_Days = covid_copy.tested_positive_in_90_days,
         No_Symptoms = covid_copy.no_symptoms,
         Dislike_Isolation = covid_copy.want_holiday_not_in_isolation,
         Think_No_Need = covid_copy.think_no_need,
         No_Exposure = covid_copy.no_exposure,
         Other = covid_copy.other)

ReasonRefuse$Recently_Tested = as.numeric(ReasonRefuse$Recently_Tested)
ReasonRefuse$Tested_Positive_in_90_Days = as.numeric(ReasonRefuse$Tested_Positive_in_90_Days)
ReasonRefuse$No_Symptoms = as.numeric(ReasonRefuse$No_Symptoms)
ReasonRefuse$Dislike_Isolation = as.numeric(ReasonRefuse$Dislike_Isolation)
ReasonRefuse$Think_No_Need = as.numeric(ReasonRefuse$Think_No_Need)
ReasonRefuse$No_Exposure = as.numeric(ReasonRefuse$No_Exposure)
ReasonRefuse$Other = as.numeric(ReasonRefuse$Other)

c = colSums(ReasonRefuse)
s = sum(ReasonRefuse)
n = nrow(HaveResponses)

a=round(c/s,3) # Percent of Responses
a

aa=barplot(a, las=1, ylim = c(0,.6), col=c(1,2,3,4,5,6,7), main="Reason for Refusing Covid Testing (Percentage)",ylab="Percentage")
text(aa, a+.02, a, pos=3)

###Create Symptoms Bar Chart
Symptoms=data.frame(covid_copy$symptoms_cough,covid_copy$symptoms_congestion,covid_copy$symptoms_diarrhea,
                    covid_copy$symptoms_feverchills,covid_copy$symptoms_musclepain,covid_copy$symptoms_sorethroat,
                    covid_copy$symptoms_headache,covid_copy$symptoms_losstastesmell,covid_copy$symptoms_vomit,
                    covid_copy$symptoms_fatigue,covid_copy$symptoms_other)
HaveResponses1=subset(covid_copy, covid_copy$symptoms_cough>0 | covid_copy$symptoms_congestion>0 | covid_copy$symptoms_diarrhea>0 | 
                     covid_copy$symptoms_feverchills>0 | covid_copy$symptoms_musclepain>0 | covid_copy$symptoms_sorethroat>0 | 
                     covid_copy$symptoms_headache>0 | covid_copy$symptoms_losstastesmell>0 | covid_copy$symptoms_vomit>0 | 
                     covid_copy$symptoms_fatigue>0 | covid_copy$symptoms_other>0)


Symptoms <- Symptoms %>%
  rename(Cough = covid_copy.symptoms_cough, Congestion = covid_copy.symptoms_congestion, Diarrhea = covid_copy.symptoms_diarrhea,
         FeverChills = covid_copy.symptoms_feverchills, Musclepain = covid_copy.symptoms_musclepain, Sorethroat = covid_copy.symptoms_sorethroat,
         Headache = covid_copy.symptoms_headache, LossTastesSmell = covid_copy.symptoms_losstastesmell, Vomit = covid_copy.symptoms_vomit,
         Fatigue = covid_copy.symptoms_fatigue, Other = covid_copy.symptoms_other)

Symptoms$Cough = as.numeric(Symptoms$Cough)
Symptoms$Congestion = as.numeric(Symptoms$Congestion)
Symptoms$Diarrhea = as.numeric(Symptoms$Diarrhea)
Symptoms$FeverChills = as.numeric(Symptoms$FeverChills)
Symptoms$Musclepain = as.numeric(Symptoms$Musclepain)
Symptoms$Sorethroat = as.numeric(Symptoms$Sorethroat)
Symptoms$Headache = as.numeric(Symptoms$Headache)
Symptoms$LossTastesSmell = as.numeric(Symptoms$LossTastesSmell)
Symptoms$Vomit = as.numeric(Symptoms$Vomit)
Symptoms$Fatigue = as.numeric(Symptoms$Fatigue)
Symptoms$Other = as.numeric(Symptoms$Other)

c = colSums(Symptoms)
s = sum(Symptoms)
n = nrow(HaveResponses1)

b=round(c/s,3) # Percent of Responses
b

bb=barplot(b, las=1, ylim = c(0,.6), col=c(1,2,3,4,5,6,7), main="Symptoms(Percentage)",ylab="Percentage")
text(bb, b+.02, b, pos=3)

###Create NoVaxReason Bar Chart
NoVaxReason=data.frame(covid_copy$novax_why_naturalimmune,covid_copy$novax_why_notreq,covid_copy$novax_why_sideeffects,
                    covid_copy$novax_why_infertility,covid_copy$novax_why_other)
HaveResponses2=subset(covid_copy, covid_copy$novax_why_naturalimmune>0 | covid_copy$novax_why_notreq>0 | covid_copy$novax_why_sideeffects>0 | 
                                  covid_copy$novax_why_infertility>0 | covid_copy$novax_why_other>0)

NoVaxReason <- NoVaxReason %>%
  rename(NaturalImmune = covid_copy.novax_why_naturalimmune, NoTreq = covid_copy.novax_why_notreq,
         SideEffects = covid_copy.novax_why_sideeffects,Infertility = covid_copy.novax_why_infertility,
         Other = covid_copy.novax_why_other)

NoVaxReason$NaturalImmune = as.numeric(NoVaxReason$NaturalImmune)
NoVaxReason$NoTreq = as.numeric(NoVaxReason$NoTreq)
NoVaxReason$SideEffects = as.numeric(NoVaxReason$SideEffects)
NoVaxReason$Infertility = as.numeric(NoVaxReason$Infertility)
NoVaxReason$Other = as.numeric(NoVaxReason$Other)

c = colSums(NoVaxReason)
s = sum(NoVaxReason)
n = nrow(HaveResponses2)

c=round(c/s,3) # Percent of Responses
c

cc=barplot(c, las=1, ylim = c(0,.6), col=c(2,3,4,5,6), main="Reason for Not Vaccinated(Percentage)",ylab="Percentage")
text(cc, c+.02, c, pos=3)

###Create TestType Bar Chart
TestType=data.frame(covid_copy$test_rapid,covid_copy$test_PCR)
HaveResponses3=subset(covid_copy, covid_copy$test_rapid>0 | covid_copy$test_PCR>0)

TestType <- TestType %>%
  rename(Rapid = covid_copy.test_rapid, PCR = covid_copy.test_PCR)

TestType$Rapid = as.numeric(TestType$Rapid)
TestType$PCR = as.numeric(TestType$PCR)

c = colSums(TestType)
s = sum(TestType)
n = nrow(HaveResponses3)

d=round(c/s,3) # Percent of Responses
d

dd=barplot(d, las=1, ylim = c(0,.8), col=c(3,4), main="Type of Covid Test(Percentage)",ylab="Percentage")
text(dd, d+.02, d, pos=3)



#=========================Let's start running some Regressions!!!=================================

#######################  For when consent_testing = 2 (No) #####################

##### how different reasons of refusal (in rapid testing) relate to their demographics, how people differ 

# first subset the data
noconsentdf <- covid_copy[covid_copy$consent_rapid=="No", ]
summary(noconsentdf)

table(covid_copy$ReasonRefuse_Holiday) # all 0!
table(covid_copy$ReasonRefuse_RecentlyTested) # 0 - 359, 1 - 58 !!!
table(covid_copy$ReasonRefuse_TestedPosIn90) # 1- 3
table(covid_copy$ReasonRefuse_NoSymp) # 1 - 6
table(covid_copy$ReasonRefuse_NoExposure) # 1 - 6
table(covid_copy$ReasonRefuse_PasstiveAttitude) # 1 - 50 !!
table(covid_copy$ReasonRefuse_Vax) # 1 - 5

# look at dataset 
# realized symptom scores all == 0 lol

#run linear multiple regressions
my.lm_passive <- lm(ReasonRefuse_PasstiveAttitude ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_passive) # age & white sig

my.lm_rrtested <- lm(ReasonRefuse_RecentlyTested ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rrtested) # only one race sig

my.lm_rrvax <- lm(ReasonRefuse_Vax ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rrvax) # gender==women sig

my.lm_rrholiday <- lm(ReasonRefuse_Holiday ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rrholiday) # nothing available ?

my.lm_rr0exp <- lm(ReasonRefuse_NoExposure ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rr0exp) # gender==women sig

my.lm_rr0symp <- lm(ReasonRefuse_NoSymp ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rr0symp) # nothing sig

my.lm_rrpos <- lm(ReasonRefuse_TestedPosIn90 ~ age + factor(genderId_coded) + race_StringCoded + insurance, data = noconsentdf) 
summary(my.lm_rrpos) # nothing sig

# no special findings, what about interaction terms?

my.logreg <- glm(ReasonRefuse_PasstiveAttitude ~ age + factor(genderId_coded) + race_StringCoded + insurance + age *race_StringCoded + factor(genderId_coded)*race_StringCoded, data = noconsentdf, family = "binomial")
summary(my.logreg) # for p-values, White Women sig
exp(my.logreg$coefficients) # for odds ratios
exp(confint(my.logreg)) # for confidence intervals on the odds ratios



#######################  For when consent_testing = 1 (Yes) #####################

###IfVaxxed as single categorical variable-- ordinal logistic regression
library(ordinal)

covid_copy$factor_IfVaxed<-factor(covid_copy$IfVaccinated, order=TRUE, 
                                  levels=c("No","1 dose","2 doses","Booster"))
covid_copy$factor_lastTest<-factor(covid_copy$last_test, order=TRUE,
                                   levels=c(1,2,3,4,5,6))

mod1<-clm(factor_IfVaxed~age+factor(genderId_coded)+insurance+factor_lastTest+race_StringCoded, data=covid_copy)
#age*insuarnce -- insurane all missing values
summary(mod1)

#######################  For Test Results Discrepancy #####################
Test_both <- covid_copy[covid_copy$test_PCR==1 & covid_copy$test_rapid==1,]
library(Hmisc)
describe(Test_both$Record.ID)
Test_dif <- Test_both[Test_both$test_rapid_results!=Test_both$test_PCR_results,]
covid_copy$test_dif[covid_copy$test_rapid_results!=covid_copy$test_PCR_results]<-1
covid_copy$test_dif[covid_copy$test_rapid_results==covid_copy$test_PCR_results]<-0

ggplot(covid_copy, 
       aes(x = test_dif, 
           fill = IfVaccinated)) + 
  geom_bar(position = "stack")

ggplot(covid_copy, 
       aes(x = test_dif, 
           fill = vax_brand_booster)) + 
  geom_bar(position = "stack")

logit1 <- glm(test_dif~symp_scores+IfVaccinated, data=covid_copy,family="binomial")
summary(logit1)

logit2 <- glm(test_dif~age+factor(genderId_coded)+insurance+factor_lastTest+race_StringCoded, data=covid_copy,family="binomial")
summary(logit2)

#test_both=113
#test_dif=16 (4 missing the result for PCR test)
#percent_dif=14.16%
#An increase of 1 unit of symptom score increase the odds for different test results by 0.468.
#(estimate=-0.7594, log odds=0.468)




