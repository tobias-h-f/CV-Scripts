
'''
This is my first R-script written in spring 2020. Its messy and huge bunches of code could be 
spared if apply functions had been used (ex. when changing variable type to factors)

The code starts with about 400 lines of data cleansing and continues with GLM and RF models.




'''








rm(list = ls(all.names=TRUE))
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(tree)
library(leaflet)
library(ggmap)
library(forcats)
library(caTools)
library(caret)
library(pROC)


NYC <- read.csv("NYPD_Complaint_Data_Historic.csv")
NY <- select(NYC, CMPLNT_NUM, CMPLNT_FR_TM, CMPLNT_FR_DT,
             ADDR_PCT_CD, PREM_TYP_DESC, PARKS_NM, Latitude,
             Longitude, BORO_NM, STATION_NAME,SUSP_AGE_GROUP,
             VIC_AGE_GROUP, VIC_RACE, VIC_SEX, OFNS_DESC,
             LAW_CAT_CD)
rm(NYC)
#NY_test
NY_test <- NY
#Omkodning af afhængig variabele
NY_test<- filter(NY_test,
                  OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES"|OFNS_DESC == "BURGLARY"
                 |OFNS_DESC == "DANGEROUS DRUGS"
                 |OFNS_DESC == "DANGEROUS WEAPONS"|OFNS_DESC == "FELONY ASSAULT"
                 |OFNS_DESC == " GRAND LARCENY"|OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE"
                 |OFNS_DESC == "KIDNAPPING"|OFNS_DESC == "KIDNAPPING & RELATED OFFENSES"
                 |OFNS_DESC == "KIDNAPPING AND RELATED OFFENSES"|OFNS_DESC == "MISCELLANEOUS PENAL LAW"
                 |OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER"|OFNS_DESC == "OFF. AGNST PUB ORD SENSBLTY &"
                 |OFNS_DESC == "OFFENSES AGAINST PUBLIC SAFETY"|OFNS_DESC == "OFFENSES AGAINST THE PERSON"
                 |OFNS_DESC == "OTHER OFFENSES RELATED TO THEF"|OFNS_DESC == "PETIT LARCENY"
                 |OFNS_DESC == "PETIT LARCENY OF MOTOR VEHICLE" |OFNS_DESC == "RAPE"|OFNS_DESC == "ROBBERY"
                 |OFNS_DESC == "SEX CRIMES"|OFNS_DESC == "THEFT-FRAUD"|OFNS_DESC == "THEFT OF SERVICES")
NY_test$OFNS_DESC <- droplevels(NY_test$OFNS_DESC)
NY_test <- NY_test %>%
  mutate(Offense = fct_collapse(OFNS_DESC,
                                Burglary = c("BURGLARY") ,
                                Larceny = c("GRAND LARCENY", "OTHER OFFENSES RELATED TO THEF",
                                            "PETIT LARCENY", "THEFT-FRAUD","THEFT OF SERVICES", "PETIT LARCENY OF MOTOR VEHICLE") ,
                                Assault = c("ASSAULT 3 & RELATED OFFENSES", "FELONY ASSAULT",
                                            "OFFENSES AGAINST THE PERSON") ,
                                CarTheft = c("GRAND LARCENY OF MOTOR VEHICLE") ,
                                Drugs_n_guns= c("DANGEROUS DRUGS", "DANGEROUS WEAPONS") ,
                                SexualAssault = c("RAPE", "SEX CRIMES") ,
                                Robbery = c("ROBBERY") ,
                                Kidnapping= c("KIDNAPPING", "KIDNAPPING & RELATED OFFENSES",
                                              "KIDNAPPING AND RELATED OFFENSES") ,
                                PublicOrder = c("MISCELLANEOUS PENAL LAW", "OFF. AGNST PUB ORD SENSBLTY &",
                                                "OFFENSES AGAINST PUBLIC SAFETY") ,
                                Murder = c("MURDER & NON-NEGL. MANSLAUGHTER")
  ))
NY_test <- NY_test %>%
  mutate(Personfarlig = fct_collapse(Offense,
                                     "1" = c("Assault", "Kidnapping", "SexualAssault", "Murder", "Robbery"),
                                     "0" = c("Burglary", "Larceny", "CarTheft", "Drugs_n_guns", "PublicOrder")
  ))
NY_test <- NY_test %>%
  mutate(Berigelse = fct_collapse(Offense,
                                  "1" = c("Burglary", "Larceny", "CarTheft"),
                                  "0" = c("Assault", "Kidnapping", "SexualAssault",
                                          "Murder", "Robbery", "Drugs_n_guns", "PublicOrder")
  ))
NY_test <- NY_test %>%
  mutate(Utryghed = fct_collapse(Offense,
                                 "1" = c("Drugs_n_guns", "PublicOrder"),
                                 "0" = c("Assault", "Kidnapping", "SexualAssault",
                                         "Murder", "Robbery", "Burglary", "Larceny", 
                                         "CarTheft")
  ))
#Fjerner ekstreme koordinater
NY_test <- NY_test %>%
  filter(Latitude > 40.39999 |Latitude < 40.920001 )
NY_test <- NY_test %>%
  filter(Longitude > -74.25999 |Longitude < -73.70001 )
NY_test <- NY_test[-c(264507, 777439, 823684, 516187, 837259, 483515, 705736, 829566, 827905, 2059026), ]

#Dato og tid ind i korrekt form, og dato splittes op i år, måned og dag
NY_test$Time<- hms(NY_test$CMPLNT_FR_TM)
NY_test$Hour <- strptime(NY_test$Time, "%H")
NY_test$Hour <- as.character((NY_test$Hour))
NY_test$Hour[is.na(NY_test$Hour) == TRUE] <- 200
NY_test$Hour <- as.factor(NY_test$Hour)
NY_test <- NY_test %>%
  filter(!Hour == 200 )
NY_test$Hour <- hour(NY_test$Hour)
NY_test$Date <- mdy(NY_test$CMPLNT_FR_DT)
NY_test$Year <- year(NY_test$Date)
NY_test$Month <- month(NY_test$Date)
NY_test$Day <- day(NY_test$Date)
NY_test1 <- NY_test %>%
  filter(Year == 2017 )

#District
NY_test1$ADDR_PCT_CD <- as.factor(NY_test1$ADDR_PCT_CD)
NY_test1$District <- NY_test1$ADDR_PCT_CD
NY_test1data <- select(NY_test1, Personfarlig, Berigelse, Utryghed,
                       Hour, Month, Day,
                       District
)

#Laver geografisk datas?t
NY_test1$Neighbourhood <- NY_test1$BORO_NM
District <- NY_test1%>%
  select(District, Neighbourhood, Year, Month, Day, Hour, Date)
District <- District[!duplicated(District$District),]
District$Neighbourhood[District$District==106] <- "QUEENS"
District$Neighbourhood[District$District==49] <- "BRONX"
District$Neighbourhood[District$District==28] <- "MANHATTAN"
District$Neighbourhood[District$District==63] <- "BROOKLYN"
District$Neighbourhood[District$District==81] <- "BROOKLYN"
District$Neighbourhood[District$District==83] <- "BROOKLYN"
District_Personfarlig2 <- filter(District_Personfarlig2, District != "22")
District <- District%>%
  filter(Neighbourhood == "QUEENS"|Neighbourhood =="BRONX"|Neighbourhood =="MANHATTAN")
District <-crossing( expand(District, nesting( District, Neighbourhood, Year), Month = 1:12, Day = 1:31, Hour = 0:23))
District$Date <- ISOdate(District$Year, District$Month, District$Day)
District <- na.omit(District)
District$Weekday <- wday(District$Date)

#Daylight
load("NY_daylight.Rda")
NY_daylight <- NY_daylight.Rda

NY_daylight <- mutate(NY_daylight, Solop = `1...1`, Solned = `1...2`)
NY_daylight <- separate(NY_daylight, col = Solop, into = c("Solop", "a", "b"), sep=" ")
NY_daylight <- separate(NY_daylight, col = Solned, into = c("Solned", "a", "b"), sep=" ")
NY_daylight <- NY_daylight %>%
  select(Solop,Solned, Month, Day)
NY_daylight$Solop <- hm(NY_daylight$Solop)
NY_daylight$Solned <- hm(NY_daylight$Solned)
District$Hour1 <- hours(District$Hour)
NY_daylight$Month <- as.integer(NY_daylight$Month)
NY_daylight$Day <- as.integer(NY_daylight$Day)
District <- left_join(District, NY_daylight, by=c("Month" = "Month", "Day" = "Day"))
District <- mutate(District, Sun = Hour1 > Solop & Hour1 < Solned)
District <- District%>%
  select(-Hour1, -Solned, -Solop, -Date)

#Laver datasæt til personfarlig
NY_Personfarlig <- NY_test1data %>%
  filter(Personfarlig == 1)
District_Personfarlig <- District %>%
  left_join(NY_Personfarlig, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Personfarlig$Personfarlig <- as.character((District_Personfarlig$Personfarlig))
District_Personfarlig$Personfarlig[is.na(District_Personfarlig$Personfarlig) == TRUE] <- 0
District_Personfarlig$Personfarlig <- as.factor(District_Personfarlig$Personfarlig)
District_Personfarlig <- District_Personfarlig %>%
  select(-Berigelse, - Utryghed)

#Laver datasæt til berigelse
NY_Berigelse <- NY_test1data %>%
  filter(Berigelse == 1)
District_Berigelse <- District %>%
  left_join(NY_Berigelse, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Berigelse$Berigelse <- as.character((District_Berigelse$Berigelse))
District_Berigelse$Berigelse[is.na(District_Berigelse$Berigelse) == TRUE] <- 0
District_Berigelse$Berigelse <- as.factor(District_Berigelse$Berigelse)

#Fjerner ikke brugbare variable
District_Berigelse <- District_Berigelse %>%
  select(-Personfarlig, - Utryghed)

#Laver datasæt til utryghed
NY_Utryghed <- NY_test1data %>%
  filter(Utryghed == 1)
District_Utryghed <- District %>%
  left_join(NY_Utryghed, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Utryghed$Utryghed <- as.character((District_Utryghed$Utryghed))
District_Utryghed$Utryghed[is.na(District_Utryghed$Utryghed) == TRUE] <- 0
District_Utryghed$Utryghed <- as.factor(District_Utryghed$Utryghed)

#Fjerner ikke brugbare variable
District_Utryghed <- District_Utryghed %>%
  select(-Personfarlig, - Berigelse)

#Skaber test-datasæt
NY_test2 <- NY_test %>%
  filter(Year == 2018 )

#District
NY_test2$ADDR_PCT_CD <- as.factor(NY_test2$ADDR_PCT_CD)
NY_test2$District <- NY_test2$ADDR_PCT_CD 

NY_test2data <- select(NY_test2, Personfarlig, Berigelse, Utryghed,
                       Hour, Month, Day,
                       District
)

#Laver geografisk datasæt
NY_test2$Neighbourhood <- NY_test2$BORO_NM
District <- NY_test2%>%
  select(District, Neighbourhood, Year, Month, Day, Hour, Date)
District <- District[!duplicated(District$District),]
District$Neighbourhood[District$District==106] <- "QUEENS"
District$Neighbourhood[District$District==49] <- "BRONX"
District$Neighbourhood[District$District==28] <- "MANHATTAN"
District$Neighbourhood[District$District==63] <- "BROOKLYN"
District$Neighbourhood[District$District==81] <- "BROOKLYN"
District$Neighbourhood[District$District==83] <- "BROOKLYN"
District <- District%>%
  filter(Neighbourhood == "QUEENS"|Neighbourhood =="BRONX"|Neighbourhood =="MANHATTAN")
District <-crossing( expand(District, nesting( District, Neighbourhood, Year), Month = 1:12, Day = 1:31, Hour = 0:23))
District$Date <- ISOdate(District$Year, District$Month, District$Day)
District <- na.omit(District)
District$Weekday <- wday(District$Date)

#Daylight
load("NY_daylight.Rda")
NY_daylight <- NY_daylight.Rda
NY_daylight <- mutate(NY_daylight, Solop = `1...1`, Solned = `1...2`)
NY_daylight <- separate(NY_daylight, col = Solop, into = c("Solop", "a", "b"), sep=" ")
NY_daylight <- separate(NY_daylight, col = Solned, into = c("Solned", "a", "b"), sep=" ")
NY_daylight <- NY_daylight %>%
  select(Solop,Solned, Month, Day)
NY_daylight$Solop <- hm(NY_daylight$Solop)
NY_daylight$Solned <- hm(NY_daylight$Solned)
District$Hour1 <- hours(District$Hour)
NY_daylight$Month <- as.integer(NY_daylight$Month)
NY_daylight$Day <- as.integer(NY_daylight$Day)
District <- left_join(District, NY_daylight, by=c("Month" = "Month", "Day" = "Day"))
District <- mutate(District, Sun = Hour1 > Solop & Hour1 < Solned)
District <- District%>%
  select(-Hour1, -Solned, -Solop, -Date)

#Laver datasæt til personfarlig
NY_Personfarlig <- NY_test2data %>%
  filter(Personfarlig == 1)
District_Personfarlig_test <- District %>%
  left_join(NY_Personfarlig, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Personfarlig_test$Personfarlig <- as.character((District_Personfarlig_test$Personfarlig))
District_Personfarlig_test$Personfarlig[is.na(District_Personfarlig_test$Personfarlig) == TRUE] <- 0
District_Personfarlig_test$Personfarlig <- as.factor(District_Personfarlig_test$Personfarlig)
District_Personfarlig_test <- District_Personfarlig_test %>%
  select(-Berigelse, - Utryghed)

#Laver datasæt til berigelse
NY_Berigelse <- NY_test2data %>%
  filter(Berigelse == 1)
District_Berigelse_test <- District %>%
  left_join(NY_Berigelse, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Berigelse_test$Berigelse <- as.character((District_Berigelse_test$Berigelse))
District_Berigelse_test$Berigelse[is.na(District_Berigelse_test$Berigelse) == TRUE] <- 0
District_Berigelse_test$Berigelse <- as.factor(District_Berigelse_test$Berigelse)

#Fjerner ikke brugbare variable
District_Berigelse_test <- District_Berigelse_test %>%
  select(-Personfarlig, - Utryghed)

#Laver datasæt til utryghed
NY_Utryghed <- NY_test2data %>%
  filter(Utryghed == 1)
District_Utryghed_test <- District %>%
  left_join(NY_Utryghed, by=c("District" = "District", "Month"="Month", "Day" = "Day", "Hour"="Hour"))

#Erstatter missing med 0 i de afh?ngige
District_Utryghed_test$Utryghed <- as.character((District_Utryghed_test$Utryghed))
District_Utryghed_test$Utryghed[is.na(District_Utryghed_test$Utryghed) == TRUE] <- 0
District_Utryghed_test$Utryghed <- as.factor(District_Utryghed_test$Utryghed)

#Fjerner ikke brugbare variable og datasæt
District_Utryghed_test <- District_Utryghed_test %>%
  select(-Personfarlig, - Berigelse)
rm(District)
rm(NY)
rm(NY_Berigelse)
rm(NY_daylight)
rm(NY_Personfarlig)
rm(NY_Utryghed)
rm(NY_test)
rm(NY_test1)
rm(NY_test2)
rm(NY_test1data)
rm(NY_test2data)

#Omkoder afh?ngige variable
droplevels(District_Berigelse$Neighbourhood)
droplevels(District_Berigelse_test$Neighbourhood)
droplevels(District_Personfarlig$Neighbourhood)
droplevels(District_Personfarlig_test$Neighbourhood)
droplevels(District_Utryghed_test$Neighbourhood)
droplevels(District_Utryghed_test$Neighbourhood)

District_Berigelse$Year <- as.factor(District_Berigelse$Year)
District_Berigelse$Month <- as.factor(District_Berigelse$Month)
District_Berigelse$Day <- as.factor(District_Berigelse$Day)
District_Berigelse$Hour <- as.factor(District_Berigelse$Hour)
District_Berigelse$Weekday <- as.factor(District_Berigelse$Weekday)
District_Berigelse_test$Year <- as.factor(District_Berigelse_test$Year)
District_Berigelse_test$Month <- as.factor(District_Berigelse_test$Month)
District_Berigelse_test$Day <- as.factor(District_Berigelse_test$Day)
District_Berigelse_test$Hour <- as.factor(District_Berigelse_test$Hour)
District_Berigelse_test$Weekday <- as.factor(District_Berigelse_test$Weekday)
District_Personfarlig$Year <- as.factor(District_Personfarlig$Year)
District_Personfarlig$Month <- as.factor(District_Personfarlig$Month)
District_Personfarlig$Day <- as.factor(District_Personfarlig$Day)
District_Personfarlig$Hour <- as.factor(District_Personfarlig$Hour)
District_Personfarlig$Weekday <- as.factor(District_Personfarlig$Weekday)
District_Personfarlig <- District_Personfarlig %>%
  select(-Date, -Hour1)
District_Personfarlig_test$Year <- as.factor(District_Personfarlig_test$Year)
District_Personfarlig_test$Month <- as.factor(District_Personfarlig_test$Month)
District_Personfarlig_test$Day <- as.factor(District_Personfarlig_test$Day)
District_Personfarlig_test$Hour <- as.factor(District_Personfarlig_test$Hour)
District_Personfarlig_test$Weekday <- as.factor(District_Personfarlig_test$Weekday)
District_Utryghed$Year <- as.factor(District_Utryghed$Year)
District_Utryghed$Month <- as.factor(District_Utryghed$Month)
District_Utryghed$Day <- as.factor(District_Utryghed$Day)

District_Utryghed$Hour <- as.factor(District_Utryghed$Hour)
District_Utryghed$Weekday <- as.factor(District_Utryghed$Weekday)
District_Utryghed_test$Year <- as.factor(District_Utryghed_test$Year)
District_Utryghed_test$Month <- as.factor(District_Utryghed_test$Month)
District_Utryghed_test$Day <- as.factor(District_Utryghed_test$Day)
District_Utryghed_test$Hour <- as.factor(District_Utryghed_test$Hour)
District_Utryghed_test$Weekday <- as.factor(District_Utryghed_test$Weekday)
District_Berigelse <- District_Berigelse%>%
  mutate(Berigelse = fct_recode(Berigelse,
                                "Yes" = "1",
                                "No" = "0"
  ))
District_Berigelse_test <- District_Berigelse_test%>%
  mutate(Berigelse = fct_recode(Berigelse,
                                "Yes" = "1",
                                "No" = "0"
  ))
District_Personfarlig <- District_Personfarlig%>%
  mutate(Personfarlig = fct_recode(Personfarlig,
                                   "Yes" = "1",
                                   "No" = "0"
  ))
District_Personfarlig_test <- District_Personfarlig_test%>%
  mutate(Personfarlig = fct_recode(Personfarlig,
                                   "Yes" = "1",
                                   "No" = "0"
  ))
District_Utryghed <- District_Utryghed%>%
  mutate(Utryghed = fct_recode(Utryghed,
                               "Yes" = "1",
                               "No" = "0"
  ))
District_Utryghed_test <- District_Utryghed_test%>%
  mutate(Utryghed = fct_recode(Utryghed,
                               "Yes" = "1",
                               "No" = "0"
  ))

##Analyse
District_Berigelse <- District_Berigelse %>%
  select(-Year)
District_Berigelse_test <- District_Berigelse_test %>%
  select(-Year)
District_Personfarlig <- District_Personfarlig %>%
  select(-Year)
District_Personfarlig_test <- District_Personfarlig_test %>%
  select( -Year)
District_Utryghed <- District_Utryghed %>%
  select(-Year)
District_Utryghed_test <- District_Utryghed_test %>%
  select(-Year)

# GLM modeller
District_Personfarlig_test <- District_Personfarlig_test %>%
  filter(!District == 102 )
District_Personfarlig_test <- District_Personfarlig_test %>%
  filter(!District == 50 )
District_Personfarlig_test$District <- droplevels(District_Personfarlig_test$District)
logit_District_Personfarlig <- glm(Personfarlig ~., family = "binomial", data=District_Personfarlig)
save(logit_District_Personfarlig, file= "logit_District_Personfarlig.Rda")
District_Utryghed_test <- District_Utryghed_test %>%
  filter(!District == 102 )
District_Utryghed_test <- District_Utryghed_test %>%
  filter(!District == 50 )
District_Utryghed_test$District <- droplevels(District_Utryghed_test$District)
logit_District_Utryghed <- glm(Utryghed ~., family = "binomial", data=District_Utryghed)
save(logit_District_Utryghed, file= "logit_District_Utryghed.Rda")

District_Berigelse_test <- District_Berigelse_test %>%
  filter(!District == 102 )
District_Berigelse_test <- District_Berigelse_test %>%
  filter(!District == 50 )
District_Berigelse_test$District <- droplevels(District_Berigelse_test$District)
logit_District_Berigelse <- glm(Berigelse ~., family = "binomial", data=District_Berigelse)
save(logit_District_Berigelse, file= "logit_District_Berigelse.Rda")


#GLM Analyse
Pred_Personfarlig <- predict(object = logit_District_Personfarlig,
                             newdata = District_Personfarlig_test,
                             type = "response")
glm.Personfarlig <- rep("No",412759)
glm.Personfarlig[Pred_Personfarlig>0.2]="Yes"
District_Personfarlig_test$Personfarlig <- as.factor(District_Personfarlig_test$Personfarlig)
Personfarlig_class <- factor(glm.Personfarlig, levels = levels(District_Personfarlig_test[["Personfarlig"]]))
confusionMatrix(Personfarlig_class, District_Personfarlig_test[["Personfarlig"]], positive = "Yes")
Pred_Utryghed <- predict(object = logit_District_Utryghed,
                         newdata = District_Utryghed_test,
                         type = "response")
glm.Utryghed <- rep("No",405751)
glm.Utryghed[Pred_Utryghed>0.2]="Yes"
District_Utryghed_test$Utryghed <- as.factor(District_Utryghed_test$Utryghed)
Utryghed_class <- factor(glm.Utryghed, levels = levels(District_Utryghed_test[["Utryghed"]]))
confusionMatrix(Utryghed_class, District_Utryghed_test[["Utryghed"]], positive = "Yes")
Pred_Berigelse <- predict(object = logit_District_Berigelse,
                          newdata = District_Berigelse_test,
                          type = "response")
glm.Berigelse <- rep("No",420245)
glm.Berigelse[Pred_Berigelse>0.2]="Yes"
District_Berigelse_test$Berigelse <- as.factor(District_Berigelse_test$Berigelse)
Berigelse_class <- factor(glm.Berigelse, levels = levels(District_Berigelse_test[["Berigelse"]]))
confusionMatrix(Berigelse_class, District_Berigelse_test[["Berigelse"]], positive = "Yes")

#Logistisk regression

myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)
myGrid <- expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20))
memory.limit(1000000)

Lm_Berigelse <- train(
  Berigelse~.,
  District_Berigelse,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl,
  preProcess = c("center", "scale", "zv"),
  metric = "ROC",
  importance = "impurity"
)
save(Lm_Berigelse, file = "Lm_berigelse.Rda")
Lm_Personfarlig <- train(
  Personfarlig~.,
  District_Personfarlig,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl,
  preProcess = c("center", "scale", "zv"),
  metric = "ROC",
  
  importance = "impurity"
)
save(Lm_Personfarlig, file = "Lm_Personfarlig.Rda")
Lm_Utryghed <- train(
  Utryghed~.,
  District_Utryghed,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl,
  preProcess = c("center", "scale", "zv"),
  metric = "ROC",
  importance = "impurity"
)
save(Lm_Utryghed, file = "Lm_Utryghed.Rda")

#Random Forest
myGrid2<- data.frame(
  .mtry = c(40, 50, 60),
  .splitrule = "gini",
  .min.node.size = 5
)
Rf_Berigelse <- train(
  Berigelse~.,
  District_Berigelse,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
  ),
  tuneGrid = myGrid2,
  importance = "impurity",
  respect.unordered.factors= TRUE
)
save(Rf_Berigelse, file = "Rf_Berigelse.Rda")
Rf_Personfarlig <- train(
  Personfarlig~.,
  District_Personfarlig,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
  ),
  tuneGrid = myGrid2,
  importance = "impurity",
  respect.unordered.factors= TRUE
)
save(Rf_Personfarlig, file = "Rf_Personfarlig.Rda")
Rf_Utryghed <- train(
  Utryghed~.,
  District_Utryghed,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
  ),
  tuneGrid = myGrid2,
  importance = "impurity",
  respect.unordered.factors= TRUE
)
save(Rf_Utryghed, file = "Rf_Utryghed.Rda")


#Personfarlig LM_testsæt

Pred_Lm_Personfarlig <- predict(object = Lm_Personfarlig,
                                newdata = District_Personfarlig_test,
                                type = "prob")
Pred_Lm_Personfarlig <- as.data.frame(Pred_Lm_Personfarlig)
Lm_Personfarlig_roc <- roc(District_Personfarlig_test[["Personfarlig"]], Pred_Lm_Personfarlig$`Yes`)
coords(Lm_Personfarlig_roc, "best","threshold", best.weights = c(5 , 0.2))
class_log_Lm_Personfarlig <- ifelse(Pred_Lm_Personfarlig$`Yes` > 0.1374592 , "Yes", "No")
District_Personfarlig_test$Personfarlig <- as.factor(District_Personfarlig_test$Personfarlig)
Lm_Personfarlig_class <- factor(class_log_Lm_Personfarlig, levels = levels(District_Personfarlig_test[["Personfarlig"]]))
confusionMatrix(Lm_Personfarlig_class, District_Personfarlig_test[["Personfarlig"]], positive = "Yes")

#Berigelse LM_testsæt
Pred_Lm_Berigelse <- predict(object = Lm_Berigelse,
                             newdata = District_Berigelse_test,
                             type = "prob")
Pred_Lm_Berigelse <- as.data.frame(Pred_Lm_Berigelse)
Lm_Berigelse_roc <- roc(District_Berigelse_test[["Berigelse"]], Pred_Lm_Berigelse$`Yes`)
coords(Lm_Berigelse_roc, "best","threshold",best.weights = c(5 , 0.2))
class_log_Lm_Berigelse <- ifelse(Pred_Lm_Berigelse$`Yes` > 0.1958723, "Yes", "No")
District_Berigelse_test$Berigelse <- as.factor(District_Berigelse_test$Berigelse)
Lm_Berigelse_class <- factor(class_log_Lm_Berigelse, levels = levels(District_Berigelse_test[["Berigelse"]]))
confusionMatrix(Lm_Berigelse_class, District_Berigelse_test[["Berigelse"]], positive = "Yes")

# Utryghed LM_testsæt
Pred_Lm_Utryghed <- predict(object = Lm_Utryghed,
                            newdata = District_Utryghed_test,
                            type = "prob")
Pred_Lm_Utryghed <- as.data.frame(Pred_Lm_Utryghed)
Lm_Utryghed_roc <- roc(District_Utryghed_test[["Utryghed"]], Pred_Lm_Utryghed$`Yes`)
coords(Lm_Utryghed_roc, "best","threshold",best.weights = c(5 , 0.2))
class_log_Lm_Utryghed <- ifelse(Pred_Lm_Utryghed$`Yes` > 0.07039285, "Yes", "No")
District_Utryghed_test$Utryghed <- as.factor(District_Utryghed_test$Utryghed)
Lm_Utryghed_class <- factor(class_log_Lm_Utryghed, levels = levels(District_Utryghed_test[["Utryghed"]]))
confusionMatrix(Lm_Utryghed_class, District_Utryghed_test[["Utryghed"]], positive = "Yes")

#Utryghed RF_tests?t
Pred_Rf_Utryghed <- predict(object = Rf_Utryghed,
                            newdata = District_Utryghed_test,
                            type = "prob")
Pred_Rf_Utryghed <- as.data.frame(Pred_Rf_Utryghed)
Rf_Utryghed_roc <- roc(District_Utryghed_test[["Utryghed"]], Pred_Rf_Utryghed$`Yes`)
coords(Rf_Utryghed_roc, "best","threshold",best.weights = c(5 , 0.2))
class_log_Rf_Utryghed <- ifelse(Pred_Rf_Utryghed$`Yes` > .0266812, "Yes", "No")
District_Utryghed_test$Utryghed <- as.factor(District_Utryghed_test$Utryghed)
Rf_Utryghed_class <- factor(class_log_Rf_Utryghed, levels = levels(District_Utryghed_test[["Utryghed"]]))
confusionMatrix(Rf_Utryghed_class, District_Utryghed_test[["Utryghed"]], positive = "Yes")

#Berigelse RF_tests?t
Pred_Rf_Berigelse <- predict(object = Rf_Berigelse,
                             newdata = District_Berigelse_test,
                            
                             type = "prob")
Pred_Rf_Berigelse <- as.data.frame(Pred_Rf_Berigelse)
Rf_Berigelse_roc <- roc(District_Berigelse_test[["Berigelse"]], Pred_Rf_Berigelse$`Yes`)
coords(Rf_Berigelse_roc, "best","threshold",best.weights = c(5 , 0.2))
class_log_Rf_Berigelse <- ifelse(Pred_Rf_Berigelse$`Yes` > .1235529, "Yes", "No")
District_Berigelse_test$Berigelse <- as.factor(District_Berigelse_test$Berigelse)
Rf_Berigelse_class <- factor(class_log_Rf_Berigelse, levels = levels(District_Berigelse_test[["Berigelse"]]))
confusionMatrix(Rf_Berigelse_class, District_Berigelse_test[["Berigelse"]], positive = "Yes")

#Personfarlig RF_tests?t
Pred_Rf_Personfarlig <- predict(object = Rf_Personfarlig,
                                newdata = District_Personfarlig_test,
                                type = "prob")
Pred_Rf_Personfarlig <- as.data.frame(Pred_Rf_Personfarlig)
Rf_Personfarlig_roc <- roc(District_Personfarlig_test[["Personfarlig"]], Pred_Rf_Personfarlig$`Yes`)
coords(Rf_Personfarlig_roc, "best","threshold",best.weights = c(5 , 0.2))
class_log_Rf_Personfarlig <- ifelse(Pred_Rf_Personfarlig$`Yes` > .1374592, "Yes", "No")
District_Personfarlig_test$Personfarlig <- as.factor(District_Personfarlig_test$Personfarlig)
Rf_Personfarlig_class <- factor(class_log_Rf_Personfarlig, levels = levels(District_Personfarlig_test[["Personfarlig"]]))
confusionMatrix(Rf_Personfarlig_class, District_Personfarlig_test[["Personfarlig"]], positive = "Yes")

Importanceplot
Imp_B <- varImp(Rf_Berigelse)
plot(Imp_B, 70)
Imp_P <- varImp(Rf_Personfarlig)
plot(Imp_P, 70)
Imp_U <- varImp(Rf_Utryghed)
plot(Imp_U, 70)