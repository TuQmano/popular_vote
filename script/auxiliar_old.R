
###########################################################################################
#####                   TURNOUT USA   /// Electoral College & Mandatory vote          #####      
###########################################################################################

##Needed packages
library(readr)
library(ggplot2)
library(stargazer)
library(reshape2)
library(purrr)
library(tidyr)
library(data.table)  
library(directlabels)
library(MASS)
### Data Managment ####
#Import dBase (.csv)

USATurnout <- read_delim("turnoutLong.csv", ";", 
                         escape_double = FALSE, na = "0",
                         trim_ws = TRUE)




###########
###########
########### UNITED STATES AVERAGE TURNOUT ANALYSIS
###########
###########
###########



#Subset UNITED STATES (Average_Year)
USAavg <- subset(USATurnout, (STATE == "United States"))
#Subset UNITED STATES * State_Year
StatesTurnout<- subset(USATurnout, !(STATE == "United States"))

colnames(USAavg) <- c("Year","State","Turnout","Type")
colnames(StatesTurnout) <- c("Year","State","Turnout","Type")

#####USA AVERAGE
##### Presidential Turnout Rate (VEP - voting elegible peple - vs VAP - voting age people)
##### 
attach(USAavg)
USAavg <- USAavg[!USAavg$Year == 1980, ]  #Delete Obs for a "complete" serie


#Difference between VAP and VEP

VAP <- subset(USAavg, (Type=="VAP"))
VEP <- subset(USAavg, (Type=="VEP"))
USA <- merge(VAP,VEP,by="Year")
USA<- USA[,c("Year","State.x","Turnout.x", "Turnout.y")]
colnames(USA) <- c("Year", "State", "VAP", "VEP")
USA$diffVAP_VEP <-USA$VEP-USA$VAP

#PLOT Diff VAP/VEP


plot_VAP_VEP <- ggplot(USA)+geom_col(aes(Year, diffVAP_VEP))+
  labs(title= "Diferencia VEP - VAP", y="Diferencia (puntos porcentuales)", x="A\u{F1}o") + 
  scale_x_continuous(breaks=seq(1980, 2016, 4))

plot_VAP_VEP 


####Bar Plot  USA mean % TURNOUT
PlotUSAavg <- ggplot(USAavg) + geom_bar(stat="identity", position = "dodge", aes(Year, Turnout, group=Type,fill=Type)) + 
  labs(title= "Participaci\u{F3}n Electoral en Estados Unidos", x="A\u{F1}o", y="%") + 
  scale_fill_grey(start = 0.2, end = 0.5, 
                  breaks=c("VEP", "VAP"),
                  labels=c("Poblaci\u{F3}n en edad para votar (VAP)", "Poblaci\u{F3}n elegible para votar (VEP)")) +
  theme(legend.position = "bottom")+theme(legend.title=element_blank())+
  scale_x_continuous(breaks=seq(1980, 2016, 4))



PlotUSAavg 



### 2016 ELETION ANALYSIS by STATE
###
###
###
###
###
###
###
###
###
#
#                                          REGRESSION ANALYSIS  (Turnout and Results/Forecast)
#
#### SUBSET TURNOUT VEP 2016
#### 

Turnout2016 <- subset(StatesTurnout, Year==2016 & Type=="VEP")
attach(Turnout2016)
Turnout2016$Year=NULL
Turnout2016$Type=NULL



#####RESULTS 2016 IMPORT dB
#####
Results2016 <- read_delim("Results2016.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

#RENAME VARS
colnames(Results2016) <- c("Clinton %", "Trump %", "State", "Clinton538", "Trump538")




##MERGE Data Frames
##
REGdb <- merge(Results2016,Turnout2016,by="State")

##Create VAR "Diff" (between Clinton and Trump) # One "RESULTS" other  "Forecast" & "DiffForecast_Results"
##

attach(REGdb)
REGdb$Diff <- abs(REGdb$`Clinton %`- REGdb$`Trump %`)  # Results margin of victory
REGdb$Diff538 <- abs(REGdb$Clinton538 - REGdb$Trump538) # Forecast margin of victory
REGdb$DiffForecast_Results<- abs(REGdb$Diff - REGdb$Diff538) # Forecast accuracy distance


###PLOT AND REGRESSION
###
###MODEL 1 (SIMPLE)
reg=lm(Turnout ~ Diff538, data = REGdb)
summary(reg)



#####MODEL 2 (with Dummy Var DC =1)
#####
##### Washington DC as a dummy var
REGdb$DC <- 0
REGdb$DC[REGdb$State =="District of Columbia"] <-1


###BoxPlot para ver Outlier (District of Columbia)
BoxPlotDC <- ggplot(REGdb, aes(x = Turnout, y = Diff538))
BoxPlotDC+geom_boxplot() +labs(title= "Outlier: District of Columbia (DC)", y="Diferencia (Clinton - Trump)", x="% Votantes") +
  geom_text(aes(label=ifelse(Diff>75, as.character(State),'')),hjust=.9,vjust=1.9)


#### REGRESION MODEL 2
regDC=lm(Turnout ~ Diff538 + DC, data = REGdb)
summary(regDC)


#####MODEL 3 (add Dummy Var HW =1)
#####
##### Washington DC as a dummy var
REGdb$HW <- 0
REGdb$HW[REGdb$State =="Hawaii"] <-1


###BoxPlot para ver Outlier (Hawaii)
BoxPlotHW <- ggplot(REGdb, aes(x = Diff538, y = Turnout))
BoxPlotHW+geom_boxplot() +labs(title= "Outlier: Hawaii", y="% VotantesDiferencia (Clinton - Trump)", x="Diferencia (Clinton - Trump)") +
  geom_text(aes(label=ifelse(Turnout<50, as.character(State),'')),hjust=0,vjust=0)


#### REGRESION MODEL 3
regHW=lm(Turnout ~ Diff538 + DC + HW, data = REGdb)
summary(regHW)


#PLOT lm 
#
ggplot(regHW, aes(x = Diff538, y = Turnout, group = DC, label=State)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "black")+ 
  labs(title= "Competitividad estadual y nivel de participaci\u{F3}n", x="Diferencia (Clinton - Trump)", y="% Votantes") +
  geom_text(aes(label=ifelse(Diff538>70 |Turnout<50, as.character(REGdb$State),'')),hjust=.9,vjust=1.9)


######
######
#####


#Diff Dems & Rep Statistics ("RESULTS")
AverageDiffDemsRep <- mean(REGdb$Diff) #18.37  -> Diff for DC = 86.41 /// Next to DC Wyoming (45.77)
MeanDiffDemsRep <- median(REGdb$Diff) #17.07


#########TABLES
######### Output -> Code for LaTex

###USA Turnout Average
stargazer(USA) #Table1  - Summary 

stargazer(USA[1:17,], summary=FALSE, rownames=FALSE) #Table2

### Electoral Results 2016
### 
Results2016$Diferencia538 <- abs(Results2016$Clinton538- Results2016$Trump538) #538 diff
Results2016$Diferencia <- abs(Results2016$`Clinton %`- Results2016$`Trump %`)  #results diff


#REORDER COLIMNS
Results2016 <- Results2016[c(1,2,7,3,4,5,6)]

###SORT
Results2016 <- Results2016[order(Results2016$Diferencia538),] #Sort by 538Diff
Results2016 <- Results2016[order(Results2016$Diferencia),] #Sort by Diff RESULTS

### Export Code for LaTex Table
stargazer(Results2016[1:51,], summary=FALSE, rownames=FALSE)




###Linear Models Output Table
## 3OLS models  ######### regHW regression model including two dummy (DC y HI) 

stargazer(reg, regDC, regHW,  title="Resultados", align=TRUE)




########
########
########
######## State Density Plots   REVISAR!!!!!!
######## 

#RESHAPE (long 2 wide -> each State a Variable)
#
#


#Subset (Only VEP obs)
StatesTurnoutWIDE <- subset(StatesTurnout, (Type == "VEP"))
StatesTurnoutWIDE$Type=NULL 
StatesTurnoutWIDE$Year=NULL 

#RESHAPE TO WIDE (dcast)
StatesTurnoutWIDE<- dcast(StatesTurnout,  Year  ~ State, value.var = "Turnout", mean)


StatesTurnoutWIDE$`United States (Excl, Louisiana)`=NULL  #Delete column different form State

StatesTurnoutWIDE$Year=NULL 

############## Create MEAN for each State
############## 
############## 



#### mean(USATournout)
NationalMean <- mean(USAavg$Turnout)  #46.82

colnames(StatesTurnoutWIDE)[9] <- "DC" #rename DC
### Facets for each state
PLOTDensity <- StatesTurnoutWIDE %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free_y", ncol = 5) +
  geom_density( fill = "red") +
  geom_vline(xintercept = 46.82) +
  theme(strip.text.x = element_text(size=11, face="bold")) +
  labs(title= toupper("Densidad del nivel de participacion por estado"),
       x=" % Participaci\u{F3}n", y= "Densidad") +
  ggthemes::theme_fivethirtyeight()

PLOTDensity





