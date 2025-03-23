#creating dataset
library(readxl)
data_1 <- read_xlsx("C:/Users/Ok/Downloads/Elections16.xlsx")
data_1 <- data_1[-c(1,53, 116, 152, 175, 254, 331, 391 ), ]
data_1<-data_1[c(1:430), -c(20, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47),]
install.packages("dplyr")
library(dplyr)
data_2 <- data_1 %>% select ("0...1",
                             "????? ???????????, ????????? ? ?????? ??????????? ?? ?????? ????????? ???????????",
                             "????? ????????????? ??????????, ???????? ???????????, ??????????????? ????????",
                             "????? ????????????? ??????????, ???????? ? ????????? ??? ??????????? ? ???? ???????????",
                             "????? ????????????? ??????????, ???????? ??? ????????? ??? ??????????? ? ???? ???????????",
                             "????? ???????????????? ????????????? ??????????",
                             "????? ????????????? ??????????, ???????????? ? ?????????? ?????? ??? ???????????",
                             "????? ????????????? ??????????, ???????????? ? ???????????? ?????? ??? ???????????",
                             '0...22',
                             '0...24',
                             '0...26',
                             '0...28',
                             '0...30',
                             '0...32',
                             '0...34',
                             '0...36',
                             '0...38',
                             '0...40', 
                             '0...42',
                             '0...44',
                             '0...46',
                             '0...48')

library(plyr)
data_3 <-rename(data_2, c('0...1'='Commission_Number',
                          '????? ???????????, ????????? ? ?????? ??????????? ?? ?????? ????????? ???????????'='Voters',
                          '????? ????????????? ??????????, ???????? ???????????, ??????????????? ????????'='Ballots_voted_early',
                          '????? ????????????? ??????????, ???????? ? ????????? ??? ??????????? ? ???? ???????????'='Ballots_in_the_station',
                          '????? ????????????? ??????????, ???????? ??? ????????? ??? ??????????? ? ???? ???????????'='Ballots_outside_the_station',
                          '????? ???????????????? ????????????? ??????????'='Invalid_1',
                          '????? ????????????? ??????????, ???????????? ? ?????????? ?????? ??? ???????????'='Ballots_portable',
                          '????? ????????????? ??????????, ???????????? ? ???????????? ?????? ??? ???????????'='Ballots_stationary',
                          '0...22'= 'Rodina',
                          '0...24'= 'Kommunisty_Rossii',
                          '0...26'='Pensionery',
                          '0...28'='United_Russia',
                          '0...30'='Zelenye',
                          '0...32'='Civic_Platform',
                          '0...34'='LDPR',
                          '0...36'='PARNAS',
                          '0...38'='Partiia_Rosta',
                          '0...40'='Civic_Power', 
                          '0...42'='YABLOKO',
                          '0...44'='KPRF',
                          '0...46'='Patriots_of_Russia',
                          '0...48'='A_Just_Russia'))

#get rid of %
data_3$Rodina<-as.numeric(sub("%", "", data_3$Rodina))/100
data_3$Kommunisty_Rossii<-as.numeric(sub("%", "", data_3$Kommunisty_Rossii))/100
data_3$Pensionery<-as.numeric(sub("%", "", data_3$Pensionery))/100
data_3$United_Russia<-as.numeric(sub("%", "", data_3$United_Russia))/100
data_3$Zelenye<-as.numeric(sub("%", "", data_3$Zelenye))/100
data_3$Civic_Platform<-as.numeric(sub("%", "", data_3$Civic_Platform))/100
data_3$LDPR<-as.numeric(sub("%", "", data_3$LDPR))/100
data_3$PARNAS<-as.numeric(sub("%", "", data_3$PARNAS))/100
data_3$Partiia_Rosta<-as.numeric(sub("%", "", data_3$Partiia_Rosta))/100
data_3$Civic_Power<-as.numeric(sub("%", "", data_3$Civic_Power))/100
data_3$YABLOKO<-as.numeric(sub("%", "", data_3$YABLOKO))/100
data_3$KPRF<-as.numeric(sub("%", "", data_3$KPRF))/100
data_3$Patriots_of_Russia<-as.numeric(sub("%", "", data_3$Patriots_of_Russia))/100
data_3$A_Just_Russia<-as.numeric(sub("%", "", data_3$A_Just_Russia))/100

Turnout <- (data_3$Ballots_voted_early+data_3$Ballots_in_the_station+data_3$Ballots_outside_the_station)/data_3$Voters

data_4 <- data.frame(data_3, Turnout)

Invalid <- data_4$Invalid_1/(data_4$Ballots_in_the_station+data_4$Ballots_outside_the_station)


data_5 <- data.frame(data_4, Invalid)


Elections_SP <- data_5 %>% select( Commission_Number, Turnout, Invalid, Rodina, Kommunisty_Rossii, Pensionery, United_Russia, Zelenye, Civic_Platform, LDPR, PARNAS, Partiia_Rosta, Civic_Power, YABLOKO, KPRF, Patriots_of_Russia, A_Just_Russia)

Elections_SP$Turnout <- round(Elections_SP$Turnout, digits=3)
Elections_SP$Invalid <- round(Elections_SP$Invalid, digits=3)
Elections_SP$Rodina <- round(Elections_SP$Rodina, digits=3)
Elections_SP$Kommunisty_Rossii <- round(Elections_SP$Kommunisty_Rossii, digits=3)
Elections_SP$Pensionery <- round(Elections_SP$Pensionery, digits=3)
Elections_SP$United_Russia <- round(Elections_SP$United_Russia, digits=3)
Elections_SP$Zelenye <- round(Elections_SP$Zelenye, digits=3)
Elections_SP$Civic_Platform <- round(Elections_SP$Civic_Platform, digits=3)
Elections_SP$LDPR <- round(Elections_SP$LDPR, digits=3)
Elections_SP$PARNAS <- round(Elections_SP$PARNAS, digits=3)
Elections_SP$Partiia_Rosta <- round(Elections_SP$Partiia_Rosta, digits=3)
Elections_SP$Civic_Power<- round(Elections_SP$Civic_Power, digits=3)
Elections_SP$YABLOKO <- round(Elections_SP$YABLOKO, digits=3)
Elections_SP$KPRF <- round(Elections_SP$KPRF, digits=3)
Elections_SP$Patriots_of_Russia <- round(Elections_SP$Patriots_of_Russia, digits=3)
Elections_SP$A_Just_Russia<- round(Elections_SP$A_Just_Russia, digits=3)

write.csv(Elections_SP,"C:/Users/Ok/Downloads/Elections_SPB.csv", col.names = TRUE, row.names = FALSE )
write.table(Elections_SP, "C:/Users/Ok/Downloads/Elections_SPB.xlsx", quote=FALSE, col.names=TRUE, row.names = FALSE, sep=";",dec=",")
Elections_S<-read.csv("C:/Users/Ok/Downloads/Elections_SPB.csv")

#1

Elections_hist_turnout<-hist(Elections_S$Turnout, main="2016 Elections Histogram",
                  xlab = "Turnout",
                  ylab = "Number of observatons")
Elections_hist_UR<-hist(Elections_S$United_Russia, main="2016 Elections Histogram",
                     xlab = "United Russia's share of votes",
                     ylab = "Number of observatons")
#2
install.packages("ggplot")
library(ggplot2)

#now i have to find out which countries got the most votes
Mod_numeric <- mutate_all(Elections_S, function(x) as.numeric(as.character(x)))
colSums(Mod_numeric)
# from this I see that 5 parties that got most votes are:United Russia, LDPR, KPRF, YABLOKO and Partiia_Rosta

Elections_S$minors <- Elections_S$Rodina + Elections_S$Pensionery + Elections_S$Zelenye + Elections_S$Civic_Platform + Elections_S$PARNAS +
  + Elections_S$Civic_Power + Elections_S$Patriots_of_Russia + Elections_S$A_Just_Russia + Elections_S$Kommunisty_Rossii

library(ggthemes)
plot_1<- ggplot(Elections_S) + geom_boxplot(aes(x='UR', y=United_Russia, fill="United Russia")) + 
  labs (x="Party", y="Share of votes")+
  geom_boxplot(aes(x='CPRF', y=KPRF, fill="CPRF"))  + 
  geom_boxplot(aes(x='LDPR', y=LDPR, fill="LDPR")) + 
  geom_boxplot(aes(x='YABLOKO', y=YABLOKO, fill="YABLOKO"))  +
  geom_boxplot(aes(x='PR', y=Partiia_Rosta, fill="Partiia Rosta"))+
  geom_boxplot(aes(x='Minor parties', y=minors, fill="Minor parties")) +
  theme_hc()
plot_1

#3
Rodina_regr<-lm(Elections_S$Rodina~Elections_S$Turnout)
Kommunisty_Rossii_regr<-lm(Elections_S$Kommunisty_Rossii~Elections_S$Turnout)
Pensionery_regr <- lm(Elections_S$Pensionery~Elections_S$Turnout)
United_Russia_regr <- lm(Elections_S$United_Russia~Elections_S$Turnout)
Zelenye_regr <- lm(Elections_S$Zelenye~Elections_S$Turnout)
Civic_Platform_regr <- lm(Elections_S$Civic_Platform~Elections_S$Turnout)
LDPR_regr <- lm(Elections_S$LDPR~Elections_S$Turnout)
PARNAS_regr <- lm(Elections_S$PARNAS~Elections_S$Turnout)
Partiia_Rosta_regr <- lm(Elections_S$Partiia_Rosta~Elections_S$Turnout)
Civic_Power_regr <- lm(Elections_S$Civic_Power~Elections_S$Turnout)
YABLOKO_regr <- lm(Elections_S$YABLOKO~Elections_S$Turnout)
KPRF_regr <- lm(Elections_S$KPRF~Elections_S$Turnout)
Patriots_of_Russia_regr <- lm(Elections_S$Patriots_of_Russia~Elections_S$Turnout)
A_Just_Russia_regr <- lm(Elections_S$A_Just_Russia~Elections_S$Turnout)

plot(Elections_S$Rodina~Elections_S$Turnout, xlim=c(0,1), ylim=c(0,1), xlab="Turnout", ylab="Share of voters")
title(main = list("Regression for Turnout and share of votes for the party"))
points(Elections_S$Kommunisty_Rossii~Elections_S$Turnout, pch=1, col="red")
points(Elections_S$Pensionery~Elections_S$Turnout, pch=1, col="purple")
points(Elections_S$United_Russia~Elections_S$Turnout, pch=1, col="gray")
points(Elections_S$Zelenye~Elections_S$Turnout, pch=1, col="green")
points(Elections_S$Civic_Platform~Elections_S$Turnout, pch=1, col="yellow")
points(Elections_S$LDPR~Elections_S$Turnout, pch=1, col="orange")
points(Elections_S$PARNAS~Elections_S$Turnout, pch=1, col="blue")
points(Elections_S$Partiia_Rosta~Elections_S$Turnout, pch=1, col="cyan4")
points(Elections_S$Civic_Power~Elections_S$Turnout, pch=1, col="brown")
points(Elections_S$YABLOKO~Elections_S$Turnout, pch=1, col="deeppink")
points(Elections_S$KPRF~Elections_S$Turnout, pch=1, col="aquamarine")
points(Elections_S$Patriots_of_Russia~Elections_S$Turnout, pch=1, col="chartreuse")
points(Elections_S$A_Just_Russia~Elections_S$Turnout, pch=1, col="darkred")


abline(Rodina_regr, lty=1, col = "black")
abline(Kommunisty_Rossii_regr, lty=1, col = "red")
abline(Pensionery_regr, lty=1, col = "purple")
abline(United_Russia_regr, lty=1, col = "gray")
abline(Zelenye_regr, lty=1, col = "green")
abline(Civic_Platform_regr, lty=1, col = "yellow")
abline(LDPR_regr, lty=1, col = "orange")
abline(PARNAS_regr, lty=1, col = "blue")
abline(Partiia_Rosta_regr, lty=1, col = "cyan4")
abline(Civic_Power_regr, lty=1, col = "brown")
abline(YABLOKO_regr, lty=1, col = "deeppink")
abline(KPRF_regr, lty=1, col = "aquamarine")
abline(Patriots_of_Russia_regr, lty=1, col = "chartreuse")
abline(A_Just_Russia_regr, lty=1, col = "darkred")


legend("topleft",lty=c(1,1,1,1,1),
       legend=c("Rodina", "Kommunisty Rossii", "Pensionery", "United Russia", "Zelenye"),cex = 0.6,bty="n",
       col=c("black", "red", "purple", "gray", "green"))

legend("top",lty=c(1,1,1,1,1),
       legend=c("Civic Platform", "LDPR", "PARNAS", "Partiia Rosta", "Civic Power"),cex = 0.6,bty="n",
       col=c("yellow", "orange", "blue", "cyan4", "brown"))

legend("topright",lty=c(1,1,1,1),
       legend=c("YABLOKO", "KPRF", "Patriots of Russia", "A Just Russia"),cex = 0.6,bty="n",
       col=c("deeppink", "aquamarine", "chartreuse", "darkred"))

#4
mod_1 <- lm(Elections_S$United_Russia~Elections_S$Turnout)
summary(mod_1)
mod_2 <- lm(Elections_S$United_Russia~Elections_S$Invalid)
summary(mod_2)
# p-value is smaller in the model with turnout so it can be considered more significant
        
mod_additive <- lm(Elections_S$United_Russia~Elections_S$Turnout+Elections_S$Invalid)
summary(mod_additive)

library(car)

mod_int <- lm(Elections_S$United_Russia~Elections_S$Invalid*Elections_S$Turnout)
summary(mod_int)
# p-value is < 0.05 so there is no interaction effect
#double checking with anova
anova(mod_1, mod_additive, mod_int)
# the test shows that additive model is better than interaction one so there is no interaction effect 

#5
 
#no plot because no interaction effect on the previous step

#6
qqPlot(mod_additive, simulate=TRUE, main="Q-Q Plot")
#not all points on the graph are inside the boundaries of the dashed lines, which indicates the violation of the assumption about the normal distribution of residuals
summary(powerTransform(Elections_S$United_Russia))

#7
crPlots(mod_additive)
attach(Elections_S)
Elect<-subset(Elections_S, Invalid > 0, Turnout > 0)
boxTidwell(Elect$United_Russia~Elect$Turnout+Elect$Invalid)


#1 descriptive statistics
install.packages("stargazer")
library(stargazer)
descriptive_statistics<-data.frame(Elections_S$United_Russia, Elections_S$Turnout, Elections_S$Invalid)

stargazer(descriptive_statistics, 
          type="html", 
          title="Elections 2016 descriptive statistics",
          covariate.labels=c("United Russia","Turnout", "Invalid"), 
          digits=2, 
          summary.stat=c("n", "mean", "sd", "min", "max","median"), 
          out="Elections2016_descriptivestatistics.htm")

#2

stargazer(mod_additive,  
          type="html",
          model.numbers = FALSE,
          title="Elections 2016 table",
          dep.var.labels = c("Percent of votes for United Russia"),
          covariate.labels = c("Percent of turnout", "Percent of invalid ballots"), 
          omit.stat = c("f"),
          out="Elections2016regression.htm")

#3
vif(mod_additive)
#the value is <10 so there is no multicollinearity and therefore no strong correlation between the IVs

ncvTest(mod_additive)
# p-value is smaller than 0.05 so the problem of heteroskedasticity is present in the model 
# therefore I'm building a model with robust (non-biased) standard errors
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)
coeftest(mod_additive, vcov=vcovHC(mod_additive, type="HC3"))

outlierTest(mod_additive)
# there are 6 outliers in the model
Elections_S[144,]
Elections_S[164,]
Elections_S[166,]
Elections_S[140,]
Elections_S[394,]
Elections_S[158,]

plot(mod_additive, which=4, cook.levels=1) 
abline(h=1, lty=2, col="red")
# there are no influential cases in the model as there are no cases in which cook's distance would be greater than 1
mod_int_2 <- lm(Elections_S$United_Russia~Elections_S$Invalid*Elections_S$Turnout)
anova(mod_1, mod_additive, mod_int_2)

#4

install.packages("https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_4.2-1.tar.gz", repos=NULL, type="source")
library(Zelig)
library(zoo)
library(sandwich)
library(AER)

New_1<-with(Elections_S,Invalid/Turnout)
New_2<-ivreg(United_Russia~Turnout+Invalid|Invalid+New_1,
                   data=Elections_S,subset=)
summary(New_2,diagnostics=TRUE)
