
#                                           
#  COVID-19 Cases by State Study, comparing Democratic and Republican States                  
#  Class project for MTU 5701 Fall 2020     
#  by Lorenzo Gordon                        
#     Owen Applequist                       
#     Kasia Krueger                         
#                                           
 
########################################
#                                      #
# Analysis by Trifecta                 #
#                                      #
########################################


# Install tidyverse library 
#install.packages("tidyverse")
library(tidyverse)

# Install qqpubr library
#install.packages("ggpubr")
library(ggpubr)

# Import Excel spredsheet 
library(readxl)
CovidByState = read_excel(file.choose())

# Strip state code from State variable and add StateCode variable to dataset
StateCode = str_split(CovidByState$State, " - ", simplify=TRUE)[,2]
CovidByState = cbind(CovidByState, StateCode)

# Calculate state population as a percentage of U.S. population and 
# add StatePopulationPercent to dataset
USpopulation = sum(CovidByState$Population)
StatePopulationPercent = CovidByState$Population/USpopulation
CovidByState = cbind(CovidByState, StatePopulationPercent)

# Compute trifecta of Governor, upper and lower state chambers and 
# add Trifecta to dataset
Trifecta = (CovidByState$"Governor's Party" == CovidByState$"Upper State Chamber" & 
    CovidByState$"Governor's Party" == CovidByState$"Lower State Chamber") 
for (i in 1:51) {
    if (Trifecta[i] == TRUE) {
      Trifecta[i] = CovidByState$"Governor's Party"[i]
    } else {
      Trifecta[i] = "Divided"
    }
}
CovidByState = cbind(CovidByState, Trifecta)

# Print list of states by Trifecta
DemocratTriSt = CovidByState$StateCode[Trifecta == 'Democrat']
RepublicanTriSt = CovidByState$StateCode[Trifecta == 'Republican']
DividedTriSt = CovidByState$StateCode[Trifecta == 'Divided']
DemocratTriStList = paste(DemocratTriSt, collapse = " ")
RepublicanTriStList = paste(RepublicanTriSt, collapse = " ")
DividedTriStList = paste(DividedTriSt, collapse = " ")

Parties = c("Democrat", "Divided", "Republican")
States = c(DemocratTriStList, DividedTriStList, RepublicanTriStList)

report = cbind(Parties, States)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))

t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Party", face = "bold")
     
# Calculate cases per thousand population for each state and 
# add CasesPerThousand to dataset
CasesPerThousand = (CovidByState$`COVID Cases`/CovidByState$Population)*1000
CovidByState = cbind(CovidByState, CasesPerThousand)

# Calculate deaths per thousand population for each state and 
# add DeathsPerThousand to dataset
DeathsPerThousand = (CovidByState$`COVID Deaths`/CovidByState$Population)*1000
CovidByState = cbind(CovidByState, DeathsPerThousand)

# Total Cases 
TotalCases = sum(CovidByState$"COVID Cases")

# Total Deaths
TotalDeaths = sum(CovidByState$"COVID Deaths")

# Calculate expected cases by state based on their population and
# add ExpectedCases to dataset
ExpectedCases = CovidByState$StatePopulationPercent*TotalCases
CovidByState = cbind(CovidByState, ExpectedCases)

# Calculate expected deaths by state based on their population and
# add ExpectedDeaths to dataset
ExpectedDeaths = CovidByState$StatePopulationPercent*TotalDeaths
CovidByState = cbind(CovidByState, ExpectedCases)

# Calculate (cases - Expected cases) as % of expected cases by state and
# add CasesDeviation to dataset
CasesDeviation = (CovidByState$'COVID Cases' - ExpectedCases)/ExpectedCases
CovidByState = cbind(CovidByState, CasesDeviation)

# Calculate (deaths - Expected deaths) as % of expected deaths by state and
# add DeathsDeviation to dataset
DeathsDeviation = (CovidByState$'COVID Deaths' - ExpectedDeaths)/ExpectedDeaths
CovidByState = cbind(CovidByState, DeathsDeviation)

# Summary of CovidByState
summary(CovidByState)

# export CovidByStae for SAS
#write.csv(CovidByState,"~/Desktop/CovidByState.csv", row.names = FALSE)

# Histogram of CasesPerThousand total and by Trifecta
mx = mean(CasesPerThousand)
h = hist(CasesPerThousand,
     main="COVID-19 Cases per Thousand by State",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CasesPerThousand[Trifecta == 'Democrat'])
h = hist(CovidByState$CasesPerThousand[Trifecta == 'Democrat'],
     main="COVID-19 Cases per Thousand by Democratic States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CasesPerThousand[Trifecta == 'Divided'])
h = hist(CovidByState$CasesPerThousand[Trifecta == 'Divided'],
         main="COVID-19 Cases per Thousand by Divided States",
         col="blue",
         xlab="Cases per Thousand Population",
         ylab="Number of States",
         ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(CasesPerThousand[Trifecta == 'Republican'])
h = hist(CovidByState$CasesPerThousand[Trifecta == 'Republican'],
     main="COVID-19 Cases per Thousand by Republican States",
     col="blue",
     xlab="Cases per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Histogram of DeathsPerThousand total and by Trifecta
mx = mean(DeathsPerThousand)
h = hist(DeathsPerThousand,
     main="COVID-19 Deaths per Thousand by State",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(DeathsPerThousand[Trifecta == 'Democrat'])
h = hist(CovidByState$DeathsPerThousand[Trifecta == 'Democrat'],
     main="COVID-19 Deaths per Thousand by Democratic States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:10)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(DeathsPerThousand[Trifecta == 'Divided'])
h = hist(CovidByState$DeathsPerThousand[Trifecta == 'Divided'],
     main="COVID-19 Deaths per Thousand by Divided States",
     col="blue",
     xlab="Deaths per Thousand Population",
     ylab="Number of States",
     ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

mx = mean(DeathsPerThousand[Trifecta == 'Republican'])
h = hist(CovidByState$DeathsPerThousand[Trifecta == 'Republican'],
         main="COVID-19 Deaths per Thousand by Republican States",
         col="blue",
         xlab="Deaths per Thousand Population",
         ylab="Number of States",
         ylim=range(0:15)
)
abline(v = mx, col = "Red", lwd = 2)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Total Cases by Trifecta
TotalCasesTriDem = sum(CovidByState$"COVID Cases"[Trifecta == 'Democrat'])
TotalCasesTriRep = sum(CovidByState$"COVID Cases"[Trifecta == 'Republican'])
TotalCasesTriDiv = sum(CovidByState$"COVID Cases"[Trifecta == 'Divided'])

# Total Deaths by Trifecta
TotalDeathsTriDem = sum(CovidByState$"COVID Deaths"[Trifecta == 'Democrat'])
TotalDeathsTriRep = sum(CovidByState$"COVID Deaths"[Trifecta == 'Republican'])
TotalDeathsTriDiv = sum(CovidByState$"COVID Deaths"[Trifecta == 'Divided'])

# Population by Trifecta
PopulationTriDem = sum(CovidByState$Population[Trifecta == 'Democrat'])
PopulationTriRep = sum(CovidByState$Population[Trifecta == 'Republican'])
PopulationTriDiv = sum(CovidByState$Population[Trifecta == 'Divided'])

# COVID infection rate per 1000 by Trifecta
CovidRateTriDem = (TotalCasesTriDem/PopulationTriDem)*1000
CovidRateTriRep = (TotalCasesTriRep/PopulationTriRep)*1000
CovidRateTriDiv = (TotalCasesTriDiv/PopulationTriDiv)*1000

# COVID death rate per 1000 by Trifecta
CovidDeathRateTriDem = (TotalDeathsTriDem/PopulationTriDem)*1000
CovidDeathRateTriRep = (TotalDeathsTriRep/PopulationTriRep)*1000
CovidDeathRateTriDiv = (TotalDeathsTriDiv/PopulationTriDiv)*1000

# Display rates by Trifecta
CovidRateTriDem 
CovidRateTriDiv
CovidRateTriRep 
CovidDeathRateTriDem 
CovidDeathRateTriDiv 
CovidDeathRateTriRep 

# boxplots of casesPerThousand for Democrat, Republican, and Divided Trifecta
boxplot((CasesDeviation*100) ~ CovidByState$"Trifecta", 
        main = "Cases Deviation by Party", 
        xlab = "Percent Deviation from Expected Cases",
        ylab="%",
        col = "blue"
)

# boxplots of deathsPerThousand for Democrat, Republican, and Divided Trifecta
boxplot((DeathsDeviation*100) ~ CovidByState$"Trifecta", 
        main = "Deaths Deviation by Party", 
        xlab = "Percent Deviation from Expected Deaths",
        ylab = "%",
        col = "blue"
)

# Barchart total cases per thousand by Trifecta
CasesPerThousandTri = c(CovidRateTriDem, CovidRateTriDiv, CovidRateTriRep)
barplot(CasesPerThousandTri, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Cases per Thousand Population by Party",
        ylim = c(0,35))

# Barchart total deaths per thousand by Trifecta
DeathsPerThousandTri = c(CovidDeathRateTriDem, CovidDeathRateTriDiv,
        CovidDeathRateTriRep) 
barplot(DeathsPerThousandTri, names=Parties, xlab="Party", ylab="Deaths per Thousand",
        col="blue", main="COVID-19 Deaths per Thousand Population by Party")

# Mean and S.D. of cases deviation from expected cases
sum(CasesDeviation)
mean(CasesDeviation)
sd(CasesDeviation)

# Mean and S.D. of cases deviation for Democratic Trifecta 
MeanTriDemocrat = round(mean(CovidByState$CasesDeviation[Trifecta == 
      'Democrat']), digits = 4)
SdTriDemocrat = round(sd(CovidByState$CasesDeviation[Trifecta == 
      'Democrat']), digits = 4)

# Mean and S.D. of cases deviation for Divided Trifecta
MeanTriDivided = round(mean(CovidByState$CasesDeviation[Trifecta == 
      'Divided']), digits = 4)
SdTriDivided = round(sd(CovidByState$CasesDeviation[Trifecta == 
      'Divided']), digits = 4)

# Mean and S.D. of cases deviation for Republican Trifecta
MeanTriRepublican = round(mean(CovidByState$CasesDeviation[Trifecta == 
      'Republican']), digits = 4)
SdTriRepublican = round(sd(CovidByState$CasesDeviation[Trifecta == 
      'Republican']), digits = 4)

# Print mean and s.d. for Trifecta
Mean = c(MeanTriDemocrat, MeanTriDivided, MeanTriRepublican)
SD = c(SdTriDemocrat, SdTriDivided, SdTriRepublican)

report = cbind(Parties, Mean, SD)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Cases Deviation Mean & SD", face = "plain")

# Mean and S.D. of death deviation from expected cases
sum(DeathsDeviation)
mean(DeathsDeviation)
sd(DeathsDeviation)

# Mean and S.D. of death deviation for Democratic Trifecta 
MeanTriDeathsDemocrat = round(mean(CovidByState$DeathsDeviation[Trifecta == 
      'Democrat']), digits = 4)
SdTriDeathsDemocrat = round(sd(CovidByState$DeathsDeviation[Trifecta == 
      'Democrat']), digits = 4)

# Mean and S.D. of death deviation for Divided Trifecta
MeanTriDeathsDivided = round(mean(CovidByState$DeathsDeviation[Trifecta == 
      'Divided']), digits = 4)
SdTriDeathsDivided = round(sd(CovidByState$DeathsDeviation[Trifecta == 
      'Divided']), digits = 4)

# Mean and S.D. of death deviation for Republican Trifecta
MeanTriDeathsRepublican = round(mean(CovidByState$DeathsDeviation[Trifecta == 
      'Republican']), digits = 4)
SdTriDeathsRepublican = round(sd(CovidByState$DeathsDeviation[Trifecta == 
      'Republican']), digits = 4)

# Print mean and s.d. for Trifecta deaths
Mean = c(MeanTriDeathsDemocrat, MeanTriDeathsDivided, MeanTriDeathsRepublican)
SD = c(SdTriDeathsDemocrat, SdTriDeathsDivided, SdTriDeathsRepublican)

report = cbind(Parties, Mean, SD)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Deaths Deviation Mean & SD", face = "plain")

#aggregate(DeathsDeviation~Trifecta, FUN=mean)
#aggregate(DeathsDeviation~Trifecta, FUN=sd)
#aggregate(CasesDeviation~Trifecta, FUN=mean)
#aggregate(CasesDeviation~Trifecta, FUN=sd)

# Checks for normality of CaseDeviations

# Q-Q plot of CaseDeviation
ggqqplot(CasesDeviation, color="blue", title = "Q-Q Plot of Cases Deviation")

# Check for bell shaped density plot
ggdensity(CasesDeviation, 
          main = "Density plot of COVID Case Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(CasesDeviation)
shapiro.test(DeathsDeviation)
 
# Two sample independent t-test of cases deviation between Democrat and republican
tResults = t.test(CovidByState$CasesDeviation[Trifecta == 'Democrat'], 
      CovidByState$CasesDeviation[Trifecta == 'Republican'],
      alternative = c("two.sided", "less", "greater"),
      mu = 0, paired = FALSE, var.equal = FALSE,
      conf.level = 0.95)

# Print table of t-test results
str(tResults)
Statistic = c("Method", "Conf Level", "t", "df", "p-value", "Conf Int Low", 
              "Conf Int High", "Mean Dem", "Mean Rep") 

Value = tResults$method
Value = c(Value, "0.95") # problem fetching this from the results
Value = c(Value, round(tResults$statistic, digits = 3))
Value = c(Value, round(tResults$parameter, digits = 2))
Value = c(Value, round(tResults$p.value, digits = 3))
Value = c(Value, round(tResults$conf.int[1], digits = 3))
Value = c(Value, round(tResults$conf.int[2], digits = 3))
Value = c(Value, round(tResults$estimate[1], digits = 3))
Value = c(Value, round(tResults$estimate[2], digits = 3))
Value
report = cbind(Statistic, Value)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:10), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "t-test COVID Cases, Rep vs Dem", face = "plain")

# Checks for normality of DeathsDeviations

# Q-Q plot of CaseDeviation
ggqqplot(DeathsDeviation, color="blue", title = "Q-Q Plot of Deaths Deviation")

# Check for bell shaped density plot
ggdensity(DeathsDeviation, 
          main = "Density plot of COVID Deaths Deviations",
          xlab = "Case Deviations", color= "blue")

# Shapiro.test for normality
shapiro.test(DeathsDeviation)

# Two sample independent t-test of deaths deviation between Democrat and republican
tResults = t.test(CovidByState$DeathsDeviation[Trifecta == 'Democrat'], 
                  CovidByState$DeathsDeviation[Trifecta == 'Republican'],
                  alternative = c("two.sided", "less", "greater"),
                  mu = 0, paired = FALSE, var.equal = FALSE,
                  conf.level = 0.95)

# Print table of t-test results
str(tResults)
Statistic = c("Method", "Conf Level", "t", "df", "p-value", "Conf Int Low", 
              "Conf Int High", "Mean Dem", "Mean Rep") 

Value = tResults$method
Value = c(Value, "0.95") # problem fetching this from the results
Value = c(Value, round(tResults$statistic, digits = 3))
Value = c(Value, round(tResults$parameter, digits = 2))
Value = c(Value, round(tResults$p.value, digits = 3))
Value = c(Value, round(tResults$conf.int[1], digits = 3))
Value = c(Value, round(tResults$conf.int[2], digits = 3))
Value = c(Value, round(tResults$estimate[1], digits = 3))
Value = c(Value, round(tResults$estimate[2], digits = 3))
Value
report = cbind(Statistic, Value)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:10), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "t-test COVID Deaths, Rep vs Dem", face = "plain")

# Print list of states by Trifecta
DemocratTriSt = CovidByState$StateCode[Trifecta == 'Democrat']
RepublicanTriSt = CovidByState$StateCode[Trifecta == 'Republican']
DividedTriSt = CovidByState$StateCode[Trifecta == 'Divided']
DemocratTriStList = paste(DemocratTriSt, collapse = " ")
RepublicanTriStList = paste(RepublicanTriSt, collapse = " ")
DividedTriStList = paste(DividedTriSt, collapse = " ")

States = c(DemocratTriStList, DividedTriStList, RepublicanTriStList)

report = cbind(Parties, States)
t = ggtexttable(report,  rows = NULL,theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:2), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "States by Party", face = "plain")

#
# One-Way ANOVA for cases deviation
#

# Overall mean and S.D. for CasesDeviation
ybar = mean(CovidByState$CasesDeviation)
ybar
var.y = var(CovidByState$CasesDeviation)
var.y

# Shortcut for CovidByState$CasesDeviation
y = CovidByState$CasesDeviation # so we don't have to keep typing 'CovidByState$CasesDeviation'

# Total observatioins, 50 states + DC
N = length(y)
N

# Sum of Squares Total
SST = sum( (y-ybar)^2 )
SST

# Check, same as var(y)
SST/(N-1) 

# Obtain sample S.D. for each group by Trifecta
#sd = aggregate(y~Trifecta, FUN=sd)[,2] 


# Obtain sample size for each group by Trifecta
lengths = aggregate(y~Trifecta, FUN=length)[,2] 

# Obtain sample means for each group by Trifecta
means = aggregate(y~Trifecta, FUN=mean)[,2]

# Sum of Squares Between
SSB = sum(lengths*(means-ybar)^2 )
SSB

# Degrees of freedom 
k = length(means)
dfB = k-1
dfW = N-k

# Sum of Squares Within
SSW = SST-SSB 

# Check SSW
# Obtain var for each group
vars = aggregate(y~Trifecta, FUN=var)[,2]
SSi = (lengths-1)*vars # vector of SS1, SS2, SS3

SSWck = sum(SSi)
SSWck

# Mean Square Deviations Between
MSB = SSB/dfB

# Mean Square Deviations Within
MSW = SSW/dfW

# F-value
fVal = MSB/MSW

# p-value
pVal = pf(fVal,dfB, dfW, lower.tail = F)

# Display ANOVA table
columns = c('Source', 'df',	'SS',	'MS',	'F',	'P-Value')

row1 = c('Between Groups', k-1, round(SSB, digits = 4), round(MSB, digits = 4), 
    round(fVal, digits = 4), round(pVal, digits = 4))
row2 = c('Within Groups', N-k, round(SSW, digits = 4), round(MSW, digits = 4), '', '')
row3 = c('Total', N-1, round(SST, digits = 4), '', '', '')
report = rbind(columns, row1, row2, row3)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:6), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "One-Way ANOVA of Cases Deviation by Party", face = "plain")

# R ANOVA function (the eazy way)
# confirm with above table
y = CovidByState$CasesDeviation
trifecta.aov = aov(y~Trifecta, data=CovidByState)
anova(trifecta.aov)

#
# ANOVA as a General Linear Model
#

trifecta.aov = aov(y~Trifecta)
trifecta.fit = fitted(trifecta.aov)
trifecta.res = residuals(trifecta.aov)
trifecta.obs = CasesDeviation
trifecta.ind = Trifecta
trifecta.table = data.frame(trifecta.ind, trifecta.obs, trifecta.fit,
    trifecta.res)
colnames(trifecta.table) = c('Ind. Var.', 'Observed', 'Fitted',
    'Residual')
trifecta.table

# Q-Q plot of residuals
ggqqplot(trifecta.res, color="blue", title = "Q-Q Plot of Residuals")

# Sum of Squares Error
SSE = sum(trifecta.res^2)
SSE

# Sum of Squares Total
SST = sum( (y-ybar)^2 )
SST

# R^2
Rsqr = 1-SSE/SST
Rsqr

# 100*Rsqr% of the variability in CaseDeviation is explained by party

# Fisher's Least Significant Difference
# produces a matrix of p-values
pairwise.t.test(CovidByState$CasesDeviation, Trifecta, p.adjust.method='none')

# Bonferroni correction
pairwise.t.test(CovidByState$CasesDeviation, Trifecta, p.adjust.method='bonferroni')

# Turey HSD
TukeyHSD(trifecta.aov)

#
# Assessing Homoscedasticity
#

# Levene's test
# can not reject H0 that variances are equal
trifecta.res = residuals(trifecta.aov)
anova(aov(trifecta.res^2~Trifecta))

# Plot residuals aginst fitted values
trifecta.fit = fitted.values(trifecta.aov)
plot(x=trifecta.fit, y=trifecta.res,
     main = "ANOVA Fitted Values vs Resuials",
     xlab = "Fitted Values",
     ylab = "Residuals")

# Standized residuals
trifecta.stdres = trifecta.res/(MSW^.5)
plot(x=trifecta.fit, y=trifecta.stdres, 
    main = "ANOVA Fitted Values vs Standard-Resuials")

#
# One-Way ANOVA for deaths deviation
#

# R ANOVA function (the eazy way)
y = CovidByState$DeathsDeviation
trifecta.aov = aov(y~Trifecta, data=CovidByState)
anovaT = anova(trifecta.aov)

# Display ANOVA table
columns = c('Source', 'df',	'SS',	'MS',	'F',	'P-Value')

row1 = c('Between Groups', anovaT$Df[1], 
         round(anovaT$'Sum Sq'[1], digits = 4), 
         round(anovaT$'Mean Sq'[1], digits = 4), 
         round(anovaT$'F value'[1], digits = 4), 
         round(anovaT$'Pr(>F)'[1], digits = 4))
row2 = c('Within Groups', anovaT$Df[2], 
         round(anovaT$'Sum Sq'[2], digits = 4), 
         round(anovaT$'Mean Sq'[2], digits = 4), 
         '', '')
row3 = c('Total', round(sum(anovaT$Df), digits = 4), 
         round(sum(anovaT$'Sum Sq'), digits = 4), 
         '', '', '')
report = rbind(columns, row1, row2, row3)

t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:4), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:6), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "One-Way ANOVA of Deaths Deviation by Party", face = "plain")

# Why are case rates different then death rates
plot(CasesDeviation, 
     DeathsDeviation,
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, All States")
abline(lm(DeathsDeviation ~ CasesDeviation))

plot(CasesDeviation[Trifecta == 'Democrat'], 
     DeathsDeviation[Trifecta == 'Democrat'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Democratic")
abline(lm(DeathsDeviation[Trifecta == 'Democrat'] ~ 
    CasesDeviation[Trifecta == 'Democrat']))

plot(CasesDeviation[Trifecta == 'Divided'], 
     DeathsDeviation[Trifecta == 'Divided'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Divided")
abline(lm(DeathsDeviation[Trifecta == 'Divided'] ~ 
     CasesDeviation[Trifecta == 'Divided']))

plot(CasesDeviation[Trifecta == 'Republican'], 
     DeathsDeviation[Trifecta == 'Republican'],
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, Republican")
abline(lm(DeathsDeviation[Trifecta == 'Republican'] ~ 
     CasesDeviation[Trifecta == 'Republican']))

# Find the outliers
SumDiv = summary(CasesDeviation)
SumDiv = summary(CasesDeviation[Trifecta == 'Divided'])
CovidByState$StateCode[CasesDeviation == SumDiv[1]]
CovidByState$StateCode[CasesDeviation == SumDiv[6]]
SumDiv = summary(CasesDeviation[Trifecta == 'Republican'])
CovidByState$StateCode[CasesDeviation == SumDiv[1]]
SumDDem = summary(DeathsDeviation[Trifecta == 'Democrat'])
CovidByState$StateCode[DeathsDeviation == SumDDem[6]]
SumDDiv = summary(DeathsDeviation[Trifecta == 'Divided'])
CovidByState$StateCode[DeathsDeviation == SumDDiv[6]]
SumDRep = summary(DeathsDeviation[Trifecta == 'Republican'])
CovidByState$StateCode[DeathsDeviation == SumDRep[6]]

# Linear regression of deaths deviation on cases deviation
casesDeaths.lm = lm(DeathsDeviation ~ CasesDeviation)

# table of statistics
Mean = c(round(mean(CasesDeviation), digits = 4), round(mean(DeathsDeviation), digits = 4))
SD = c(round(sd(CasesDeviation), digits = 4), round(sd(DeathsDeviation), digits = 4))
Variables = c("Cases Deviation", "Deaths Deviation")
report = cbind(Variables, Mean, SD)
t = ggtexttable(report, rows = NULL, theme = ttheme("mBlue"))
t %>%
  tab_add_hline(at.row = c(1:3), row.side = "top", linewidth = 3, linetype = 1) %>%
  tab_add_vline(at.column = c(1:3), column.side = c("left", "right"), from.row = 1, linewidth = 3, linetype = 1) %>%
  tab_add_border(from.row = 1, linetype = 1, linewidth = 3, linecolor = "black") %>%
  tab_add_title(text = "Deviations Mean & SD", face = "plain")

# create a data frame with the given x values
x1 = min(CasesDeviation)
x2 = max(CasesDeviation)
x = seq(x1, x2, l = 200)
CasesDeviationVals = data.frame(CasesDeviation = x)

conf.int = predict(casesDeaths.lm, CasesDeviationVals, interval='confidence')
pred.int = predict(casesDeaths.lm, CasesDeviationVals, interval='prediction')

plot(x = CovidByState$CasesDeviation, 
     y = CovidByState$DeathsDeviation,
     xlab = "Cases Deviation",
     ylab = "Deaths Deviation",
     main = "Cases Deviation vs Deaths Deviation, All States")
abline(casesDeaths.lm)

# add blue confidence interval lines
lines(x, conf.int[,2], col='blue')
lines(x, conf.int[,3], col='blue')

# add red prediction interval lines
lines(x, pred.int[,2], col='red')
lines(x, pred.int[,3], col='red')

legend('topleft', lty=1, col=c('blue', 'red'), legend=c('95% Conf band',
                                                        '95% Pred band'), cex=.7)

# residual analysis
casesDeaths.res = residuals(casesDeaths.lm)
casesDeaths.fit = fitted.values(casesDeaths.lm)
qqnorm(casesDeaths.res)
qqline(casesDeaths.res)
plot(casesDeaths.res~casesDeaths.fit)
#abline(h=0)
boxplot(casesDeaths.res, 
        main = "Regression Residuals", 
        xlab = "",
        ylab="Residual",
        col = "blue"
)
hist(casesDeaths.res,
   main="Regression Residuals",
   col="blue",
   xlab="Value",
   ylab="Frequency",
   ylim=range(0:10))

coefficients(casesDeaths.lm)
summary(casesDeaths.lm)
confint(casesDeaths.lm, level=.95)
anova(casesDeaths.lm)


# Linear regression of deaths deviation on cases deviation Democrat
CasesDeviationDem = CovidByState$CasesDeviation[Trifecta == 'Democrat']
DeathsDeviationDem = CovidByState$DeathsDeviation[Trifecta == 'Democrat']
casesDeathsDem.lm = lm(DeathsDeviationDem ~ CasesDeviationDem)

# create a data frame with the given x values
x1 = min(CasesDeviationDem)
x2 = max(CasesDeviationDem)
x = seq(x1, x2, l = 200)
CasesDeviationDemVals = data.frame(CasesDeviationDem = x)

confDem.int = predict(casesDeathsDem.lm, CasesDeviationDemVals, interval='confidence')
predDem.int = predict(casesDeathsDem.lm, CasesDeviationDemVals, interval='prediction')

plot(x = CasesDeviationDem, 
     y = DeathsDeviationDem,
     xlab = "Cases Deviation Democrat",
     ylab = "Deaths Deviation Democrat",
     main = "Cases Deviation vs Deaths Deviation, Democrat States")
abline(casesDeathsDem.lm)

# add blue confidence interval lines
lines(x, confDem.int[,2], col='blue')
lines(x, confDem.int[,3], col='blue')

# add red prediction interval lines
lines(x, predDem.int[,2], col='red')
lines(x, predDem.int[,3], col='red')

legend('topleft', lty=1, col=c('blue', 'red'), legend=c('95% Conf band',                                                    '95% Pred band'), cex=.7)

# residual analysis
casesDeathsDem.res = residuals(casesDeathsDem.lm)
casesDeathsDem.fit = fitted.values(casesDeathsDem.lm)
qqnorm(casesDeathsDem.res)
qqline(casesDeathsDem.res)

plot(casesDeathsDem.res~casesDeathsDem.fit)
boxplot(casesDeathsDem.res, 
        main = "Regression Residuals", 
        xlab = "",
        ylab="Residual",
        col = "blue"
)
hist(casesDeathsDem.res,
     main="Regression Residuals",
     col="blue",
     xlab="Value",
     ylab="Frequency",
     ylim=range(0:10))

coefficients(casesDeathsDem.lm)
summary(casesDeathsDem.lm)
confint(casesDeathsDem.lm, level=.95)
anova(casesDeathsDem.lm)


# Linear regression of deaths deviation on cases deviation Republican
CasesDeviationRep = CovidByState$CasesDeviation[Trifecta == 'Republican']
DeathsDeviationRep = CovidByState$DeathsDeviation[Trifecta == 'Republican']
casesDeathsRep.lm = lm(DeathsDeviationRep ~ CasesDeviationRep)

# create a data frame with the given x values
x1 = min(CasesDeviationRep)
x2 = max(CasesDeviationRep)
x = seq(x1, x2, l = 200)
CasesDeviationRepVals = data.frame(CasesDeviationRep = x)

confRep.int = predict(casesDeathsRep.lm, CasesDeviationRepVals, interval='confidence')
predRep.int = predict(casesDeathsRep.lm, CasesDeviationRepVals, interval='prediction')

plot(x = CasesDeviationRep, 
     y = DeathsDeviationRep,
     xlab = "Cases Deviation Republican",
     ylab = "Deaths Deviation Republican",
     main = "Cases Deviation vs Deaths Deviation, Republican States")
abline(casesDeathsRep.lm)

# add blue confidence interval lines
lines(x, confRep.int[,2], col='blue')
lines(x, confRep.int[,3], col='blue')

# add red prediction interval lines
lines(x, predRep.int[,2], col='red')
lines(x, predRep.int[,3], col='red')

legend('topleft', lty=1, col=c('blue', 'red'), legend=c('95% Conf band',                                                    '95% Pred band'), cex=.7)

# residual analysis
casesDeathsRep.res = residuals(casesDeathsRep.lm)
casesDeathsRep.fit = fitted.values(casesDeathsRep.lm)
qqnorm(casesDeathsRep.res)
qqline(casesDeathsRep.res)

plot(casesDeathsRep.res~casesDeathsRep.fit)
boxplot(casesDeathsRep.res, 
        main = "Regression Residuals", 
        xlab = "",
        ylab="Residual",
        col = "blue"
)
hist(casesDeathsRep.res,
     main="Regression Residuals",
     col="blue",
     xlab="Value",
     ylab="Frequency",
     ylim=range(0:10))

coefficients(casesDeathsRep.lm)
summary(casesDeathsRep.lm)
confint(casesDeathsRep.lm, level=.95)
anova(casesDeathsRep.lm)
