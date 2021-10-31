
#                                           
#  COVID-19 by State Study                  
#  Class project for MTU 5701 Fall 2020     
#  by Lorenzo Gordon                        
#     Owen Applequist                       
#     Kasia Krueger                         
#                                           
 
# Install tidyverse library 
install.packages("tidyverse")
library(tidyverse)

# Import Excel spredsheet 
library(readxl)
CovidByState <- read_excel(file.choose(),
    col_types = c("text", "numeric", "text", "text", "text", "numeric", "numeric"))
attach(CovidByState)

# Strip state code from State variable and add StateCode variable to dataset
StateCode = str_split(State, " - ", simplify=TRUE)[,2]
CovidByState <- cbind(CovidByState, StateCode)

# Calculate state population as a percentage of U.S. population
USpopulation = sum(Population)
StatePopulationPercent = Population/USpopulation
CovidByState <- cbind(CovidByState, StatePopulationPercent)

# Check
#USpopulation
#sum(StatePopulationPercent)
#CovidByState

# Calculate cases per thousand population for each state
CasesPerThousand = (`COVID Cases`/Population)*1000
CovidByState <- cbind(CovidByState, CasesPerThousand)

# Calculate deaths per thousand population for each state
DeathsPerThousand = (`COVID Deaths`/Population)*1000
CovidByState <- cbind(CovidByState, DeathsPerThousand)
 
# Summary of CovidByState
summary(CovidByState)

# split dataframe by Governer's Party
CovidByGov = split(CovidByState, `Governer's Party`)

# split dataframe by 2016 election Party
CovidBy2016Party = split(CovidByState, `2016 R/D`)

# split dataframe by state region
CovidByRegion = split(CovidByState, `Region`)

# Histogram of CasesPerThousand total and by Governer's Party
hist(CasesPerThousand)
hist(CasesByGov$Democrat$CasesPerThousand)
hist(CasesByGov$Republican$CasesPerThousand)

# Histogram of DeathsPerThousand total and by Governer's Party
hist(DeathsPerThousand)
hist(CovidByGov$Democrat$DeathsPerThousand)
hist(CovidByGov$Republican$DeathsPerThousand)

# Histogram of CasesPerThousand by 2016 election Party
hist(CovidBy2016Party$Democrat$CasesPerThousand)
hist(CovidBy2016Party$Republican$CasesPerThousand)

# Hystogram of CasesPerThousand by 2016 election Party
hist(CovidBy2016Party$Democrat$DeathsPerThousand)
hist(CovidBy2016Party$Republican$DeathsPerThousand)

# Total Cases and total deaths
TotalCases = sum(CovidByState$"COVID Cases")
TotalDeaths = sum(CovidByState$'COVID Deaths')

# Total Cases and total Deaths by gov party
TotalCasesGovDem = sum(CovidByGov$Democrat$"COVID Cases")
TotalDeathsGovDem = sum(CovidByGov$Democrat$'COVID Deaths')
TotalCasesGovRep = sum(CovidByGov$Republican$"COVID Cases")
TotalDeathsGovRep = sum(CovidByGov$Republican$'COVID Deaths')

#TotalCasesGovDem 
#TotalDeathsGovDem
#TotalCasesGovRep 
#TotalDeathsGovRep 

# Population by Gov party
PopulationGovRep = sum(CovidByGov$Republican$Population)
PopulationGovDem = sum(CovidByGov$Democrat$Population)

#PopulationGovRep
#PopulationGovDem

# COVID infection rate per 1000 by Gov party 
CovidRateGovDem = (TotalCasesGovDem/PopulationGovDem)*1000
CovidRateGovRep = (TotalCasesGovRep/PopulationGovRep)*1000
DeathRateGovDem = (TotalDeathsGovDem/PopulationGovDem)*1000
DeathRateGovRep = (TotalDeathsGovRep/PopulationGovRep)*1000

# Total Cases and total Deaths by 2016 election party
TotalCases2016Dem = sum(CovidBy2016Party$Democrat$"COVID Cases")
TotalDeaths2016Dem = sum(CovidBy2016Party$Democrat$'COVID Deaths')
TotalCases2016Rep = sum(CovidBy2016Party$Republican$"COVID Cases")
TotalDeaths2016Rep = sum(CovidBy2016Party$Republican$'COVID Deaths')

# Population by 2016 election party
Population2016Rep = sum(CovidBy2016Party$Republican$Population)
Population2016Dem = sum(CovidBy2016Party$Democrat$Population)

# COVID infection rate per 1000 by 2016 election party
CovidRate2016Dem = (TotalCases2016Dem/Population2016Dem)*1000
CovidRate2016Rep = (TotalCases2016Rep/Population2016Rep)*1000
DeathRate2016Dem = (TotalDeaths2016Dem/Population2016Dem)*1000
DeathRate2016Rep = (TotalDeaths2016Rep/Population2016Rep)*1000

# Total Cases and total Deaths by region
TotalCasesRegionWest = sum(CovidByRegion$West$"COVID Cases")
TotalDeathsRegionWest = sum(CovidByRegion$West$'COVID Deaths')
TotalCasesRegionSouth = sum(CovidByRegion$South$"COVID Cases")
TotalDeathsRegionSouth = sum(CovidByRegion$South$'COVID Deaths')
otalCasesRegionMidwest = sum(CovidByRegion$Midwest$"COVID Cases")
TotalDeathsRegionMidwest = sum(CovidByRegion$Midwest$'COVID Deaths')
TotalCasesRegionNortheast = sum(CovidByRegion$Northeast$"COVID Cases")
TotalDeathsRegionNortheast = sum(CovidByRegion$Northeast$'COVID Deaths')

# Population by region
PopulationRegionSouth = sum(CovidByRegion$South$Population)
PopulationRegionWest = sum(CovidByRegion$West$Population)
PopulationRegionMidwest = sum(CovidByRegion$Midwest$Population)
PopulationRegionNortheast = sum(CovidByRegion$Northeast$Population)

# COVID infection rate per 1000 by region
CovidRateRegionSouth = (TotalCasesRegionSouth/PopulationRegionSouth)*1000
CovidRateRegionWest = (TotalCasesRegionWest/PopulationRegionWest)*1000
CovidRateRegionMidwest = (TotalCasesRegionWest/PopulationRegionWest)*1000
CovidRateRegionNorthEast = (TotalCasesRegionNortheast/PopulationRegionNortheast)*1000
DeathRateRegionSouth = (TotalDeathsRegionSouth/PopulationRegionSouth)*1000
DeathRateRegionWest= (TotalDeathsRegionWest/PopulationRegionWest)*1000
DeathRateRegionMidwest = (TotalDeathsRegionMidwest/PopulationRegionMidwest)*1000
DeathRateRegionNortheast = (TotalDeathsRegionNortheast/PopulationRegionNortheast)*1000

# Display rates by gov
CovidRateGovDem 
CovidRateGovRep 
DeathRateGovDem 
DeathRateGovRep

# Display rates by 2016 election
CovidRate2016Dem 
CovidRate2016Rep 
DeathRate2016Dem 
DeathRate2016Rep

# Display rates by region
CovidRateRegionSouth
CovidRateRegionWest
CovidRateRegionMidwest
CovidRateRegionNorthEast
DeathRateRegionSouth 
DeathRateRegionWest
DeathRateRegionMidwest 
DeathRateRegionNortheast

# boxplots of casesPerThousand for Democrat and Republican Governers
boxplot(CasesByGov$Democrat$CasesPerThousand, horizontal = TRUE, 
        main = "COVID Cases per 1000 for States with Democratic Governer", xlab = "Cases Per Thousand")
boxplot(CasesByGov$Republican$CasesPerThousand, horizontal = TRUE, 
        main = "COVID Cases per 1000 for States with Republican Governer", xlab = "Cases Per Thousand")

# boxplots of casesPerThousand for Democrat and Republican by 2016 eletion
boxplot(CovidBy2016Party$Democrat$CasesPerThousand, horizontal = TRUE, 
        main = "COVID Cases per 1000 for States with Democratic Governer", xlab = "Cases Per Thousand")
boxplot(CovidBy2016Party$Republican$CasesPerThousand, horizontal = TRUE, 
        main = "COVID Cases per 1000 for States with Republican Governer", xlab = "Cases Per Thousand")

# Barchart total cases and deaths per thousand by Governer's party
CasesPerThousandGov = c(CovidRateGovDem, CovidRateGovRep)
DeathsPerThousandGov = c(DeathRateGovDem, DeathRateGovRep)
Parties = c("Democrat", "Republican")
barplot(CasesPerThousandGov, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Cases per Thousand Population by Governor's Party")
barplot(DeathsPerThousandGov, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Deaths per Thousand Population by Governor's Party")

# Barchart total cases and deaths per thousand by 2016 election party
CasesPerThousand2016 = c(CovidRate2016Dem, CovidRate2016Rep)
DeathsPerThousand2016 = c(DeathRate2016Dem, DeathRate2016Rep)
Parties = c("Democrat", "Republican")
barplot(CasesPerThousand2016, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Cases per Thousand Population by 2016 Election Party")
barplot(DeathsPerThousand2016, names=Parties, xlab="Party", ylab="Cases per Thousand", col="blue", 
        main="COVID-19 Deaths per Thousand Population by 2016 Election Party")

# detach CovidByState
#detach(CovidByState)

