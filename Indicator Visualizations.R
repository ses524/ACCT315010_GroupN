## Clear Environment & Console
rm(list=ls(all=TRUE))
cat("\014") 

## Set up packages
library(tidyverse)
library(here)
library(dplyr)
library(gapminder)
library(ggrepel)
library(RColorBrewer)
if (!require("dichromat")) install.packages("dichromat")
library(dichromat)

## Import data
data<-read_csv(here("dataset.csv"))
sapply(data, class)

## Set data length
n <- length(data$datadate)

## Reorder data by CIK
data <- data %>% arrange(cik)

## Fix year to date
library(lubridate)
data$datadate <- year(as.Date(as.character(data$datadate), format = "%Y"))

# ROA: add column, calculate
data$ROA <- 0

for (i in 1:n) {
  data$ROA[i] <- (data$ni[i]/data$at[i]) 
}

# ROE: add column, calculate
data$ROE <- 0

for (i in 1:n) {
  data$ROE[i] <- (data$ni[i]/data$csho[i]) 
}

# Gross Profit Ratio: add column, calculate
data$GPrat <- 0

for (i in 1:n) {
  data$GPrat[i] <- ((data$sale[i]-data$cogs[i])/data$sale[i]) 
}

# Current ratio
data$Current <- 0

for (i in 1:n) {
  data$Current[i] <- (data$act[i]/data$lct[i])
}

# Debt-to-Equity ratio
data$DE <- 0

for (i in 1:n) {
  data$DE[i] <- (data$dt[i]/data$ceq[i])
}

## Visualization theme
theme_set(theme_classic())
#########
## ROA ##
#########
## Industry full
ifROA0 <- ggplot(data = subset(data,
                           cik != "39911"),
             mapping = aes(x = fyear,
                           y = ROA))

ifROA1 <- ifROA0 + geom_line(mapping = 
                       aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-1,1))
ifROA1

ifROA2<- ifROA1 + geom_smooth(color="#333333")
ifROA2

LE <- "blue"
ifROA3 <- ifROA2 + geom_line(data = subset(data,
                           cik == "799288"),
             mapping = aes(x = fyear,
                           y = ROA, group = cik, color = LE))
ifROA3

JCrew <- "chartreuse"
ifROA4 <- ifROA3 + geom_line(data = subset(data,
                                   cik == "1051251"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = JCrew))

ifROA4

CB <- "deepskyblue"
ifROA5 <- ifROA4 + geom_line(data = subset(data,
                                   cik == "883943"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = CB))
ifROA5

AE <- "firebrick"
ifROA6 <- ifROA5 + geom_line(data = subset(data,
                                   cik == "919012"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = AE))
ifROA6

UO <- "gold"
ifROA7 <- ifROA6 + geom_line(data = subset(data,
                                   cik == "912615"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = UO)) 
ifROA7

Gap <- "magenta"
ifROA8 <- ifROA7 + geom_line(data = subset(data,
                                   cik == "39911"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "ROA",
       title = "Gap's Return on Assets per Year Compared to Immediate Competitors within Industry Average",
       color="Company")+ 
  theme_minimal() + theme(legend.position="top")   
ifROA8


## Industry 2019
iROA0 <- ggplot(data = subset(data,
                           cik != "39911"),
             mapping = aes(x = fyear,
                           y = ROA))

iROA1 <- iROA0 + geom_line(mapping = 
                       aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-.5,.5)) +
  scale_x_continuous(limits=c(2015,2020), breaks=c(2015,2016,2017,2018,2019,2020) )
iROA1

iROA2<- iROA1 + geom_smooth(color="#333333")
iROA2

LE <- "blue"
iROA3 <- iROA2 + geom_line(data = subset(data,
                                   cik == "799288"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = LE))
iROA3

JCrew <- "chartreuse"
iROA4 <- iROA3 + geom_line(data = subset(data,
                                   cik == "1051251"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = JCrew))
iROA4

CB <- "deepskyblue"
iROA5 <- iROA4 + geom_line(data = subset(data,
                                   cik == "883943"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = CB))
iROA5

AE <- "firebrick"
iROA6 <- iROA5 + geom_line(data = subset(data,
                                   cik == "919012"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = AE))
iROA6

UO <- "gold"
iROA7 <- iROA6 + geom_line(data = subset(data,
                                   cik == "912615"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = UO)) 
iROA7

Gap <- "magenta"
iROA8 <- iROA7 + geom_line(data = subset(data,
                                   cik == "39911"),
                     mapping = aes(x = fyear,
                                   y = ROA, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "ROA",
       title = "Gap's Return on Assets per Year Compared to Immediate Competitors within Industry",
       color="Company")  + 
  theme_minimal() + theme(legend.position="top") 
iROA8


### Just Gap
gROA0 <- ggplot(data = subset(data,
                           cik == "39911"),
             mapping = aes(x = fyear,
                           y = ROA))

gROA1 <- gROA0 + geom_line(mapping = 
                       aes(group = cik),color = "magenta") +
  labs(x = "Year", y = "ROA",
       title = "Gap's Return on Assets per Year") +
  scale_x_continuous(breaks=c(1995,200,2005,2010,2015,2019,2020) )+ 
  theme_minimal() + theme(legend.position="top")
gROA1



#########
## ROE ##
#########
## Industry full
ifROE0 <- ggplot(data = subset(data,
                               cik != "39911"),
                 mapping = aes(x = fyear,
                               y = ROE))

ifROE1 <- ifROE0 + geom_line(mapping = 
                               aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-10,10))
ifROE1

ifROE2<- ifROE1 + geom_smooth(color="#333333")
ifROE2

LE <- "blue"
ifROE3 <- ifROE2 + geom_line(data = subset(data,
                                           cik == "799288"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = LE))
ifROE3

JCrew <- "chartreuse"
ifROE4 <- ifROE3 + geom_line(data = subset(data,
                                           cik == "1051251"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = JCrew))

ifROE4

CB <- "deepskyblue"
ifROE5 <- ifROE4 + geom_line(data = subset(data,
                                           cik == "883943"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = CB))
ifROE5

AE <- "firebrick"
ifROE6 <- ifROE5 + geom_line(data = subset(data,
                                           cik == "919012"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = AE))
ifROE6

UO <- "gold"
ifROE7 <- ifROE6 + geom_line(data = subset(data,
                                           cik == "912615"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = UO)) 
ifROE7

Gap <- "magenta"
ifROE8 <- ifROE7 + geom_line(data = subset(data,
                                           cik == "39911"),
                             mapping = aes(x = fyear,
                                           y = ROE, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "ROE",
       title = "Gap's Return on Equity per Year Compared to Immediate Competitors within Industry Average",
       color="Company")   + 
  theme_minimal() + theme(legend.position="top")
ifROE8


## Industry 2019
iROE0 <- ggplot(data = subset(data,
                              cik != "39911"),
                mapping = aes(x = fyear,
                              y = ROE))

iROE1 <- iROE0 + geom_line(mapping = 
                             aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-5,5)) +
  scale_x_continuous(limits=c(2015,2020), breaks=c(2015,2016,2017,2018,2019,2020) )
iROE1

iROE2<- iROE1 + geom_smooth(color="#333333")
iROE2

LE <- "blue"
iROE3 <- iROE2 + geom_line(data = subset(data,
                                         cik == "799288"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = LE))
iROE3

JCrew <- "chartreuse"
iROE4 <- iROE3 + geom_line(data = subset(data,
                                         cik == "1051251"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = JCrew))
iROE4

CB <- "deepskyblue"
iROE5 <- iROE4 + geom_line(data = subset(data,
                                         cik == "883943"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = CB))
iROE5

AE <- "firebrick"
iROE6 <- iROE5 + geom_line(data = subset(data,
                                         cik == "919012"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = AE))
iROE6

UO <- "gold"
iROE7 <- iROE6 + geom_line(data = subset(data,
                                         cik == "912615"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = UO)) 
iROE7

Gap <- "magenta"
iROE8 <- iROE7 + geom_line(data = subset(data,
                                         cik == "39911"),
                           mapping = aes(x = fyear,
                                         y = ROE, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "ROE",
       title = "Gap's Return on Equity per Year Compared to Immediate Competitors within Industry",
       color="Company")  + 
  theme_minimal() + theme(legend.position="top") 
iROE8


### Just Gap
gROE0 <- ggplot(data = subset(data,
                              cik == "39911"),
                mapping = aes(x = fyear,
                              y = ROE))

gROE1 <- gROE0 + geom_line(mapping = 
                             aes(group = cik),color = "magenta") +
  labs(x = "Year", y = "ROE",
       title = "Gap's Return on Equity per Year") +
  scale_x_continuous(breaks=c(1995,200,2005,2010,2015,2019,2020) )+ 
  theme_minimal() + theme(legend.position="top")
gROE1


########################
## Gross Profit Ratio ##
########################
## Industry full
ifGPrat0 <- ggplot(data = subset(data,
                                 cik != "39911"),
                   mapping = aes(x = fyear,
                                 y = GPrat))

ifGPrat1 <- ifGPrat0 + geom_line(mapping = 
                                   aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(0,1))
ifGPrat1

ifGPrat2<- ifGPrat1 + geom_smooth(color="#333333")
ifGPrat2

LE <- "blue"
ifGPrat3 <- ifGPrat2 + geom_line(data = subset(data,
                                               cik == "799288"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = LE))
ifGPrat3

JCrew <- "chartreuse"
ifGPrat4 <- ifGPrat3 + geom_line(data = subset(data,
                                               cik == "1051251"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = JCrew))

ifGPrat4

CB <- "deepskyblue"
ifGPrat5 <- ifGPrat4 + geom_line(data = subset(data,
                                               cik == "883943"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = CB))
ifGPrat5

AE <- "firebrick"
ifGPrat6 <- ifGPrat5 + geom_line(data = subset(data,
                                               cik == "919012"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = AE))
ifGPrat6

UO <- "gold"
ifGPrat7 <- ifGPrat6 + geom_line(data = subset(data,
                                               cik == "912615"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = UO)) 
ifGPrat7

Gap <- "magenta"
ifGPrat8 <- ifGPrat7 + geom_line(data = subset(data,
                                               cik == "39911"),
                                 mapping = aes(x = fyear,
                                               y = GPrat, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Gross Profit Ratio",
       title = "Gap's Gross Profit Ratio per Year Compared to Immediate Competitors within Industry Average",
       color="Company")  + 
  theme_minimal() + theme(legend.position="top") 
ifGPrat8


## Industry 2019
iGPrat0 <- ggplot(data = subset(data,
                                cik != "39911"),
                  mapping = aes(x = fyear,
                                y = GPrat))

iGPrat1 <- iGPrat0 + geom_line(mapping = 
                                 aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(limits=c(2015,2020), breaks=c(2015,2016,2017,2018,2019,2020) )
iGPrat1

iGPrat2<- iGPrat1 + geom_smooth(color="#333333")
iGPrat2

LE <- "blue"
iGPrat3 <- iGPrat2 + geom_line(data = subset(data,
                                             cik == "799288"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = LE))
iGPrat3

JCrew <- "chartreuse"
iGPrat4 <- iGPrat3 + geom_line(data = subset(data,
                                             cik == "1051251"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = JCrew))
iGPrat4

CB <- "deepskyblue"
iGPrat5 <- iGPrat4 + geom_line(data = subset(data,
                                             cik == "883943"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = CB))
iGPrat5

AE <- "firebrick"
iGPrat6 <- iGPrat5 + geom_line(data = subset(data,
                                             cik == "919012"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = AE))
iGPrat6

UO <- "gold"
iGPrat7 <- iGPrat6 + geom_line(data = subset(data,
                                             cik == "912615"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = UO)) 
iGPrat7

Gap <- "magenta"
iGPrat8 <- iGPrat7 + geom_line(data = subset(data,
                                             cik == "39911"),
                               mapping = aes(x = fyear,
                                             y = GPrat, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Gross Profit Ratio",
       title = "Gap's Gross Profit Ratio per Year Compared to Immediate Competitors within Industry",
       color="Company")   + 
  theme_minimal() + theme(legend.position="top")
iGPrat8


### Just Gap
gGPrat0 <- ggplot(data = subset(data,
                                cik == "39911"),
                  mapping = aes(x = fyear,
                                y = GPrat))

gGPrat1 <- gGPrat0 + geom_line(mapping = 
                                 aes(group = cik),color = "magenta") +
  labs(x = "Year", y = "Gross Profit Ratio",
       title = "Gap's Gross Profit Ratio per Year") +
  scale_x_continuous(breaks=c(1995,200,2005,2010,2015,2019,2020) )+ 
  theme_minimal() + theme(legend.position="top")
gGPrat1


###################
## Current Ratio ##
###################
## Industry full
ifCurrent0 <- ggplot(data = subset(data,
                                   cik != "39911"),
                     mapping = aes(x = fyear,
                                   y = Current))

ifCurrent1 <- ifCurrent0 + geom_line(mapping = 
                                       aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(0,10))
ifCurrent1

ifCurrent2<- ifCurrent1 + geom_smooth(color="#333333")
ifCurrent2

LE <- "blue"
ifCurrent3 <- ifCurrent2 + geom_line(data = subset(data,
                                                   cik == "799288"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = LE))
ifCurrent3

JCrew <- "chartreuse"
ifCurrent4 <- ifCurrent3 + geom_line(data = subset(data,
                                                   cik == "1051251"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = JCrew))

ifCurrent4

CB <- "deepskyblue"
ifCurrent5 <- ifCurrent4 + geom_line(data = subset(data,
                                                   cik == "883943"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = CB))
ifCurrent5

AE <- "firebrick"
ifCurrent6 <- ifCurrent5 + geom_line(data = subset(data,
                                                   cik == "919012"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = AE))
ifCurrent6

UO <- "gold"
ifCurrent7 <- ifCurrent6 + geom_line(data = subset(data,
                                                   cik == "912615"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = UO)) 
ifCurrent7

Gap <- "magenta"
ifCurrent8 <- ifCurrent7 + geom_line(data = subset(data,
                                                   cik == "39911"),
                                     mapping = aes(x = fyear,
                                                   y = Current, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Current Ratio",
       title = "Gap's Current Ratio per Year Compared to Immediate Competitors within Industry Average",
       color="Company") + 
  theme_minimal() + theme(legend.position="top")  
ifCurrent8


## Industry 2019
iCurrent0 <- ggplot(data = subset(data,
                                  cik != "39911"),
                    mapping = aes(x = fyear,
                                  y = Current))

iCurrent1 <- iCurrent0 + geom_line(mapping = 
                                     aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(0,10)) +
  scale_x_continuous(limits=c(2015,2020), breaks=c(2015,2016,2017,2018,2019,2020) )
iCurrent1

iCurrent2<- iCurrent1 + geom_smooth(color="#333333")
iCurrent2

LE <- "blue"
iCurrent3 <- iCurrent2 + geom_line(data = subset(data,
                                                 cik == "799288"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = LE))
iCurrent3

JCrew <- "chartreuse"
iCurrent4 <- iCurrent3 + geom_line(data = subset(data,
                                                 cik == "1051251"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = JCrew))
iCurrent4

CB <- "deepskyblue"
iCurrent5 <- iCurrent4 + geom_line(data = subset(data,
                                                 cik == "883943"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = CB))
iCurrent5

AE <- "firebrick"
iCurrent6 <- iCurrent5 + geom_line(data = subset(data,
                                                 cik == "919012"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = AE))
iCurrent6

UO <- "gold"
iCurrent7 <- iCurrent6 + geom_line(data = subset(data,
                                                 cik == "912615"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = UO)) 
iCurrent7

Gap <- "magenta"
iCurrent8 <- iCurrent7 + geom_line(data = subset(data,
                                                 cik == "39911"),
                                   mapping = aes(x = fyear,
                                                 y = Current, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Current Ratio",
       title = "Gap's Current Ratio per Year Compared to Immediate Competitors within Industry",
       color="Company") + 
  theme_minimal() + theme(legend.position="top")  
iCurrent8


### Just Gap
gCurrent0 <- ggplot(data = subset(data,
                                  cik == "39911"),
                    mapping = aes(x = fyear,
                                  y = Current))

gCurrent1 <- gCurrent0 + geom_line(mapping = 
                                     aes(group = cik),color = "magenta") +
  labs(x = "Year", y = "Current Ratio",
       title = "Gap's Current Ratio per Year") +
  scale_x_continuous(breaks=c(1995,200,2005,2010,2015,2019,2020) ) + 
  theme_minimal() + theme(legend.position="top")
gCurrent1


##########################
## Debt-to-Equity Ratio ##
##########################
## Industry full
ifDE0 <- ggplot(data = subset(data,
                              cik != "39911"),
                mapping = aes(x = fyear,
                              y = DE))

ifDE1 <- ifDE0 + geom_line(mapping = 
                             aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-.5,4))
ifDE1

ifDE2<- ifDE1 + geom_smooth(color="#333333")
ifDE2

LE <- "blue"
ifDE3 <- ifDE2 + geom_line(data = subset(data,
                                         cik == "799288"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = LE))
ifDE3

JCrew <- "chartreuse"
ifDE4 <- ifDE3 + geom_line(data = subset(data,
                                         cik == "1051251"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = JCrew))

ifDE4

CB <- "deepskyblue"
ifDE5 <- ifDE4 + geom_line(data = subset(data,
                                         cik == "883943"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = CB))
ifDE5

AE <- "firebrick"
ifDE6 <- ifDE5 + geom_line(data = subset(data,
                                         cik == "919012"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = AE))
ifDE6

UO <- "gold"
ifDE7 <- ifDE6 + geom_line(data = subset(data,
                                         cik == "912615"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = UO)) 
ifDE7

Gap <- "magenta"
ifDE8 <- ifDE7 + geom_line(data = subset(data,
                                         cik == "39911"),
                           mapping = aes(x = fyear,
                                         y = DE, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Debt-to-Equity Ratio",
       title = "Gap's Debt-to-Equity Ratio per Year Compared to Immediate Competitors within Industry Average",
       color="Company")   + 
  theme_minimal() + theme(legend.position="top")
ifDE8


## Industry 2019
iDE0 <- ggplot(data = subset(data,
                             cik != "39911"),
               mapping = aes(x = fyear,
                             y = DE))

iDE1 <- iDE0 + geom_line(mapping = 
                           aes(group = cik), alpha = 0.10, color = "gray50") + 
  scale_y_continuous(limits=c(-.5,4)) +
  scale_x_continuous(limits=c(2015,2020), breaks=c(2015,2016,2017,2018,2019,2020) )
iDE1

iDE2<- iDE1 + geom_smooth(color="#333333")
iDE2

LE <- "blue"
iDE3 <- iDE2 + geom_line(data = subset(data,
                                       cik == "799288"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = LE))
iDE3

JCrew <- "chartreuse"
iDE4 <- iDE3 + geom_line(data = subset(data,
                                       cik == "1051251"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = JCrew))
iDE4

CB <- "deepskyblue"
iDE5 <- iDE4 + geom_line(data = subset(data,
                                       cik == "883943"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = CB))
iDE5

AE <- "firebrick"
iDE6 <- iDE5 + geom_line(data = subset(data,
                                       cik == "919012"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = AE))
iDE6

UO <- "gold"
iDE7 <- iDE6 + geom_line(data = subset(data,
                                       cik == "912615"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = UO)) 
iDE7

Gap <- "magenta"
iDE8 <- iDE7 + geom_line(data = subset(data,
                                       cik == "39911"),
                         mapping = aes(x = fyear,
                                       y = DE, group = cik, color = Gap)) +
  scale_color_manual(values = c(LE,JCrew,CB,AE,UO,Gap),labels=c("Lands' End","JCrew","Christopher & Banks", "American Eagle", "Urban Outfitters","Gap")) +
  labs(x = "Year", y = "Debt-to-Equity Ratio",
       title = "Gap's Debt-to-Equity Ratio per Year Compared to Immediate Competitors within Industry",
       color="Company")   + 
  theme_minimal() + theme(legend.position="top")
iDE8


### Just Gap
gDE0 <- ggplot(data = subset(data,
                             cik == "39911"),
               mapping = aes(x = fyear,
                             y = DE))

gDE1 <- gDE0 + geom_line(mapping = 
                           aes(group = cik),color = "magenta") +
  labs(x = "Year", y = "Debt-to-Equity Ratio",
       title = "Gap's Debt-to-Equity Ratio per Year") +
  scale_x_continuous(breaks=c(1995,200,2005,2010,2015,2019,2020) )+ 
  theme_minimal() + theme(legend.position="top")
gDE1











###############STOP HERE############





# Fixed asset turnover ratio
(net sales/(fixed assets-accumulated depreciation))



# avg inv

data$AvgInv <- 0


for (i in 2:n) {
  data$AvgInv <-  ((data$invt[i]+data$invt[i-1]))
}


for (i in 2:n) {
  data$AvgInv <- (if(data$cik[i] == data$cik[i-1]) {
    (mean(data$invt[i],data$invt[i-1]))
  } else {
    ""
  }
  )
}

for (i in 2:n) {
  data$AvgInv <- (data$cik[i] == data$cik[i-1])
}


## TEST VISUALIZATION
p <- ggplot(data = data,
            mapping = aes(x = fyear,
                          y = ROA))
p + geom_line(mapping = 
                aes(group = cik, color = cik))       





## Indicators



# Net sales growth

# Inv turnover ratio
(cogs/avg inv)



# Asset turnover ratio
(sales/avg assets)

