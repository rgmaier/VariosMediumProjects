library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(tidyr)
library(dplyr)

#Loading the data from the files I previously downloaded from Eurostat
hpi <- read.csv("estat_prc_hpi_q_filtered.tsv",sep="\t", header=TRUE)
hcpi <- read.csv("estat_prc_hicp_aind.tsv", sep="\t", header=TRUE)
netIncome <- read.csv("estat_earn_nt_net_filtered.tsv", sep="\t", header=TRUE)
countrycode <- read.table("iso3166-1.txt", sep=";", header=TRUE)

#Cleaning up the HPI data
hpi$freq.purchase.unit.geo.TIME_PERIOD <- as.character(hpi$freq.purchase.unit.geo.TIME_PERIOD)
hpi.old <- hpi
countryCodeTmp <- as.data.frame(t(as.data.frame(strsplit(x=hpi$freq.purchase.unit.geo.TIME_PERIOD,split=","))))$V4
hpi <- subset(hpi, select=-c(freq.purchase.unit.geo.TIME_PERIOD))
hpi <- as.data.frame(lapply(hpi, function(x) gsub("d|p", "",x)))
#Use Q4 as year end data to match frequency of other values
hpi <- select(hpi, matches("(Q4|CodeTmp)"))
hpi <- as.data.frame(sapply(hpi, function (x) as.numeric(as.character(x))))
hpi <- as.data.frame(apply(hpi, 2, function(x) (as.numeric(x)/hpi$X2010.Q4)*100))
hpi <- cbind(countryCodeTmp, hpi)
hpi <- pivot_longer(hpi, cols=starts_with("X"), names_to="year", names_prefix="X", values_to="HPIValue", values_drop_na=FALSE)
hpi$year <- substr(hpi$year,1,nchar(hpi$year)-3)


#Cleaning up the HCPI data
hcpi$freq.unit.coicop.geo.TIME_PERIOD <- as.character(hcpi$freq.unit.coicop.geo.TIME_PERIOD)
countryCodeTmp <- as.data.frame(t(as.data.frame(strsplit(x=hcpi$freq.unit.coicop.geo.TIME_PERIOD,split=","))))$V4
hcpi <- subset(hcpi, select=-c(freq.unit.coicop.geo.TIME_PERIOD))
hcpi <- as.data.frame(lapply(hcpi, function(x) gsub("d|p", "",x)))
hcpi <- as.data.frame(sapply(hcpi, function(x) as.numeric(as.character(x))))
hcpi <- as.data.frame(apply(hcpi, 2, function(x) (as.numeric(x)/hcpi$X2010)*100))
hcpi <- cbind(hcpi,countryCodeTmp)
hcpi <- pivot_longer(hcpi, cols=starts_with("X"), names_to="year", names_prefix="X", values_to="HCPIValue", values_drop_na=FALSE)

#Cleaning up the NetIncome data
netIncome$freq.currency.estruct.ecase.geo.TIME_PERIOD <- as.character(netIncome$freq.currency.estruct.ecase.geo.TIME_PERIOD)
countryCodeTmp <- as.data.frame(t(as.data.frame(strsplit(x=netIncome$freq.currency.estruct.ecase.geo.TIME_PERIOD,split=","))))$V5
netIncome <- subset(netIncome, select=-c(freq.currency.estruct.ecase.geo.TIME_PERIOD))
netIncome <- as.data.frame(lapply(netIncome, function(x) gsub("d|p", "",x)))
netIncome <- as.data.frame(sapply(netIncome, function (x) as.numeric(as.character(x))))
netIncome <- as.data.frame(apply(netIncome, 2, function(x) (as.numeric(x)/netIncome$X2010)*100))
netIncome <- cbind(netIncome,countryCodeTmp)
netIncome <- pivot_longer(netIncome, cols=starts_with("X"), names_to="year", names_prefix="X", values_to="NetIncomeValue", values_drop_na=FALSE)

data <- left_join(hcpi,hpi,by=c("countryCodeTmp"="countryCodeTmp","year"="year"))
data <- left_join(data,netIncome, by=c("countryCodeTmp"="countryCodeTmp","year"="year"))
countriesInAnalysis <- c("AT",
                         "BE",
                         "DE", 
                         "IT", 
                         "FR",
                         "PL",
                         "PT",
                         "SE",
                         "SI",
                         "SK",
                         "UK",
                         "EA19")

data <- subset(data, countryCodeTmp %in% countriesInAnalysis)

data <- pivot_longer(data, cols=c("HPIValue","HCPIValue","NetIncomeValue"),names_to="IndexType",values_to="IndexValue")
data$year <- substr(data$year,3,nchar(data$year))
data$year <- as.factor(data$year)
data$IndexType <- as.factor(data$IndexType)

data <- left_join(data,countrycode,by=c("countryCodeTmp"="iso3166.1.alpha1"))

p <- ggplot(data=data, aes(x=year,y=IndexValue, group=IndexType, color=IndexType))+
  geom_line(aes(linetype=IndexType),size=1)+
  theme_minimal()+
  labs(x="Year", y="Index value (2010 = 100)", title="Development of House Price Index (HPI), Harmonised Consumer Price Index (HCPI)\n and Net Income Index by year for selected countries",caption="Source: Eurostat", size=12)+
  ylab("Index value (2010 = 100)")+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12))+
  facet_wrap(~country)+
  theme(legend.position="bottom")

png(filename="plot.png", width=1000, height=1000, units="px")
p
dev.off()