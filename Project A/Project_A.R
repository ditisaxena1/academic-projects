#install.packages("readxl")
#install.packages("dplyr")
#install.packages("tidyverse")
library(readxl)
library(tidyverse)

#read the daily gold data
getwd()
gold_data <- read_excel("Gold Historical Prices.xlsx")
summary(gold_data)#confirmed that there are no NAs. ara ta krataw ola ta data mou
gold_data<-na.omit(gold_data)
#format the dates correctly
gold_data$Date <- as.Date(gold_data$Date, format= "%d-%b-%y")
is.numeric(gold_data$Price)

#Create subset according to the years from 2000 to 2018
gold_data_2000<-subset(gold_data,Date>="2000-01-01" & Date<="2000-12-31")
gold_data_2001<-subset(gold_data,Date>="2001-01-01" & Date<="2001-12-31")
gold_data_2002<-subset(gold_data,Date>="2002-01-01" & Date<="2002-12-31")
gold_data_2003<-subset(gold_data,Date>="2003-01-01" & Date<="2003-12-31")
gold_data_2004<-subset(gold_data,Date>="2004-01-01" & Date<="2004-12-31")
gold_data_2005<-subset(gold_data,Date>="2005-01-01" & Date<="2005-12-31")
gold_data_2006<-subset(gold_data,Date>="2006-01-01" & Date<="2006-12-31")
gold_data_2007<-subset(gold_data,Date>="2007-01-01" & Date<="2007-12-31")
gold_data_2008<-subset(gold_data,Date>="2008-01-01" & Date<="2008-12-31")
gold_data_2009<-subset(gold_data,Date>="2009-01-01" & Date<="2009-12-31")
gold_data_2010<-subset(gold_data,Date>="2010-01-01" & Date<="2010-12-31")
gold_data_2011<-subset(gold_data,Date>="2011-01-01" & Date<="2011-12-31")
gold_data_2012<-subset(gold_data,Date>="2012-01-01" & Date<="2012-12-31")
gold_data_2013<-subset(gold_data,Date>="2013-01-01" & Date<="2013-12-31")
gold_data_2014<-subset(gold_data,Date>="2014-01-01" & Date<="2014-12-31")
gold_data_2015<-subset(gold_data,Date>="2015-01-01" & Date<="2015-12-31")
gold_data_2016<-subset(gold_data,Date>="2016-01-01" & Date<="2016-12-31")
gold_data_2017<-subset(gold_data,Date>="2017-01-01" & Date<="2017-12-31")
gold_data_2018<-subset(gold_data,Date>="2018-01-01" & Date<="2018-12-31")


#Taking out prices for each year to calculate Mean, SD and CI
gold_data_2000_price<-gold_data_2000$Price
gold_data_2001_price<-gold_data_2001$Price
gold_data_2002_price<-gold_data_2002$Price
gold_data_2003_price<-gold_data_2003$Price
gold_data_2004_price<-gold_data_2004$Price
gold_data_2005_price<-gold_data_2005$Price
gold_data_2006_price<-gold_data_2006$Price
gold_data_2007_price<-gold_data_2007$Price
gold_data_2008_price<-gold_data_2008$Price
gold_data_2009_price<-gold_data_2009$Price
gold_data_2010_price<-gold_data_2010$Price
gold_data_2011_price<-gold_data_2011$Price
gold_data_2012_price<-gold_data_2012$Price
gold_data_2013_price<-gold_data_2013$Price
gold_data_2014_price<-gold_data_2014$Price
gold_data_2015_price<-gold_data_2015$Price
gold_data_2016_price<-gold_data_2016$Price
gold_data_2017_price<-gold_data_2017$Price
gold_data_2018_price<-gold_data_2018$Price

#calculate mean of all yearly categorised data

mean_years<-c(mean(gold_data_2000_price),mean(gold_data_2001_price),mean(gold_data_2002_price),mean(gold_data_2003_price),mean(gold_data_2004_price),mean(gold_data_2005_price),mean(gold_data_2006_price),mean(gold_data_2007_price),mean(gold_data_2008_price),mean(gold_data_2009_price),mean(gold_data_2010_price),mean(gold_data_2011_price),mean(gold_data_2012_price),mean(gold_data_2013_price),mean(gold_data_2014_price),mean(gold_data_2015_price),mean(gold_data_2016_price),mean(gold_data_2017_price),mean(gold_data_2018_price))

#calculate standard deviation of all yearly categorised data

standard_deviation_years<-c(sd(gold_data_2000_price),sd(gold_data_2001_price),sd(gold_data_2002_price),sd(gold_data_2003_price),sd(gold_data_2004_price),sd(gold_data_2005_price),sd(gold_data_2006_price),sd(gold_data_2007_price),sd(gold_data_2008_price),sd(gold_data_2009_price),sd(gold_data_2010_price),sd(gold_data_2011_price),sd(gold_data_2012_price),sd(gold_data_2013_price),sd(gold_data_2014_price),sd(gold_data_2015_price),sd(gold_data_2016_price),sd(gold_data_2017_price),sd(gold_data_2018_price))

#print mean of all years 
print(mean_years)

#print standard devation of all years
print(standard_deviation_years)

#Confidence Interval Upper Limit
UpperLimit_2000<-mean(gold_data_2000_price)+(1.96)*(sd(gold_data_2000_price)/sqrt(dim(gold_data_2000)[1]))
UpperLimit_2001<-mean(gold_data_2001_price)+(1.96)*(sd(gold_data_2001_price)/sqrt(dim(gold_data_2001)[1]))
UpperLimit_2002<-mean(gold_data_2002_price)+(1.96)*(sd(gold_data_2002_price)/sqrt(dim(gold_data_2002)[1]))
UpperLimit_2003<-mean(gold_data_2003_price)+(1.96)*(sd(gold_data_2003_price)/sqrt(dim(gold_data_2003)[1]))
UpperLimit_2004<-mean(gold_data_2004_price)+(1.96)*(sd(gold_data_2004_price)/sqrt(dim(gold_data_2004)[1]))
UpperLimit_2005<-mean(gold_data_2005_price)+(1.96)*(sd(gold_data_2005_price)/sqrt(dim(gold_data_2005)[1]))
UpperLimit_2006<-mean(gold_data_2006_price)+(1.96)*(sd(gold_data_2006_price)/sqrt(dim(gold_data_2006)[1]))
UpperLimit_2007<-mean(gold_data_2007_price)+(1.96)*(sd(gold_data_2007_price)/sqrt(dim(gold_data_2007)[1]))
UpperLimit_2008<-mean(gold_data_2008_price)+(1.96)*(sd(gold_data_2008_price)/sqrt(dim(gold_data_2008)[1]))
UpperLimit_2009<-mean(gold_data_2009_price)+(1.96)*(sd(gold_data_2009_price)/sqrt(dim(gold_data_2009)[1]))
UpperLimit_2010<-mean(gold_data_2010_price)+(1.96)*(sd(gold_data_2010_price)/sqrt(dim(gold_data_2010)[1]))
UpperLimit_2011<-mean(gold_data_2011_price)+(1.96)*(sd(gold_data_2011_price)/sqrt(dim(gold_data_2011)[1]))
UpperLimit_2012<-mean(gold_data_2012_price)+(1.96)*(sd(gold_data_2012_price)/sqrt(dim(gold_data_2012)[1]))
UpperLimit_2013<-mean(gold_data_2013_price)+(1.96)*(sd(gold_data_2013_price)/sqrt(dim(gold_data_2013)[1]))
UpperLimit_2014<-mean(gold_data_2014_price)+(1.96)*(sd(gold_data_2014_price)/sqrt(dim(gold_data_2014)[1]))
UpperLimit_2015<-mean(gold_data_2015_price)+(1.96)*(sd(gold_data_2015_price)/sqrt(dim(gold_data_2015)[1]))
UpperLimit_2016<-mean(gold_data_2016_price)+(1.96)*(sd(gold_data_2016_price)/sqrt(dim(gold_data_2016)[1]))
UpperLimit_2017<-mean(gold_data_2017_price)+(1.96)*(sd(gold_data_2017_price)/sqrt(dim(gold_data_2017)[1]))
UpperLimit_2018<-mean(gold_data_2018_price)+(1.96)*(sd(gold_data_2018_price)/sqrt(dim(gold_data_2018)[1]))

#Confidence Interval Lower Limit

LowerLimit_2000<-mean(gold_data_2000_price)-(1.96)*(sd(gold_data_2000_price)/sqrt(dim(gold_data_2000)[1]))
LowerLimit_2001<-mean(gold_data_2001_price)-(1.96)*(sd(gold_data_2001_price)/sqrt(dim(gold_data_2001)[1]))
LowerLimit_2002<-mean(gold_data_2002_price)-(1.96)*(sd(gold_data_2002_price)/sqrt(dim(gold_data_2002)[1]))
LowerLimit_2003<-mean(gold_data_2003_price)-(1.96)*(sd(gold_data_2003_price)/sqrt(dim(gold_data_2003)[1]))
LowerLimit_2004<-mean(gold_data_2004_price)-(1.96)*(sd(gold_data_2004_price)/sqrt(dim(gold_data_2004)[1]))
LowerLimit_2005<-mean(gold_data_2005_price)-(1.96)*(sd(gold_data_2005_price)/sqrt(dim(gold_data_2005)[1]))
LowerLimit_2006<-mean(gold_data_2006_price)-(1.96)*(sd(gold_data_2006_price)/sqrt(dim(gold_data_2006)[1]))
LowerLimit_2007<-mean(gold_data_2007_price)-(1.96)*(sd(gold_data_2007_price)/sqrt(dim(gold_data_2007)[1]))
LowerLimit_2008<-mean(gold_data_2008_price)-(1.96)*(sd(gold_data_2008_price)/sqrt(dim(gold_data_2008)[1]))
LowerLimit_2009<-mean(gold_data_2009_price)-(1.96)*(sd(gold_data_2009_price)/sqrt(dim(gold_data_2009)[1]))
LowerLimit_2010<-mean(gold_data_2010_price)-(1.96)*(sd(gold_data_2010_price)/sqrt(dim(gold_data_2010)[1]))
LowerLimit_2011<-mean(gold_data_2011_price)-(1.96)*(sd(gold_data_2011_price)/sqrt(dim(gold_data_2011)[1]))
LowerLimit_2012<-mean(gold_data_2012_price)-(1.96)*(sd(gold_data_2012_price)/sqrt(dim(gold_data_2012)[1]))
LowerLimit_2013<-mean(gold_data_2013_price)-(1.96)*(sd(gold_data_2013_price)/sqrt(dim(gold_data_2013)[1]))
LowerLimit_2014<-mean(gold_data_2014_price)-(1.96)*(sd(gold_data_2014_price)/sqrt(dim(gold_data_2014)[1]))
LowerLimit_2015<-mean(gold_data_2015_price)-(1.96)*(sd(gold_data_2015_price)/sqrt(dim(gold_data_2015)[1]))
LowerLimit_2016<-mean(gold_data_2016_price)-(1.96)*(sd(gold_data_2016_price)/sqrt(dim(gold_data_2016)[1]))
LowerLimit_2017<-mean(gold_data_2017_price)-(1.96)*(sd(gold_data_2017_price)/sqrt(dim(gold_data_2017)[1]))
LowerLimit_2018<-mean(gold_data_2018_price)-(1.96)*(sd(gold_data_2018_price)/sqrt(dim(gold_data_2018)[1]))

#store upper and lower limits in vectors
UpperLimits_2000_2018<-c(UpperLimit_2000,UpperLimit_2001,UpperLimit_2002,UpperLimit_2003,UpperLimit_2004,UpperLimit_2005,UpperLimit_2006,UpperLimit_2007,UpperLimit_2008,UpperLimit_2009,UpperLimit_2010,UpperLimit_2011,UpperLimit_2012,UpperLimit_2013,UpperLimit_2014,UpperLimit_2015,UpperLimit_2016,UpperLimit_2017,UpperLimit_2018)
LowerLimit_2000_2018<-c(LowerLimit_2000,LowerLimit_2001,LowerLimit_2002,LowerLimit_2003,LowerLimit_2004,LowerLimit_2005,LowerLimit_2006,LowerLimit_2007,LowerLimit_2008,LowerLimit_2009,LowerLimit_2010,LowerLimit_2011,LowerLimit_2012,LowerLimit_2013,LowerLimit_2014,LowerLimit_2015,LowerLimit_2016,LowerLimit_2017,LowerLimit_2018)

#Data frame
df<-data.frame(Years=c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"), Mean=c(mean_years),CI_Upper_Limit=UpperLimits_2000_2018,CI_Lower_Limit=LowerLimit_2000_2018)
print(df)
plot.default(df$Years,df$Mean)

getwd()
write.csv(df,"C:\\Users\\ditsaxen\\Desktop\\Personal\\ISB CBA\\Projects\\GoldPrices.csv",row.names=FALSE)


























