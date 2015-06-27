# Time series Analysis:
# Segmenting data on the basis of different dates:

# Done in this file:
# Plotting of Dates vs CTR
# Plotting of Dates vs Revenue from 3 Models

# Required : 'ggplot2' and 'reshape' Packages

a<- read.csv("Formatted_Final.csv", header=TRUE)

# Including columns : Date_Type, Reach, Impressions, Spend, Clicks, Actions
time_series <- as.data.frame(a$Date_Type)
time_series$Reach <- a$Reach
time_series$Impressions <- a$Impressions
time_series$Spend <- a$Spend
time_series$Clicks <- a$Clicks
time_series$Actions <- a$Actions
colnames(time_series)[1] <- "Date_Type"

# Converting Type to Date, else it won't help in PLOTTING the data
time_series$Date_Type <- as.Date(time_series$Date_Type)

dateWise_series <- aggregate(.~Date_Type,data=time_series,sum)
# TAKE SCREENSHOT
# dim(dateWise_series)
# 25  6

#dateWise_series$Date_Type <- as.Date(dateWise_series$Date_Type)

# PLOTTING : Click Through Rate (CTR) date wise:
dateWise_series$CTR_mile <- NA
dateWise_series$CTR_mile <- (dateWise_series$Clicks*1000)/dateWise_series$Impressions

png("Date_CTR.png",width = 720, height = 720)
plot(dateWise_series$Date_Type, dateWise_series$CTR_mile, type="h", xlab="Dates", ylab="Click Through Rate(*10^3)", lwd=2, col="blue")
dev.off() 


# COMPARING 3 MODELS on the basis of dates :
dateWise_series$CPC <- NA
dateWise_series$CPC <- dateWise_series$Spend/dateWise_series$Clicks

dateWise_series$CPA <- NA
dateWise_series$CPA <- dateWise_series$Spend/dateWise_series$Actions

dateWise_series$CPM <- NA
dateWise_series$CPM <- (dateWise_series$Spend*1000)/dateWise_series$Impressions

# Install the required packages : 'ggplot2' and 'reshape', after that
library(ggplot2)
library(reshape)

# Changed name from df to date_df
date_df <- data.frame(dateWise_series$Date_Type, dateWise_series$CPC, dateWise_series$CPA, dateWise_series$CPM)
colnames(date_df) <- c("Date_Type", "CPC","CPA","CPM")
date_df <- as.data.frame(date_df)
# TAKE SCREENSHOT

# Melting changes the structure of the data frame from 25x4 to 75x3(Date, Models, Value). 
# It is just putting all the values one below the other
date_df <- melt(date_df, id="Date_Type", variable_name="Models")

# GGPLOT
png("Date_Models.png",width = 650, height = 700)
g<- ggplot(date_df, aes(Date_Type,value)) + geom_line(aes(colour = Models))
g+labs(x="Dates")+labs(y="Value in 3 Models")
dev.off()

