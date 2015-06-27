# Day wise Analysis :
# Segmenting data on the basis of different days:

# Done in this file:
# Plotting of Days vs CTR
# Plotting of Days vs Revenue from 3 Models

# Required : 'ggplot2' and 'reshape' Packages

a<- read.csv("Formatted_Final.csv", header=TRUE)

# Analyzing on the basis of Day Types:

a1 <- which(a$Day_Type %in% "Sunday")				# This is all of Sunday Data
sun_data <- a[a1, ]
a2 <- which(a$Day_Type %in% "Monday")				# This is all of Monday Data
mon_data <- a[a2, ]
a3 <- which(a$Day_Type %in% "Tuesday")				# This is all of Tuesday Data
tue_data <- a[a3, ]
a4 <- which(a$Day_Type %in% "Wednesday")				# This is all of Wednesday Data
wed_data <- a[a4, ]
a5 <- which(a$Day_Type %in% "Thursday")				# This is all of Thursday Data
thu_data <- a[a5, ]
a6 <- which(a$Day_Type %in% "Friday")				# This is all of Friday Data
fri_data <- a[a6, ]
a7 <- which(a$Day_Type %in% "Saturday")				# This is all of Saturday Data
sat_data <- a[a7, ]

# Now we have the day-wise data: 
# sun/mon/tue/wed/thu/fri/sat_data

# For analysing further, we will be taking specific columns only from the data : 
#  Impressions, Spend, Clicks, Actions

dayWise_series <- as.data.frame(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
colnames(dayWise_series) <- "Day_Type"
# Converting Type to Date, else it won't help in PLOTTING the data
dayWise_series$Day_Type <- as.character(dayWise_series$Day_Type)
dayWise_series$Day_num <- NA
dayWise_series$Day_num <- 1:7

dayWise_series$Impressions <- NA
dayWise_series$Spend <- NA
dayWise_series$Clicks <- NA
dayWise_series$Actions <- NA

# This assigns the specific values to the variables
comment(sun_data) <- "1"
comment(mon_data) <- "2"
comment(tue_data) <- "3"
comment(wed_data) <- "4"
comment(thu_data) <- "5"
comment(fri_data) <- "6"
comment(sat_data) <- "7"

for(dataframe in list(sun_data,mon_data,tue_data,wed_data,thu_data,fri_data,sat_data)) {
	dfnm <- comment(dataframe)
	dayWise_series$Impressions[as.numeric(dfnm)] <- (sum(dataframe$Impressions))	
	dayWise_series$Spend[as.numeric(dfnm)] <- (sum(dataframe$Spend))	
	dayWise_series$Clicks[as.numeric(dfnm)] <- (sum(dataframe$Clicks))	
	dayWise_series$Actions[as.numeric(dfnm)] <- (sum(dataframe$Actions))	
}


# PLOTTING : Click Through Rate (CTR) day wise:
dayWise_series$CTR_mile <- NA
dayWise_series$CTR_mile <- (dayWise_series$Clicks*1000)/dayWise_series$Impressions

png("Day_CTR.png",width = 720, height = 720)
plot(dayWise_series$Day_num , dayWise_series$CTR_mile, type="h", lwd=10, lend="square", xaxt="n", xlab="Days", ylab="Click Through Rate(*10^3)")
axis(1, 1:7, labels= dayWise_series$Day_Type)
dev.off() 


# COMPARING 3 MODELS on the basis of days :
dayWise_series$CPC <- NA
dayWise_series$CPC <- dayWise_series$Spend/dayWise_series$Clicks

dayWise_series$CPA <- NA
dayWise_series$CPA <- dayWise_series$Spend/dayWise_series$Actions

dayWise_series$CPM <- NA
dayWise_series$CPM <- (dayWise_series$Spend*1000)/dayWise_series$Impressions


# Install the required packages : 'ggplot2' and 'reshape', after that
library(ggplot2)
library(reshape)

day_df <- data.frame(dayWise_series$Day_num, dayWise_series$CPC, dayWise_series$CPA, dayWise_series$CPM)
colnames(day_df) <- c("Day_num", "CPC","CPA","CPM")
day_df <- as.data.frame(day_df)
# TAKE SCREENSHOT

# Melting changes the structure of the data frame from 25x4 to 75x3(Day, Models, Value). 
# It is just putting all the values one below the other
day_df <- melt(day_df, id="Day_num", variable_name="Models")

# GGPLOT
png("Day_Models.png",width = 720, height = 720)
g<- ggplot(day_df, aes(Day_num,value)) + geom_line(aes(colour = Models))
g+labs(x="Days")+labs(y="Value in 3 Models")+scale_x_discrete(breaks = 1:7, labels = dayWise_series$Day_Type)
dev.off()

