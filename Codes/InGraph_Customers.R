# Analyszing on the basis of Weekday or Weekend Types, for different Genders :
# Done in this file :
# 1. On which weekday/end -> For Male gender -> Which age group ->  Which date -> Plotted REACH, CTR, Revenue Model(CPA,CPM,CPC) w.r.t. Dates

# Required : 'ggplot2' and 'reshape' Packages

a<- read.csv("Formatted_Final.csv", header=TRUE)

# Breaking the data on the basis of the genders : NOT USED HERE
male_data = subset(a, a$Gender=="male")
female_data = subset(a, a$Gender=="female")
unknown_data = subset(a, a$Gender=="unknown")
 
# ======== WEEKEND DATA ==========
endIndex <- which(a$Weekday_end %in% "Weekend")				# This is all of Weekend Data
endRows <- a[endIndex, ]
# OR Use this command :
# endRows <- a[grepl(paste(c("Saturday","Sunday"), collapse = "|"), a$Day_Type, ignore.case=TRUE), ]

# Further breaking of the data on the basis of Gender
male_end = subset(endRows, endRows$Gender=="male")			# NOW PLAY WITH THIS DATA	
female_end = subset(endRows, endRows$Gender=="female")
unknown_end = subset(endRows, endRows$Gender=="unknown")

# ======== WEEKDAY DATA ==========
midIndex <- which(a$Weekday_end %in% "Weekday")				# This is all of Weekday Data
midRows <- a[midIndex, ]

male_mid = subset(midRows, midRows$Gender=="male")
female_mid = subset(midRows, midRows$Gender=="female")
unknown_mid = subset(midRows, midRows$Gender=="unknown")


#	============================== Part 1 : Male ============================== 
# PUT aggregate function here, to select the males on the basis of different age groups, 
male_end_df <- as.data.frame(male_end$Age)
colnames(male_end_df) <- "Age"
male_end_df$Date_Type <- NA
male_end_df$Date_Type <- male_end$Date_Type
# Column for Reach
male_end_df$Reach <- NA
male_end_df$Reach <- male_end$Reach
# Column for Impressions
male_end_df$Impressions <- NA
male_end_df$Impressions <- male_end$Impressions
# # Column for Spend
male_end_df$Spend <- NA
male_end_df$Spend <- male_end$Spend
# Column for Clicks
male_end_df$Clicks <- NA
male_end_df$Clicks <- male_end$Clicks
# # Column for Actions
male_end_df$Actions <- NA
male_end_df$Actions <- male_end$Actions


# This has data of different age groups of males on the basis of dates : which are weekends	
male_end_order <- aggregate(.~Date_Type+Age,data=male_end_df,sum)

# Removing the cases where Clicks and Actions are '0', as these cases won't be useful
male_end_order <- male_end_order[male_end_order$Actions!=0,]
male_end_order <- male_end_order[male_end_order$Clicks!=0,]
male_end_order$Reach <- male_end_order$Reach*10^(-5)

# Making desired columns
male_end_order$CTR_mile <- NA
male_end_order$CTR_mile <- (male_end_order$Clicks*1000)/male_end_order$Impressions

# COMPARING 3 MODELS on the basis of dates :
male_end_order$CPC <- NA
male_end_order$CPC <- male_end_order$Spend/male_end_order$Clicks
male_end_order$CPA <- NA
male_end_order$CPA <- male_end_order$Spend/male_end_order$Actions
male_end_order$CPM <- NA
male_end_order$CPM <- (male_end_order$Spend*1000)/male_end_order$Impressions
# Now we have the properly ordered data  : male_end_order

# Analysis : On which weekday/end -> For which gender -> Which age group ->  Which date -> how many REACH, CPC, CPA, CPM
library(ggplot2)
library(reshape)

male_end_order$Date_Type <- as.Date(male_end_order$Date_Type)
male_end_order$Age <- as.character(male_end_order$Age)

png("Weekend_Male_Reach.png",width = 720, height = 720)
g<- ggplot(male_end_order, aes(Date_Type,Reach)) + geom_line(aes(colour = Age))
g+labs(x="Weekend Dates")+labs(y="Reach Value(*10^-5) for Males by Age Groups on Weekends")
dev.off()

# Depending upon the revenue model, we can check their revenue :
png("Weekend_Male_CPC.png",width = 720, height = 720)
g<- ggplot(male_end_order, aes(Date_Type,CPC)) + geom_line(aes(colour = Age))
g+labs(x="Weekend Dates")+labs(y="CPC Value for Males by Age Groups on Weekends")
dev.off()

png("Weekend_Male_CPA.png",width = 720, height = 720)
g<- ggplot(male_end_order, aes(Date_Type,CPA)) + geom_line(aes(colour = Age))
g+labs(x="Weekend Dates")+labs(y="CPA Value for Males by Age Groups on Weekends")
dev.off()

png("Weekend_Male_CPM.png",width = 720, height = 720)
g<- ggplot(male_end_order, aes(Date_Type,CPM)) + geom_line(aes(colour = Age))
g+labs(x="Weekend Dates")+labs(y="CPM Value for Males by Age Groups on Weekends")
dev.off()

png("Weekend_Male_CTR.png",width = 720, height = 720)
g<- ggplot(male_end_order, aes(Date_Type,CTR_mile)) + geom_line(aes(colour = Age))
g+labs(x="Weekend Dates")+labs(y="CTR(*10^3) Value for Males by Age Groups on Weekends")
dev.off()

# ========= PART 1 : FEMALE and UNKNOWN can be done similarly for Weekends ====== 

# ========= PART 2 : MALE, FEMALE and UNKNOWN can be done similarly for Weekdays ====== 

