# Done in this file : Segmenting data on the basis of the 4 Groups of Advertisements

# DO THIS : 
# Plot the reach and CTR of the different type of unique Level2 ads in group1_data and other
# This helps us in finding which Advertisements have been the major contributor to our Revenue share.
# PLOT on the basis of these unique Level2 Categories : i.e. reach/CTR/Revenue from each of these Specific Ads and that too categorically


a<- read.csv("Formatted_Final.csv", header=TRUE)

## Segmenting the Ads on the basis of their Field, 
## Dividing the whole data set on the basis of Grouping Level-1 : 4 Groups
#	 au bi br ca et hl jb mb pc re


# ================ Group 1 Analysis ==========
# Group1 :  Vehicles : AU, BI, CA
#au : Automobile	auto <- subset(a, a$Level1 == "au")
#bi : Bike			bike <- subset(a, a$Level1 == "bi")
#ca : Cars			cars <- subset(a, a$Level1 == "ca")

group1 <- c("au","bi","ca")
group1_Data <- a[grepl(paste(group1, collapse = "|"), a$Level1, ignore.case = TRUE), ]
# Removing the Rows which have Clicks =0, as these are not the priortity as of now.
group1_Data <- group1_Data[group1_Data$Clicks!=0,]
group1_Data <- group1_Data[with(group1_Data, order(-Actions, -Clicks, -Reach)), ]

# length(unique(group1_Data$Level2))
# [1] 55


group1_df <- as.data.frame(as.character(group1_Data$Level1))
colnames(group1_df) <- "Level1"
group1_df$Level2 <- NA
group1_df$Level2 <- group1_Data$Level2
# Column for Reach
group1_df$Reach <- NA
group1_df$Reach <- group1_Data$Reach
group1_df$Reach <- group1_df$Reach*(10^-3)

# Column for Impressions
group1_df$Impressions <- NA
group1_df$Impressions <- group1_Data$Impressions
# # Column for Spend
group1_df$Spend <- NA
group1_df$Spend <- group1_Data$Spend
# Column for Clicks
group1_df$Clicks <- NA
group1_df$Clicks <- group1_Data$Clicks
# # Column for Actions
group1_df$Actions <- NA
group1_df$Actions <- group1_Data$Actions

group1_adwise <- aggregate(.~Level1+Level2,data=group1_df,sum)
group1_adwise$Level1 <- as.character(group1_adwise$Level1)
group1_adwise$Level2 <- as.character(group1_adwise$Level2)
group1_adwise$CTR_mile <- NA
group1_adwise$CTR_mile <- (group1_adwise$Clicks*1000)/group1_adwise$Impressions

# COMPARING 3 MODELS on the basis of dates :
group1_adwise$CPC <- NA
group1_adwise$CPC <- group1_adwise$Spend/group1_adwise$Clicks
group1_adwise$CPA <- NA
group1_adwise$CPA <- group1_adwise$Spend/group1_adwise$Actions
group1_adwise$CPM <- NA
group1_adwise$CPM <- (group1_adwise$Spend*1000)/group1_adwise$Impressions

library(ggplot2)
library(reshape)

group1_adwise_ca <- subset(group1_adwise, group1_adwise$Level1 == "ca")
group1_adwise_bi_au <- subset(group1_adwise, group1_adwise$Level1 != "ca")

png("Group1_ca_Reach.png",width = 1000, height = 1000)
plot(1:44, group1_adwise_ca$Reach , type="h", lwd=2, lend="square", xaxt="n", xlab="Level-2 Type", main="Cars Advertisement : Group1", ylab="Reach (*10^3)")
axis(1, 1:44, labels= group1_adwise_ca$Level2, las=2)
dev.off() 

png("Group1_bi_au_Reach.png",width = 640, height = 640)
plot(1:11, group1_adwise_bi_au$Reach , type="h", lwd=2, lend="square", xaxt="n", xlab="Level-2 Type", main="Bikes and Automobile Advertisement : Group1", ylab="Reach (*10^3)")
axis(1, 1:11, labels= group1_adwise_bi_au$Level2, las=2)
dev.off()

group1_dff <- data.frame(1:length(unique(group1_adwise$Level2)), group1_adwise$CPC, group1_adwise$CPA, group1_adwise$CPM)
colnames(group1_dff) <- c("Num", "CPC","CPA","CPM")
group1_dff <- as.data.frame(group1_dff)
#group1_dff$Level2 <- as.character(group1_dff$Num)
group1_dff <- melt(group1_dff, id="Num", variable_name="Models")
group1_dff$Models <- as.character(group1_dff$Models)
# GGPLOT
png("Group1_Revenue.png",width = 1000, height = 1000)
g<- ggplot(group1_dff, aes(Num,value)) + geom_line(aes(colour = Models))
g+labs(x="Level-2 Ads in Group-1")+labs(y="Revenue Value in 3 Models")+scale_x_discrete(breaks = 1:length(unique(group1_adwise$Level2)), labels = group1_adwise$Level2)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()


# ================ Group 2 Analysis ==========
# Group2 :  Electronic Accessories : ET, HL, MB
#et : electronic items		elec <- subset(a, a$Level1 == "et")
#hl : watches				wtch <- subset(a, a$Level1 == "hl")
#mb: Mobile phones			mobi <- subset(a, a$Level1 == "mb")

group2 <- c("et","hl","mb")
group2_Data <- a[grepl(paste(group2, collapse = "|"), a$Level1, ignore.case = TRUE), ]
# Removing the Rows which have Clicks =0, as these are not the priortity as of now.
group2_Data <- group2_Data[group2_Data$Clicks!=0,]
group2_Data <- group2_Data[with(group2_Data, order(-Actions, -Clicks, -Reach)), ]

#length(unique(group2_Data$Level2))
#[1] 22


group2_df <- as.data.frame(as.character(group2_Data$Level1))
colnames(group2_df) <- "Level1"
group2_df$Level2 <- NA
group2_df$Level2 <- group2_Data$Level2
# Column for Reach
group2_df$Reach <- NA
group2_df$Reach <- group2_Data$Reach
group2_df$Reach <- group2_df$Reach*(10^-3)

# Column for Impressions
group2_df$Impressions <- NA
group2_df$Impressions <- group2_Data$Impressions
# # Column for Spend
group2_df$Spend <- NA
group2_df$Spend <- group2_Data$Spend
# Column for Clicks
group2_df$Clicks <- NA
group2_df$Clicks <- group2_Data$Clicks
# # Column for Actions
group2_df$Actions <- NA
group2_df$Actions <- group2_Data$Actions

group2_adwise <- aggregate(.~Level1+Level2,data=group2_df,sum)
group2_adwise$Level1 <- as.character(group2_adwise$Level1)
group2_adwise$Level2 <- as.character(group2_adwise$Level2)
group2_adwise$CTR_mile <- NA
group2_adwise$CTR_mile <- (group2_adwise$Clicks*1000)/group2_adwise$Impressions

# COMPARING 3 MODELS on the basis of dates :
group2_adwise$CPC <- NA
group2_adwise$CPC <- group2_adwise$Spend/group2_adwise$Clicks
group2_adwise$CPA <- NA
group2_adwise$CPA <- group2_adwise$Spend/group2_adwise$Actions
group2_adwise$CPM <- NA
group2_adwise$CPM <- (group2_adwise$Spend*1000)/group2_adwise$Impressions

library(ggplot2)
library(reshape)

png("Group2_Reach.png",width = 1000, height = 1000)
plot(1:length(unique(group2_adwise$Level2)), group2_adwise$Reach , type="h", lwd=2, lend="square", xaxt="n", xlab="Level-2 Type", main="Electronics Advertisement : Group2", ylab="Reach (*10^3)")
axis(1, 1:length(unique(group2_adwise$Level2)), labels= group2_adwise$Level2, las=2)
dev.off() 

group2_dff <- data.frame(1:length(unique(group2_adwise$Level2)), group2_adwise$CPC, group2_adwise$CPA, group2_adwise$CPM)
colnames(group2_dff) <- c("Num", "CPC","CPA","CPM")
group2_dff <- as.data.frame(group2_dff)
group2_dff <- melt(group2_dff, id="Num", variable_name="Models")
group2_dff$Models <- as.character(group2_dff$Models)
# GGPLOT
png("Group2_Revenue.png",width = 1000, height = 1000)
g<- ggplot(group2_dff, aes(Num,value)) + geom_line(aes(colour = Models))
g+labs(x="Level-2 Ads in Group-2")+labs(y="Revenue Value in 3 Models")+scale_x_discrete(breaks = 1:length(unique(group2_adwise$Level2)), labels = group2_adwise$Level2)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()


# ================ Group 3 Analysis ==========
# Group3 :  Jobs : JB
#jb : job types				jobs <- subset(a, a$Level1 == "jb")

group3 <- c("jb")
group3_Data <- a[grepl(paste(group3, collapse = "|"), a$Level1, ignore.case = TRUE), ]
# Removing the Rows which have Clicks =0, as these are not the priortity as of now.
group3_Data <- group3_Data[group3_Data$Clicks!=0,]
group3_Data <- group3_Data[with(group3_Data, order(-Actions, -Clicks, -Reach)), ]

#length(unique(group3_Data$Level2))
#[1] 15

group3_df <- as.data.frame(as.character(group3_Data$Level1))
colnames(group3_df) <- "Level1"
group3_df$Level2 <- NA
group3_df$Level2 <- group3_Data$Level2
# Column for Reach
group3_df$Reach <- NA
group3_df$Reach <- group3_Data$Reach
group3_df$Reach <- group3_df$Reach*(10^-3)

# Column for Impressions
group3_df$Impressions <- NA
group3_df$Impressions <- group3_Data$Impressions
# # Column for Spend
group3_df$Spend <- NA
group3_df$Spend <- group3_Data$Spend
# Column for Clicks
group3_df$Clicks <- NA
group3_df$Clicks <- group3_Data$Clicks
# # Column for Actions
group3_df$Actions <- NA
group3_df$Actions <- group3_Data$Actions

group3_adwise <- aggregate(.~Level1+Level2,data=group3_df,sum)
group3_adwise$Level1 <- as.character(group3_adwise$Level1)
group3_adwise$Level2 <- as.character(group3_adwise$Level2)
group3_adwise$CTR_mile <- NA
group3_adwise$CTR_mile <- (group3_adwise$Clicks*1000)/group3_adwise$Impressions

# COMPARING 3 MODELS on the basis of dates :
group3_adwise$CPC <- NA
group3_adwise$CPC <- group3_adwise$Spend/group3_adwise$Clicks
group3_adwise$CPA <- NA
group3_adwise$CPA <- group3_adwise$Spend/group3_adwise$Actions
group3_adwise$CPM <- NA
group3_adwise$CPM <- (group3_adwise$Spend*1000)/group3_adwise$Impressions

library(ggplot2)
library(reshape)

png("Group3_Reach.png",width = 1000, height = 1000)
plot(1:length(unique(group3_adwise$Level2)), group3_adwise$Reach , type="h", lwd=2, lend="square", xaxt="n", xlab="Level-2 Type", main="Jobs Advertisement : Group3", ylab="Reach (*10^3)")
axis(1, 1:length(unique(group3_adwise$Level2)), labels= group3_adwise$Level2, las=2)
dev.off() 

group3_dff <- data.frame(1:length(unique(group3_adwise$Level2)), group3_adwise$CPC, group3_adwise$CPA, group3_adwise$CPM)
colnames(group3_dff) <- c("Num", "CPC","CPA","CPM")
group3_dff <- as.data.frame(group3_dff)
group3_dff <- melt(group3_dff, id="Num", variable_name="Models")
group3_dff$Models <- as.character(group3_dff$Models)
# GGPLOT
png("Group3_Revenue.png",width = 1000, height = 1000)
g<- ggplot(group3_dff, aes(Num,value)) + geom_line(aes(colour = Models))
g+labs(x="Level-2 Ads in Group-3")+labs(y="Revenue Value in 3 Models")+scale_x_discrete(breaks = 1:length(unique(group3_adwise$Level2)), labels = group3_adwise$Level2)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()



# ================ Group 4 Analysis ==========
# Group4 :  Other Sales : RE, BR, PC
#re : rent, sale, land			rent <- subset(a, a$Level1 == "re")
#br : mixed type 				bore <- subset(a, a$Level1 == "br")
#pc : pets - dog/cat		pets <- subset(a, a$Level1 == "pc")

group4 <- c("re","br","pc")
group4_Data <- a[grepl(paste(group4, collapse = "|"), a$Level1, ignore.case = TRUE), ]
# Removing the Rows which have Clicks =0, as these are not the priortity as of now.
group4_Data <- group4_Data[group4_Data$Clicks!=0,]
group4_Data <- group4_Data[with(group4_Data, order(-Actions, -Clicks, -Reach)), ]


group4_df <- as.data.frame(as.character(group4_Data$Level1))
colnames(group4_df) <- "Level1"
group4_df$Level2 <- NA
group4_df$Level2 <- group4_Data$Level2
# Column for Reach
group4_df$Reach <- NA
group4_df$Reach <- group4_Data$Reach
group4_df$Reach <- group4_df$Reach*(10^-3)

# Column for Impressions
group4_df$Impressions <- NA
group4_df$Impressions <- group4_Data$Impressions
# # Column for Spend
group4_df$Spend <- NA
group4_df$Spend <- group4_Data$Spend
# Column for Clicks
group4_df$Clicks <- NA
group4_df$Clicks <- group4_Data$Clicks
# # Column for Actions
group4_df$Actions <- NA
group4_df$Actions <- group4_Data$Actions

group4_adwise <- aggregate(.~Level1+Level2,data=group4_df,sum)
group4_adwise$Level1 <- as.character(group4_adwise$Level1)
group4_adwise$Level2 <- as.character(group4_adwise$Level2)
group4_adwise$CTR_mile <- NA
group4_adwise$CTR_mile <- (group4_adwise$Clicks*1000)/group4_adwise$Impressions

# COMPARING 3 MODELS on the basis of dates :
group4_adwise$CPC <- NA
group4_adwise$CPC <- group4_adwise$Spend/group4_adwise$Clicks
group4_adwise$CPA <- NA
group4_adwise$CPA <- group4_adwise$Spend/group4_adwise$Actions
group4_adwise$CPM <- NA
group4_adwise$CPM <- (group4_adwise$Spend*1000)/group4_adwise$Impressions

library(ggplot2)
library(reshape)

png("Group4_Reach.png",width = 1000, height = 1000)
plot(1:length(unique(group4_adwise$Level2)), group4_adwise$Reach , type="h", lwd=2, lend="square", xaxt="n", xlab="Level-2 Type", main="Miscellaneous Advertisement : Group4", ylab="Reach (*10^3)")
axis(1, 1:length(unique(group4_adwise$Level2)), labels= group4_adwise$Level2, las=2)
dev.off() 

group4_dff <- data.frame(1:length(unique(group4_adwise$Level2)), group4_adwise$CPC, group4_adwise$CPA, group4_adwise$CPM)
colnames(group4_dff) <- c("Num", "CPC","CPA","CPM")
group4_dff <- as.data.frame(group4_dff)
group4_dff <- melt(group4_dff, id="Num", variable_name="Models")
group4_dff$Models <- as.character(group4_dff$Models)
# GGPLOT
png("Group4_Revenue.png",width = 1000, height = 1000)
g<- ggplot(group4_dff, aes(Num,value)) + geom_line(aes(colour = Models))
g+labs(x="Level-2 Ads in Group-4")+labs(y="Revenue Value in 3 Models")+scale_x_discrete(breaks = 1:length(unique(group4_adwise$Level2)), labels = group4_adwise$Level2)+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
dev.off()


