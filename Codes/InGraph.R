# Processing the Data : 
# Making desired columns in it

# Make these 4 Columns in the Excel File using normal excel calculations: 
# CTR, CPC, CPA, CPM : Order in this manner only
# Convert the file to CSV(Comma Separated Values) as "Trial_Assignment.csv"

# Save 'Assignment_Data_Calculations.xlsx' file as "Assignment_File.csv" format

# setwd("E:\\InterviewFiles\\InGraph\\R")
a<- read.csv("Assignment_File.csv", header=TRUE)

# If in case, rows till end are added because of excel operations
a <- a[1:36012,]

# Keep this in mind : NA is taken as unique in unique(a[,14])
#a[2,14]
#[1] NA
#class(a[2,14])
#[1] "numeric"

#	as.character(unlist(unique(a[,6])))

# Change the type of the date column : factor to Character
a[,4] <- as.character(a[,4])

# Add columns for Day and Dates
a$Day_Type <- NA
a$Date_Type <- NA
# This will transform the dates into "YYYY-MM-DD" format and then set the type of the column to 'Date'
a$Date_Type <- strptime(a[,4], "%m/%d/%Y")		
a$Date_Type <- as.Date(a$Date_Type)

# This will add the Day type to the Date
a$Day_Type <-  weekdays(as.Date(a$Date_Type))


# Adding column for Datatype as Weekday/Weekend
a$Weekday_end <- NA
# This might take a while .. a more than while ... and a little more
for (i in 1:dim(a)[1]) {
    if (a$Day_Type[i] == "Saturday" || a$Day_Type[i] == "Sunday") {
        a$Weekday_end[i] <- "Weekend"
    } else {
        a$Weekday_end[i] <- "Weekday"
    }
}

# Confirmation 
a$Day_Type[25187]
#[1] "Sunday"
a$Weekday_end[25187]
#[1] "Weekend"
 
# Generate a .CSV file here, so as to prevent further calculations:
write.csv(file="Formatted_Final.csv", x=a) 

 # Column names present in it :
# [1] "Level1"      "Level2"      "Ad"          "Date"        "Age"        "Gender"      "Reach"       "Impressions" "Spend"       "Clicks"     
# [11] "Actions"     "CTR"         "CPC"         "CPA"        "CPM"        "Date_Type"    "Day_Type"    "Weekday_end"

