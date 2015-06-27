# Done in this file :
# Segmenting data on the basis of CTR, CPA, CPC, CPM: 
# 1. Removing rows with '0' or 'NA' values in CTR, CPA, CPC, CPM
# 2. Ordering the data on the basis of : Reach( for CTR), Clicks (for CPC), Actions (for CPA)

a<- read.csv("Formatted_Final.csv", header=TRUE)

# Data of all the successful cases of CTR : 
# Data without '0' values in CTR
clear_ctr <- a[a[,12]!=0,]

# Ordering the whole data frame on the basis of CTR, decreasing order :
clear_ctr$CTR <- as.numeric(as.character(clear_ctr$CTR))
# Data without NA values in CTR
ctr_bad <- is.na(clear_ctr$CTR)
# length(ctr_bad)
clear_ctr <- clear_ctr[!ctr_bad,]
# length(clear_ctr$CTR)

# This has best representation of the data : sorting on the basis of REACH
clear_ctr <- clear_ctr[with(clear_ctr, order(-Reach)), ] 

# Poor representation of data : (Not used later)
clear_ctr1 <- clear_ctr[with(clear_ctr, order(-Actions)), ]
clear_ctr2 <- clear_ctr[with(clear_ctr, order(-CTR)), ]

# This data is the best fit for CPC model, as we have removed the rows which have '0' value for Clicks:
clear_ctr_clicks <- clear_ctr[clear_ctr$Clicks!=0,]
# Now ordering this data on the basis of 'Clicks' :
clear_ctr_clicks <- clear_ctr_clicks[with(clear_ctr_clicks, order(-Clicks)), ]

clear_ctr_actions <- clear_ctr[clear_ctr$Clicks!=0,]
# Now ordering this data on the basis of 'Clicks' :
clear_ctr_actions <- clear_ctr_actions[with(clear_ctr_actions, order(-Clicks)), ]



# Checking the dimensions, ignore these
# These are the cases where Actions!=0, These are the top in the priority list. So these should be TARGETED for CPA MODEL
dim(clear_ctr[clear_ctr$Actions!=0,])
[1] 16035    19
dim(clear_ctr[clear_ctr$Impressions!=0,])
[1] 16035    19
# These have Clicks==0, these are 2nd in the priority, because these have Clicks >0 , CTR > 0, and Actions>0. So these should be TARGETED for CPC MODEL
dim(clear_ctr[clear_ctr$Clicks!=0,])
[1] 15271    19
# There is no such case, which has Clicks=0 and Actions >0 : Verified
dim(clear_ctr[clear_ctr$Clicks==0 && clear_ctr$Actions!=0,])


# ======================
# These commands were not used.
# Data with 3 types of models : CPA, CPC, CPM : and each without the NA values.

# Data without NA values in CPC
cpc_bad <- is.na(a$CPC)
clear_cpc <- a[!cpc_bad,]

# Data without NA values in CPA
cpa_bad <- is.na(a$CPA)
clear_cpa <- a[!cpa_bad,]

