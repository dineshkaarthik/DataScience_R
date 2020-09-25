
housesalesdata <- read.csv('housesalesdata.csv')
housefeaturesdata <- read.csv('housefeatures.csv')
housedata <- merge(housesalesdata, housefeaturesdata, by="ID")
# We can write the merged dataset to a csv file with write.csv command
write.csv(housedata, file = "housedatacomplete.csv")



# See summary

summary(housedata)

#See data distribution of numeric variables using hist command
hist(housedata$SalePrice)
# See distribution of categorical variable using table or prop.table command
table(housedata$SaleType)
prop.table(table(housedata$SaleType))


#See data distribution using hist command with other arguments
#Turn off scientific notation. To turn it on, set scipen=0
#Go through hist() command in detail in google to understand the arguments
options(scipen = 999) 
hist(housedata$SalePrice, main="House Price Histogram", 
     xlab = "Sale Price", ylab = "Count", labels = T, 
     xlim = c(0, 500000), ylim=c(0,500), 
     include.lowest = T, axes = T, col="orange")

# See other statistics like mean, median, standard deviation, correlation
# Notice that we have NA in our data.
# na.rm=T will ignore NAs in your data and give you the output
# If you do not use na.rm=T, it will consider NA and
# and will not be able to give you the output value because
# any number + NA = NA
mean(housedata$DistFromStreet, na.rm = T)
median(housedata$DistFromStreet, na.rm = T)
sd(housedata$DistFromStreet, na.rm = T)
# Finding correlation between 2 numeric variable
cor(housedata$DistFromStreet, housedata$SalePrice, use='complete.obs')

# Filter out only numeric coluns
numeric_df <- Filter(is.numeric, housedata)
names(numeric_df)

# Finding correlation of all numeric variables in the dataset

cor<-cor(numeric_df[,-1], use = 'complete.obs')
write.csv(cor, 'cormatrix.csv')
plot(numeric_df[,2:6])
plot(numeric_df[,c(2,4,6,8)])



# Study your data
# Notice that some categorical variables got imported as integer/numeric
# Like, HouseType & HouseCondition
# So change it to factor variable
housedata$HouseType <- as.factor(housedata$HouseType)
housedata$HouseCondition <- as.factor(housedata$HouseCondition)
# See carpet are is numeric
housedata$CarpetArea <- as.numeric(housedata$CarpetArea)

# Also we will not need variable ID 
# So we can remove it from our dataset
# Index of ID is 1
# So we use -1 to remove that variable
housedata <- housedata[,-1]




# Missing value treatment
# Option 1: For numeric variables, fill in with median

# We will be seeing another way of treating missing values.
housedata$DistFromStreet <- ifelse(is.na(housedata$DistFromStreet), 
                                   median(housedata$DistFromStreet, na.rm = T),
                                   housedata$DistFromStreet
                                   )
# See summary after treating missing values with median
# You will notice that NAs are gone
summary(housedata)


# Option 2: 
# aggregate() is only to view mean of CarpetArea, grouped by HouseType
# This is only an example. You can group by other categories as well
# Why are we doing this?
# Instead of taking an overall mean/median, we are finding mean by 
# HouseType and then try to
# fill in the mean by group
aggregate(data = housedata, housedata$CarpetArea ~ housedata$HouseType, median, na.rm = TRUE)

# Create a vector of average carpet area
# After running this syntax, run ave_carepetarea and see what goes into ave_carepetarea
ave_carepetarea <- ave(housedata$CarpetArea, housedata$HouseType,
                       FUN = function(x) median(x, na.rm = TRUE))

# You will see it has created a vector of average of sales price
# by Each type of house (number of elements in this vector is same as
# number of observations in the data set)
ave_carepetarea


# if the values in CarpetArea is NA, 
# replace with the values from ave_carepetarea
# else replace with the same value of CarpetArea
housedata$CarpetArea <- ifelse(is.na(housedata$CarpetArea), ave_carepetarea, 
                               housedata$CarpetArea)
summary(housedata)



# Distribution of sale price by Zone, Landmark, Sale Type, House Type
library(RColorBrewer)
boxplot(housedata$SalePrice ~ housedata$Zone, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Zonewise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$SalePrice ~ housedata$Landmark, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Landmark-wise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$SalePrice ~ housedata$SaleType, data=housedata, horizontal = FALSE,
        xlab="Zone", ylab="Sale Price", main="Sale Type wise Sale Price",
        col=brewer.pal(5, "Set3"))

boxplot(housedata$CarpetArea ~ housedata$HouseType, data=housedata, horizontal = FALSE,
        xlab="House Type", ylab="Carpet Area", main="Carepet Area by House Type",
        col=brewer.pal(5, "Set3"))



# A few examples of Feature engineering
# Create Age of building as on current year
# Find the current year from Sys.Date()
currentyear = format(Sys.Date(), "%Y")
currentyear
# You will notice that currentyear is coming up as a tring
# so convert currentyear into numeric
currentyear <- as.numeric(currentyear)

# Find the difference between current year and builtyear
# to calculate the age of the building
housedata$buildingage <- currentyear - housedata$BuiltYear


# Create Age of building when sold
housedata$age_when_sold <- housedata$YearSold - housedata$BuiltYear

# Bin Garage Area into 3 segments - create a new variable
# You can create binning using ifelse statement
# or if you have many bins to be created use cut()
housedata$binneddist <- cut(housedata$DistFromStreet, 
                            c(0,10,20,30,40,50,60,70,80))
table(housedata$binneddist)
prop.table(table(housedata$binneddist))


############### End of Code ###############






