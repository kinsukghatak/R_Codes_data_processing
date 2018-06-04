library(dplyr)
install.packages("hflights")

library(hflights)

head(hflights)
summary(hflights)
nrow (hflights)
ncol(hflights)
## Let's use the tbl data format (Which is a special type of data frame 
# that helps to work and see the original dataframe)
hflights<-tbl_df(hflights)
# To understand the glimpse or gist of the tbl file
glimpse(hflights)
hflights

carrier<-hflights$UniqueCarrier


# LookUp function :  https://nicercode.github.io/blog/2013-07-09-modifying-data-with-lookup-tables/ 
#lut is the lookup table 
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
glimpse(hflights)

length(unique(hflights$UniqueCarrier))

# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier] # This is the lookup table

# Glimpse at hflights
glimpse(hflights)

