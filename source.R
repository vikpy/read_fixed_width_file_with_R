path_for_data <-  "./data.txt"

#fetching the values from the fwf dataset, skipping the column headers and mentioning the size using the column sizes 
df_weather_data <- read.fwf(path_for_data, c(6, 5, 3, 3, 4, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), header = F, skip=1 )

#Adding the column names using the dataset
colnames(df_weather_data) <- c("INDEX", "YEAR",  "MN",  "HR",  ".RH",  "NO",  ".MWS",  "W1",  "W2", "W3",  "W4",  "NO",  ".N", "NE",  ".E",  "SE",  ".S",  "SW",  ".W", "NW",  "CA",  "VA",  "NO")

#Splitting the data into 830 dataset and 1730 dataset 
df_weather_data_830  <- df_weather_data[df_weather_data$HR == 3, ]
summary(df_weather_data_830)
df_weather_data_1730 <- df_weather_data[df_weather_data$HR == 12,]
summary(df_weather_data_1730)

#Defining fucntion to replace mean
replaceNAwithMean  <-  function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

#Replacing Misssing data with mean 
lapply(df_weather_data_830, replaceNAwithMean)
summary(df_weather_data_830)
lapply(df_weather_data_1730, replaceNAwithMean)
summary(df_weather_data_830)


#Checking if the count of line items are same in all the three data sets
if( length(df_weather_data_1730[,1]) + length(df_weather_data_830[,1]) ==  length(df_weather_data[,1]) ) {

 sprintf("Num of line items are same")
  
} else {
  
  sprintf("There is some error in calculation")
  
}

#Writing the data in csv
write.csv(df_weather_data_1730, file = "./weather_data_1730.csv")
write.csv(df_weather_data_830, file = "./weather_data_830.csv")

