library(gdata)
path_for_data <-  "./data.txt"
setwd("C:/Users/Vikraant Pai/workspace/read_fixed_width_file_with_R")

#fetching the values from the fwf dataset, skipping the column headers and mentioning the size using the column sizes 
df_weather_data <- read.fwf(path_for_data, c(6, 5, 3, 3, 4, 3, 5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3), header = F, skip=33 , n=2455)

#Adding the column names using the dataset
colnames(df_weather_data) <- c("INDEX", "YEAR",  "MN",  "HR",  "RH",  "NO1",  "MWS",  "W1",  "W2", "W3",  "W4",  "NO2",  "N", "NE",  "E",  "SE",  "S",  "SW",  "W", "NW",  "CA",  "VA",  "NO3")
#We will refer to column names starting from RH
col_names_list <- c("RH",  "NO1",  "MWS",  "W1",  "W2", "W3",  "W4",  "NO2",  "N", "NE",  "E",  "SE",  "S",  "SW",  "W", "NW",  "CA",  "VA",  "NO3")

#Splitting the data into 830 dataset and 1730 dataset 
df_weather_data_830  <- df_weather_data[ df_weather_data$HR == 3, ]
df_weather_data_830
summary(df_weather_data_830)
df_weather_data_1730 <- df_weather_data[df_weather_data$HR == 12,]
df_weather_data_1730
summary(df_weather_data_1730)

#Finding means for 830 data set and replacing them with means
df_weather_data_830_means <-  aggregate( df_weather_data_830[, 5:23] , by = list(df_weather_data_830$MN), FUN=function(x) return(mean(x, na.rm = T)) )
df_weather_data_830_means
for(column_name in col_names_list){
  for( month_number in c(1:12) ){
  if( nrow(
  df_weather_data_830[ which( is.na(df_weather_data_830[column_name]) & df_weather_data_830$MN == month_number),][column_name]
    )
    )
    {
  
    df_weather_data_830[ which( is.na(df_weather_data_830[column_name]) & df_weather_data_830$MN == month_number) ,][column_name]  <- df_weather_data_830_means[column_name][month_number, ]
    }   
  }
}
df_weather_data_830

#Finding means for 1730 data set and replacing them with means
df_weather_data_1730_means <-  aggregate( df_weather_data_1730[, 5:23] , by = list(df_weather_data_1730$MN), FUN=function(x) return(mean(x, na.rm = T)) )
df_weather_data_1730_means
for(column_name in col_names_list){
  for( month_number in c(1:12) ){
    if( nrow(
      df_weather_data_1730[ which( is.na(df_weather_data_1730[column_name]) & df_weather_data_1730$MN == month_number),][column_name]
    )
    )
    {
      
      df_weather_data_1730[ which( is.na(df_weather_data_1730[column_name]) & df_weather_data_1730$MN == month_number) ,][column_name]  <- df_weather_data_1730_means[column_name][month_number, ]
    }   
  }
}
df_weather_data_1730



#Checking if the count of line items are same in all the three data sets
if( length(df_weather_data_1730[,1]) + length(df_weather_data_830[,1]) ==  length(df_weather_data[,1]) ) {

  sprintf("Num of line items are same")
  
} else {
  
  sprintf("There is some error in calculation")
  
}

#Writing the data in csv
write.fwf(df_weather_data_1730, file = "./weather_data_1730.txt")
write.fwf(df_weather_data_830, file = "./weather_data_830.txt")
