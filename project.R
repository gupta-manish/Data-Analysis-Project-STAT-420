library(readr)

#reading data from the csv
train_data = read_csv("./train-data.csv")

#clean data function
clean_data = function(data) {
  #removing New  Price column
  data = data[,-13]
  remove_units = function(values) {
    remove_units_value =  function(value) {
      as.numeric(sub("\\s+\\D+$", "", value))
    }
    unlist(lapply(values, remove_units_value))
  }
  data$Location = factor(data$Location)
  data$Transmission = factor(data$Transmission)
  data$Fuel_Type = factor(data$Fuel_Type)
  data$Owner_Type = factor(data$Owner_Type)
  data$Seats = factor(data$Seats)
  data$Power = remove_units(data$Power)
  data$Mileage = remove_units(data$Mileage)
  data$Engine = remove_units(data$Engine)
  data = na.omit(data)
  return(data)
}

train_data = clean_data(train_data)


