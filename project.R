library(readr)

#reading data from the csv
train_data = read_csv("./train-data.csv")
test_data = read_csv("./test-data.csv")

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
  data$Power = remove_units(data$Power)
  data$Mileage = remove_units(data$Mileage)
  data$Engine = remove_units(data$Engine)
  data = na.omit(data)
  data = data[-which(data$Mileage == 0),]
  data = data[,-c(1,2)]
  return(data)
}

loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

train_data = clean_data(train_data)
test_data = clean_data(test_data)

pairs(train_data[c("Power","Mileage","Engine","Price")])

plot(log(Price) ~ log(Power), data = train_data)
train_data_nm = train_data
mileage_transform = function(x) {
  return(-x)
}
plot(log(Price) ~ mileage_transform(Mileage), data = train_data_nm)

plot(log(Price) ~ log(Engine), data = train_data)

train_data_nm = train_data
train_data_nm = train_data[-which.max(train_data$Kilometers_Driven),]
plot(log(Price) ~ log(Kilometers_Driven), data = train_data_nm)

train_data = train_data[,-c(1,2)]
model = lm(Price ~ ., data =  train_data)
model_aic = step(model, direction = "backward")
anova(model_aic, model)

log_model = lm(log(Price) ~ .,data =  train_data)
log_model_aic = step(log_model, direction = "backward")

loocv_rmse(log_model)
loocv_rmse(log_model_aic)
sqrt(mean(resid(log_model_aic)^2))

sqrt(mean(resid(model_aic)^2))
loocv_rmse(model_aic)


log_model_int = lm(log(Price) ~ (.)^2, data = train_data)
log_model_int_aic = step(log_model_int, direction = "backward")
loocv_rmse(log_model_int_aic)
anova(log_model_aic, log_model_int_aic)
sqrt(mean(resid(log_model_int_aic)^2))

price_test_hat = exp(predict(log_model_int_aic, newdata = test_data))
