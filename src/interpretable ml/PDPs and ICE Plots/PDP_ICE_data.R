# Data for PDPs and ICE Plots explantion
# Conor O'Sullivan
# 06 June 2022

# ======================
# Generate Dataset 
# ======================

new_row <- function() {
  "Generates one random row (i.e. car sale) of the feature matrix"
  
  owner_age = sample(18:75, 1)
  car_age = round(runif(1, 1, 40),2)
  km_driven = 15000 + car_age*runif(1, 0.10, 0.75)*7500 +
              runif(1, -5000, 10000)*as.integer(runif(1, 0, 1) < 0.50)  + 
              runif(1, -10000, 20000)*as.integer(runif(1, 0, 1) < 0.20)  
  
  repairs = sample(0:25, 1)
  car_type = as.integer(runif(1, 0, 1) < 0.20)
  
  #Target Variable 
  price = (0*owner_age +
             (40)*car_age*car_type + (-10)*car_age +
             100*car_type + 
             (-2)*km_driven/325 + 1000 +
             (0.4566)*repairs^3 - (23.9249)*repairs^2 + (313.3928)*repairs+ 100 +
             runif(1, 0, 400) )*3
  
  row = c(owner_age,car_age,km_driven,repairs,car_type,round(price,0))
  return(row)
}

new_row()
#Create dataset of 1000 employees
datalist = list()
for (i in 1:1000) {
  datalist[[i]] <- new_row()
}
data = do.call(rbind, datalist)
colnames(data) =  c('owner_age', 'car_age', 'km_driven', 'repairs','car_type', 'price') 

#Save table
setwd("~/Documents/git/medium-articles/data")
write.table(data, file = "PDP_ICE.csv",
            sep = "\t", row.names = F)
