library(tidyverse)
library(ggplot2)
library(dplyr)
#install.packages("forecast")
require(forecast)
ggAcf(district_pvi$pvi)
ggPacf(district_pvi$pvi)
library(readr)



## CLEAN THE DATA
co2_data_clean <- read.csv("dataset/clean_co2.csv")

#write_csv(co2_data_clean, file = here::here("dataset", "clean_co2.csv"))

#save(co2_data_clean, file = here::here("dataset/clean_co2.RData"))
co2_data_clean <- co2_data_clean[,-1]

co2_data_clean[co2_data_clean == ".."] = NA
co2_data_clean <- co2_data_clean %>% type_convert()
co2_data_clean[is.na(co2_data_clean)] = 0


co2_data_clean <- co2_data_clean[19:50,]

for(i in 2:ncol(co2_data_clean)){
  co2_data_clean[,i][(co2_data_clean[,i]==0)] = mean(co2_data_clean[,i])
}

#Correlation matrix of all variables with all other variables
res = cor(co2_data_clean[2:13])
res

#Correlation of all other variables with CO2 emissions
cor(co2_data_clean[,-1], co2_data_clean$CO2_emissions_kt)

#plot of correlation: heatmap
heatmap(res)


#------------------------------------------------------------------------------------------

## BARLEY

barley_data_3 <- read_csv(here::here("dataset", "barley/crop_p015_t005.csv"),skip = 5)
barley_data_3<- barley_data_3[-2,3:ncol(barley_data_3)] %>% drop_na(Year) %>% head(-1)

barley_data_4 <- read_csv(here::here("dataset", "barley/crop_p016_t010.csv"),skip = 5)
barley_data_4<- barley_data_4[-2,3:ncol(barley_data_4)] %>% drop_na(Year) %>% head(-1)

barley_data <- barley_data_3%>%bind_rows(barley_data_4) 
barley_data <- barley_data %>% type_convert()

barley_co2_data <- co2_data_clean %>% left_join(barley_data, by = c("Time" = "Year"))
write_csv(co2_data_clean, file = here::here("dataset", "barley_co2_data"))
save(co2_data_clean, file = here::here("dataset/barley_co2_.RData"))
#barley_co2_data <- barley_co2_data %>% filter(CO2_emissions_kt != "..") %>% type_convert()
View(barley_co2_data)


#write_csv(barely_co2_data, file = here::here("dataset", "barley_co2_data"))

#save(barley_co2_data, file = here::here("dataset/barely_co2_data.RData"))


ggAcf(barley_co2_data$`Area harvested`)  ###use difference 

ggAcf(diff(barley_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(barley_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(barley_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot

barley_co2_data %>% ggplot(aes(y = `Area harvested`, x = (CO2_emissions_kt))) + geom_point()
barley_co2_data %>% ggplot(aes(y = `Production`, x = (CO2_emissions_kt))) + geom_point()
barley_co2_data %>% ggplot(aes(y = `Area planted`, x = (CO2_emissions_kt))) + geom_point()
barley_co2_data %>% ggplot(aes(y = `Yield per acre`, x = (CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(barley_co2_data$`Area harvested`), diff_co2 = diff(barley_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(y = diff_area, x = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between difference data, relation seems to be cubic

barley_co2_data %>% ggplot(aes(y = `Area harvested`, x = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is very weak correlation

lm1 <- (lm(`Area harvested`~ CO2_emissions_kt, barley_co2_data)) 
summary(lm1)

lm1_diff <-lm(diff_area ~ poly(diff_co2,3), diff_df)
summary(lm1_diff)

## No significant association between co2 emissions and area harvested

ggAcf(lm1$residuals)
ggAcf(lm1_diff$residuals)

#------------------------------------------------------------------------------------------

## CORN

corn_data_3 <-read_csv(here::here("dataset", "corn/crop_p031_t024.csv"),skip = 7)
corn_data_3<- corn_data_3[-2,3:ncol(corn_data_3)] %>% drop_na(Year) %>%filter(nchar(Year)==4)
colnames(corn_data_3) <- colnames(barley_data_3)
corn_data_3 <- corn_data_3[,1:7]
View(corn_data_3)

corn_data_4 <- read_csv(here::here("dataset", "corn/crop_p032_t034.csv"),skip = 7)
corn_data_4<- corn_data_4[-2,3:ncol(corn_data_4)] %>% drop_na(Year) %>%filter(nchar(Year)==4)
colnames(corn_data_4) <- colnames(barley_data_4)
corn_data_4 <- corn_data_4[,1:7]
View(corn_data_4)


corn_data <- corn_data_3%>%bind_rows(corn_data_4) %>% type_convert()
View(corn_data)
corn_co2_data <- co2_data_clean %>% left_join(corn_data, by = c("Time" = "Year"))
View(corn_co2_data)




ggAcf(corn_co2_data$`Area harvested`)  ###use difference 

ggAcf(diff(corn_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(corn_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(corn_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot


#corn_co2_data %>% ggplot(aes(x = diff(`Area harvested`), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(corn_co2_data$`Area harvested`), diff_co2 = diff(corn_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between co2 and harvested

corn_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is weak correlation here, the relation seems to be cubic

lm1 <- (lm(`Area harvested`~ poly(CO2_emissions_kt,3), corn_co2_data)) 
summary(lm1)

lm1_diff <-lm(diff_area ~ diff_co2, diff_df) 
summary(lm1_diff)

# No significant association

ggAcf(lm1$residuals)
ggAcf(lm1_diff$residuals)








#------------------------------------------------------------------------------------------


## FLAXSEED

flaxseed_data_3 <- read_csv("dataset/flaxseed/crop_p086_t049.csv", 
                            col_types = cols(`49` = col_skip(), t = col_skip(), 
                                             `Crop Production Historical Track Records: Released April 11, 2022, by the National Agricultural Statistics Service (NASS), Agricultural Statistics Board, United States Department of Agriculture (USDA).` = col_double(), 
                                             ...3 = col_double(), u = col_skip()), 
                            skip = 7)
colnames(flaxseed_data_3) <- colnames(barley_data_3)

flaxseed_data_3 <- flaxseed_data_3 %>% drop_na(Year)

View(flaxseed_data_3)
flaxseed_data_4 <- read_csv("dataset/flaxseed/crop_p087_t050.csv", 
                            col_types = cols(`50` = col_skip(), t = col_skip(), 
                                             `Crop Production Historical Track Records: Released April 11, 2022, by the National Agricultural Statistics Service (NASS), Agricultural Statistics Board, United States Department of Agriculture (USDA).` = col_double(), 
                                             ...3 = col_double(), u = col_skip()), 
                            skip = 7)
colnames(flaxseed_data_4) <- colnames(barley_data_4)

flaxseed_data_4 <- flaxseed_data_4 %>% drop_na(Year)

View(flaxseed_data_4)
flaxseed_data_3 <- flaxseed_data_3 %>% type_convert()
flaxseed_data <- flaxseed_data_3%>%bind_rows(flaxseed_data_4) 
View(flaxseed_data)

flaxseed_co2_data <- co2_data_clean %>% left_join(flaxseed_data, by = c("Time" = "Year"))


flaxseed_co2_data <- flaxseed_co2_data %>% filter(CO2_emissions_kt != "..") %>% type_convert()

View(flaxseed_co2_data)


ggAcf(flaxseed_co2_data$Area.harvested)  ###use difference 

ggAcf(diff(flaxseed_co2_data$Area.harvested)) ## no autocorrelation


# co2 emission
ggAcf(flaxseed_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(flaxseed_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot



#flaxseed_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(flaxseed_co2_data$Area.harvested), diff_co2 = diff(flaxseed_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between co2 and harvested

flaxseed_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is very strong correlation here
flaxseed_co2_data <- flaxseed_co2_data %>% mutate(t = c(1:nrow(flaxseed_co2_data)))

#lm2 <- (lm(`Area harvested`~ CO2_emissions_kt + I(CO2_emissions_kt^2) + lag(`Area harvested`), flaxseed_co2_data)) 
#using lag, it is autogressive term, used in time series in lag. 
lm2 <- (lm(`Area harvested`~ CO2_emissions_kt +  I(CO2_emissions_kt^2) + t,  flaxseed_co2_data)) 
summary(lm2)
#as c02 emission increase the area harvested decrease.Adjusting the model to show autocorrelation
ggAcf(lm2$residuals) #some autocorrelation
#High correlation




ggAcf(lm2$residuals)
#Show plot of (249, ggplot, there is some correlation between to of them. (252, lm2,show the summary)
# It is important to show they are not autocorrelated by showing ggACF of residuals
#flaxseed_co2_data %>% mutate(y_fit = c(NA,predict(lm2))) %>% ggplot(aes(x = `Area harvested`, y = y_fit)) + geom_point()+ geom_smooth(method="lm")
flaxseed_co2_data %>% mutate(y_fit = predict(lm2)) %>% ggplot(aes(x = `Area harvested`, y = y_fit)) + geom_point()+ geom_smooth(method="lm")
#predicted 400, and true value is 200. 
#lag of harvest data, and co2 emission
#prediction aganist the real value of area harvested. The point needs to be aligned and 45 degrees. 
lm2 <- (lm(`Area harvested`~ lag(CO2_emissions_kt) +  I(lag(CO2_emissions_kt)^2) + t,  flaxseed_co2_data)) 
summary(lm2)
y_pred <- flaxseed_co2_data %>% select(`Area harvested`,CO2_emissions_kt, t) 
y_pred <-rbind(y_pred, c(NA,NA,33))
y_pred2 <- predict(lm2,y_pred)
flaxseed_pred <- data.frame(y = c(flaxseed_co2_data$`Area harvested`, NA),
                            y_fit= c(y_pred2),
                            Year= c(1990:2022))
flaxseed_pred %>% ggplot()+
  geom_line(aes(x = Year, y = y, color = "Obs"))+
  geom_line(aes(x = Year, y = y_fit, color = "pred"))
flaxseed_pred %>% tail(5)
# next year harvest based on the co2 emission is 295.4370 2022
#we are plot obs values in red. The blue line is predict values. We predicting 2022 using 
#lag of co2 emission that is why co2 emission, we used 2021 to predict 2022. We are using the trend 
#which is variable
#------------------------------------------------------------------------------------------
## OATS
oat_dataset_3<- read_csv(here::here("dataset", "oat/crop_p124_t077.csv"),skip = 5)
oat_dataset_3<- oat_dataset_3 %>% drop_na(Year) %>%filter(nchar(Year)==4) %>% drop_na(Year) %>% type_convert()
oat_dataset_3<- oat_dataset_3[-2,3:ncol(oat_dataset_3)] %>% drop_na(Year) %>% head(-1)
colnames(oat_dataset_3) <- colnames(barley_data_3)
View(oat_dataset_3)


oat_dataset_4<- read_csv(here::here("dataset", "oat/crop_p125_t078.csv"),skip = 5)
oat_dataset_4<- oat_dataset_4 %>% drop_na(Year) %>%filter(nchar(Year)==4) %>% drop_na(Year) %>% type_convert()
oat_dataset_4<- oat_dataset_4[-2,3:ncol(oat_dataset_4)] %>% drop_na(Year) %>% head(-1)
colnames(oat_dataset_4) <- colnames(barley_data_4)
View(oat_dataset_4)


oat_data <- oat_dataset_3%>%bind_rows(oat_dataset_4) 
View(oat_data)

oat_co2_data <- co2_data_clean %>% left_join(oat_data, by = c("Time" = "Year"))
View(oat_co2_data)

oat_co2_data <- oat_co2_data %>% filter(CO2_emissions_kt != "..") %>% type_convert()
View(oat_co2_data)


#--------

ggAcf(oat_co2_data$`Area harvested`)  ###use difference 
ggAcf(diff(oat_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(oat_co2_data$CO2_emissions_kt) 
ggAcf(diff(oat_co2_data$CO2_emissions_kt)) 


oat_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(oat_co2_data$`Area harvested`), diff_co2 = diff(oat_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()

oat_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()


lm2 <- (lm(`Area harvested`~ CO2_emissions_kt, oat_co2_data)) 

lm2_diff <-lm(diff_area ~ diff_co2, diff_df) 

ggAcf(lm2$residuals)
ggAcf(lm1_diff$residuals)

oat_co2_data %>% ggplot(aes(y = `Area harvested`, x = (CO2_emissions_kt))) + geom_point()
oat_co2_data %>% ggplot(aes(y = `Production`, x = (CO2_emissions_kt))) + geom_point()
oat_co2_data %>% ggplot(aes(y = `Area planted`, x = (CO2_emissions_kt))) + geom_point()
oat_co2_data %>% ggplot(aes(y = `Yield per acre`, x = (CO2_emissions_kt))) + geom_point()

#------------------------------------------------------------------------------------------
#RYE

rye_dataset_3<- read_csv(here::here("dataset", "rye/crop_p170_t101.csv"),skip = 5)
rye_dataset_3<- rye_dataset_3 %>% drop_na(Year) %>%filter(nchar(Year)==4) %>% drop_na(Year) %>% type_convert()
rye_dataset_3<- rye_dataset_3[-2,3:ncol(oat_dataset_3)] %>% drop_na(Year) %>% head(-1)
colnames(rye_dataset_3) <- colnames(barley_data_3)

View(rye_dataset_3)


rye_dataset_4<- read_csv(here::here("dataset", "rye/crop_p171_t102.csv"),skip = 5)
rye_dataset_4<- rye_dataset_4 %>% drop_na(Year) %>%filter(nchar(Year)==4) %>% drop_na(Year) %>% type_convert()
rye_dataset_4<- rye_dataset_4[-2,3:ncol(rye_dataset_4)] %>% drop_na(Year) %>% head(-1)
colnames(rye_dataset_4) <- colnames(barley_data_4)

View(rye_dataset_4)

rye_data <- rye_dataset_3%>%bind_rows(rye_dataset_4) 
View(rye_data)

rye_co2_data <- co2_data_clean %>% left_join(rye_data, by = c("Time" = "Year"))
View(rye_co2_data)

rye_co2_data <- rye_co2_data %>% filter(CO2_emissions_kt != "..") %>% type_convert()
View(rye_co2_data)
#-------

ggAcf(rye_co2_data$`Area harvested`)  ###use difference 
ggAcf(diff(rye_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(rye_co2_data$CO2_emissions_kt) 
ggAcf(diff(rye_co2_data$CO2_emissions_kt)) 


rye_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(rye_co2_data$`Area harvested`), diff_co2 = diff(rye_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()

rye_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()


lm2 <- (lm(`Area harvested`~ CO2_emissions_kt, rye_co2_data)) 

lm2_diff <-lm(diff_area ~ diff_co2, diff_df) 

ggAcf(lm2$residuals)
ggAcf(lm1_diff$residuals)

rye_co2_data %>% ggplot(aes(y = `Area harvested`, x = (CO2_emissions_kt))) + geom_point()
rye_co2_data %>% ggplot(aes(y = `Production`, x = (CO2_emissions_kt))) + geom_point()
rye_co2_data %>% ggplot(aes(y = `Area planted`, x = (CO2_emissions_kt))) + geom_point()
rye_co2_data %>% ggplot(aes(y = `Yield per acre`, x = (CO2_emissions_kt))) + geom_point()

