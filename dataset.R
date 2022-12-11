library(tidyverse)
library(ggplot2)
library(dplyr)
#install.packages("forecast")
require(forecast)
ggAcf(district_pvi$pvi)
ggPacf(district_pvi$pvi)
library(readr)



## clEAN the data
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



## BARLEY


#crops and co2
co2_data <- read_csv(here::here("dataset", "CSV_fixed_climate_data.csv"))

## clEAN the data
co2_data_clean <- co2_data

write_csv(co2_data_clean, file = here::here("dataset", "clean_co2.csv"))

save(co2_data_clean, file = here::here("dataset/clean_co2.RData"))
co2_data_clean <- co2_data_clean[,-1]
co2_data_clean <- co2_data_clean %>% type_convert()
co2_data_clean[co2_data_clean == ".."] = NA
co2_data_clean[is.na(co2_data_clean)] = 0
co2_data_clean[co2_data_clean == 0] = NA

co2_data_clean <- co2_data_clean[19:50,]

for(i in 3:ncol(co2_data_clean)){
  co2_data_clean[,i][is.na(co2_data_clean[,i])] = mean(co2_data_clean[,i],na.rm=TRUE)
}
cor(co2_data_clean$CO2_emissions_kt,co2_data_clean$Population_growth_annual_percent,na.rm =TRUE) 
co2_data_clean$CO2_emissions_kt[co2_data_clean$CO2_emissions_kt==0,] <- mean(co2_data_clean$CO2_emissions_kt, na.rm = TRUE)
cor(co2_data_clean[3:14],na.rm=T)
View(co2_data_clean)

## BARLEY


barley_data_3 <- read_csv(here::here("dataset", "barley/crop_p015_t005.csv"),skip = 5)
barley_data_3<- barley_data_3[-2,3:ncol(barley_data_3)] %>% drop_na(Year) %>% head(-1)

barley_data_4 <- read_csv(here::here("dataset", "barley/crop_p016_t010.csv"),skip = 5)
barley_data_4<- barley_data_4[-2,3:ncol(barley_data_4)] %>% drop_na(Year) %>% head(-1)

barley_data <- barley_data_3%>%bind_rows(barley_data_4) 


barley_co2_data <- co2_data_clean %>% left_join(barley_data, by = c("Time" = "Year"))

barley_co2_data <- barley_co2_data %>% filter(CO2_emissions_kt != "..") %>% type_convert()
View(barley_co2_data)


write_csv(barely_co2_data, file = here::here("dataset", "barley_co2_data"))

save(barley_co2_data, file = here::here("dataset/barely_co2_data.RData"))


ggAcf(barley_co2_data$`Area harvested`)  ###use difference 

ggAcf(diff(barley_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(barley_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(barley_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot


barley_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(barley_co2_data$`Area harvested`), diff_co2 = diff(barley_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between co2 and harvested

barley_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is very strong correlation here

lm1 <- (lm(`Area harvested`~ CO2_emissions_kt, barley_co2_data)) 

lm1_diff <-lm(diff_area ~ diff_co2, diff_df) 

ggAcf(lm1$residuals)
ggAcf(lm1_diff$residuals)



## CORN

corn_data_3 <-read_csv(here::here("dataset", "corn/crop_p031_t024.csv"),skip = 7)
corn_data_3<- corn_data_3[-2,3:ncol(corn_data_3)] %>% drop_na(Year) %>%filter(nchar(Year)==4)
colnames(corn_data_3) <- colnames(barley_data_3)

View(corn_data_3)

corn_data_4 <- read_csv(here::here("dataset", "corn/crop_p032_t034.csv"),skip = 7)
corn_data_4<- corn_data_4[-2,3:ncol(corn_data_4)] %>% drop_na(Year) %>%filter(nchar(Year)==4)
colnames(corn_data_4) <- colnames(barley_data_4)

View(corn_data_4)


corn_data <- corn_data_3%>%bind_rows(corn_data_4) %>% type_convert()
View(corn_data)
corn_co2_data <- co2_data_clean %>% left_join(corn_data, by = c("Time" = "Year"))
View(corn_co2_data)

write_csv(corn_co2_data, file = here::here("dataset", "corn_co2_data"))

save(corn_co2_data, file = here::here("dataset/corn_co2_data.RData"))


ggAcf(corn_co2_data$`Area harvested`)  ###use difference 

ggAcf(diff(corn_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(corn_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(corn_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot


corn_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(corn_co2_data$`Area harvested`), diff_co2 = diff(corn_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between co2 and harvested

corn_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is very strong correlation here

lm1 <- (lm(`Area harvested`~ CO2_emissions_kt, corn_co2_data)) 

lm1_diff <-lm(diff_area ~ diff_co2, diff_df) 

ggAcf(lm1$residuals)
ggAcf(lm1_diff$residuals)















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


ggAcf(flaxseed_co2_data$`Area harvested`)  ###use difference 

ggAcf(diff(flaxseed_co2_data$`Area harvested`)) ## no autocorrelation


# co2 emission
ggAcf(flaxseed_co2_data$CO2_emissions_kt) ##persistent  autocorrelation 
ggAcf(diff(flaxseed_co2_data$CO2_emissions_kt)) #increase of c02 over time, a good plot


flaxseed_co2_data %>% ggplot(aes(x = diff('Area harvested'), y = diff(CO2_emissions_kt))) + geom_point()

diff_df <- data.frame(diff_area = diff(flaxseed_co2_data$`Area harvested`), diff_co2 = diff(flaxseed_co2_data$CO2_emissions_kt) )
diff_df %>% ggplot(aes(x = diff_area, y = diff_co2)) + geom_point()+ geom_smooth()
### weak correlation between co2 and harvested

flaxseed_co2_data %>% ggplot(aes(x = `Area harvested`, y = CO2_emissions_kt)) + geom_point()+ geom_smooth()
##there is very strong correlation here

lm2 <- (lm(`Area harvested`~ CO2_emissions_kt, flaxseed_co2_data)) 

lm2_diff <-lm(diff_area ~ diff_co2, diff_df) 

ggAcf(lm2$residuals)
ggAcf(lm1_diff$residuals)




## OATS





oat_dataset_3 <- read_csv("dataset/oat/crop_p124_t077.csv", 
                          col_types = cols(`77` = col_skip(), h = col_skip()), 
                          skip = 5)
oat_data_3<- oat_dataset_3 %>% drop_na(Year) %>%filter(nchar(Year)==4) %>% drop_na(Year) %>% type_convert()


View(oat_dataset_3)

colnames(oat_data_3) <- colnames(barley_data_3)
View(oat_dataset_3)

oat_dataset_4 <- read_csv("dataset/oat/crop_p125_t078.csv", 
                          col_types = cols(`78` = col_skip(), t = col_skip(), 
                                           u = col_skip(), ...3 = col_double(), 
                                           `(1,000 acres)...4` = col_double()), 
                          skip = 7)


View(oat_dataset_4)


oat_data <- oat_data_3%>%bind_rows(oat_data_4)







## RYE

rye_data_3 <-read_csv(here::here("dataset", "rye/crop_p170_t101.csv"),skip = 5)
View(rye_dataset_3)
rye_data_3<- rye_data_3[-2,3:ncol(rye_data_3)] %>% drop_na(Year) %>% type_convert()


View(rye_dataset_3)


View(rye_dataset_4)

rye_dataset <- rye_dataset_3%>%bind_rows(rye_dataset_4)





