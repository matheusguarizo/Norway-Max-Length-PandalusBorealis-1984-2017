#Max Lengths predicts by GAMs for Pandalus borealis (DeepNorway and Skagerakk), 1984-2017
#Based on methodology by:
# Lavin, C. P., Gordó-Vilaseca, C., Stephenson, F., Shi, Z., & Costello, M. J. (2022).
# Warmer temperature decreases the maximum length of six species of marine fishes, crustacean, and squid in New Zealand.
# Environmental Biology of Fishes, 105(10), 1431–1446. https://doi.org/10.1007/s10641-022-01251-7


#info: https://www.sealifebase.org/summary/Pandalus-borealis.html
#range 2 - 2.443 cm Max length : 12.0 cm TL male/unsexed, 16.5 cm TL (female)
#Benthopelagic; depth range : usually 50 - 500 m

library(ncdf4)
library(reshape2)
library(dplyr)
library(mgcv)
library(raster)
library(ggplot2)
library(fitdistrplus)
library(gratia)

setwd("C:/Users/mathe/Documents/NovoBackup/Doutorado/Phdproject/S.data")

#Import lenght data
length_data <- read_delim("data.nor/measurementorfact.nor.txt", 
                     +     delim = "\t", escape_double = FALSE, 
                     +     col_types = cols(measurementID = col_skip(), 
                                            +         occurrenceID = col_skip(), measurementType = col_skip(), 
                                            +         measurementTypeID = col_skip(), measurementValue = col_number(), 
                                            +         measurementValueID = col_skip(), 
                                            +         measurementAccuracy = col_skip(), 
                                            +         measurementUnit = col_skip(), measurementUnitID = col_skip(), 
                                            +         measurementDeterminedDate = col_skip(), 
                                            +         measurementDeterminedBy = col_skip(), 
                                            +         measurementMethod = col_skip(), measurementRemarks = col_skip()), 
                     +     trim_ws = TRUE)

#574137 obs
#Filter data
length_data <- length_data %>%
  filter(measurementValue != 0.00000)
length_data <- length_data %>%
  filter(measurementValue <= 0.165) #Max documented length

#Max values
maxlength <- length_data %>%
  group_by(id) %>%
  summarise(max_value = max(measurementValue, na.rm = TRUE))
boxplot(maxlength$max_value) #2588 obs. 

maxlength<- maxlength %>%
  arrange(id)



#drawing Temp data from NetCDF-4 file
#Source: Copernicus Marine Service https://doi.org/10.17882/46219 (1984-1992), 
#Source: Copernicus Marine Service https://doi.org/10.48670/moi-00021 (1993-1984)
#Lat and Long data already attributed: Box 57.4° N, 10.9° E, 59.92° N, 0.2° W 

## For the year 1984 (temp84)
temp84 <- raster("C:/Users/mathe/Documents/NovoBackup/Doutorado/Phdproject/S.data/temp.year/84.nc")
temp_df84 <- as.data.frame(temp84, xy = TRUE)
mean_temp84 <- mean(temp_df84$Temperature, na.rm = TRUE)
print(mean_temp84)
rm(temp84)
rm(temp_df84)
#Mean Temperature 7.334604
#Looped from 1984 - 2017


View(cleaned_data_tempyear.csv) 
#id, max_value, temp, temp.seq
#temp.seq => oceantemp <- seq(from = 6.74303, to = 8.943899, length.out = 2588)

summary(temp_final$temperature)
hist(temp_final$temperature)
boxplot(temp_final$temperature)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6.813   7.198   7.784   7.966   8.524   9.947 

#resulting data treated elsewhere as .csv (cleaned_data_tempyear.csv)

#Pearson
pearson <- cor(cleaned_data_tempyear$max_value, cleaned_data_tempyear$temp, 
               method = "pearson")
print(pearson)
#0.01162408

#Distribution
distr <- fitdist(cleaned_data_tempyear$max_value, "gamma")
print(distr)
#Parameters:
#estimate Std. Error
#shape  103.1795   2.892135
#rate  3945.1580 110.856984

#gam
cleaned_data_tempyear_gam <- mgcv::gam(max_value ~ s(temp.seq, k = 4, bs = "ts"), 
                                      family = Gamma (link = "log"),  
                                      method = "REML",  
                                      data = cleaned_data_tempyear)
print(cleaned_data_tempyear_gam)
summary(cleaned_data_tempyear_gam)
summary(cleaned_data_tempyear_gam)$r.sq.adj
plot(cleaned_data_tempyear_gam
     )

#edf         Ref.df     F p-value    
# 2.915      3 29.72    <2e-16 ***
#R^2
#R-sq.(adj) =  0.0328

#Akaike
aic_value <- AIC(cleaned_data_tempyear_gam)
print(aic_value)
#-23612.8

#Appraise
ap <- gratia::appraise(cleaned_data_tempyear_gam)
print(ap)


#max length
fitted_values <- predict(cleaned_data_tempyear_gam, 
                         type = 'response', se.fit = TRUE)
predictor_values <- cleaned_data_tempyear$temp.seq

predictions <- data.frame(
  mean_ocean_temperature = predictor_values,
  fit = fitted_values
)
print(predictions)
print(max(predictions$fit))
#[1] 0.02681542


#99% of confidence
predictions$lower_ci <- predictions$fit - 2.576 * predictions$se.fit
predictions$upper_ci <- predictions$fit + 2.576 * predictions$se.fit


#plot
ggplot() + geom_line(aes(x = predictions$mean_ocean_temperature, 
                         y = predictions$fit)) + 
  geom_ribbon(aes(x = predictions$mean_ocean_temperature, ymin = predictions$lower_ci, 
                  ymax = predictions$upper_ci), fill="#8ACE00", alpha = 0.5) + 
  theme_classic() + theme(axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 8)) +
  labs(caption = "(Skagerrak and Deep Norway, 1984-2017)",
       x= "Temperature (ºC)",
       y= "Length (TL, cm)") + 
  scale_y_continuous(labels = function(x) x * 100) + 
  geom_segment(aes(x = 6.61, y = 0.0250, xend = 8.24, yend = 0.0250), 
               color = "#8ACE00", size =1, linetype = "dashed")


ggplot() + geom_line(aes(x = predictions$mean_ocean_temperature, 
                         y = predictions$fit)) + 
  geom_ribbon(aes(x = predictions$mean_ocean_temperature, ymin = predictions$lower_ci, 
                  ymax = predictions$upper_ci), fill="#8ACE00", alpha = 0.5) + 
  theme_classic() + theme(axis.text.x = element_text(size = 8),
                          axis.text.y = element_text(size = 8)) +
  labs(caption = "(Skagerrak and Deep Norway, 1984-2017)",
       x= "Temperature (ºC)",
       y= "Length (TL, cm)") + 
  scale_y_continuous(labels = function(x) x * 100) + 
  geom_segment(aes(x = 6.61, y = 0.0250, xend = 8.24, yend = 0.0250), 
               color = "#8ACE00", size =1, linetype = "dashed") +
  geom_segment(aes(x = -0.9, y = 0.0248, xend = 5.6, yend = 0.0248),
               color = "royalblue", size =1, linetype = "dashed") +
  geom_point(aes(x=1.9, y=0.0248), shape=19, colour="royalblue", size = 4)
  
#Preferred temperature (Ref. 115969): -0.9 - 5.6, mean 1.9 (based on 2142 cells). 
#Boreal; -2°C - 12°C (Ref. 85480), preferred 4°C 
#Source: https://www.sealifebase.org/summary/Pandalus-borealis.html
