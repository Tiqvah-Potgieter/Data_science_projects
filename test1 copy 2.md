---
title: "Anthesis assessment"
format: 
  html: 
    embed-resources: true 
editor: visual
---

## 

# Skills Assessment

**Tiqvah Potgieter**

All my work is conducted in rStudio, using R. Please note that the graphs are zoomable.The general layout of this assessment starts with each task and the questions. Below each question is my answer but all my workings for that answer is displayed in R (which follows the task section). Please also read the commented code which helps explain my thinking process.

## Task 1: Data Interpretation and Analysis

[**Questions:**]{.underline}

**1.Which province-municipality-district combination uses the most Diesel Consumption and in which year respectively?**

Answer 1: Free State-Ngwathe-Fezile Dabi has the most Diesel consumption, specifically year 2020 with a total of 76107.5 for that year

**2.What is the average Diesel Consumption for KwaZulu-Natal?**

Answer 2: The average diesel consumption for KwaZulu-Natal is 120.6709

**3. Identify which farms in KwaZulu-Natal have used more than the average Diesel Consumption for the same province.**

Answer 3:

1 Durleigh, 2 Broadview, 3 Kenlei Farms, 4 Stone Ridge, 5 Colbourne Farm, 6 Glenisla, 7 Spring Valley, 8 Baynesfield, 9 Der, 10 Glen Read, 11 Sugarbush, 12 Avogrow, 13 Sarsgrove.

**4a. Who emitted the lowest carbon dioxide emissions rate overall and in what year?**

Answer 4a: FarmID=570 Farm=Rietfontein Year=2022 co2_emission_rate=0.09921795

**4b. When comparing the avg 2017-2019 carbon dioxide emissions rate with that of the avg 2020-2021 rate per farm, which farm increased their avg rate by the largest delta?**

Answer 4b: FarmID=552 with a delta of 21909.577

```{r}
#| warning: FALSE
#| message: FALSE
#| code-overflow: wrap 

############################### TASK1 #########################################################

##############Question: 1 

#Loading libraries that I will need for this assessment 
library(dplyr)
library(readr)
library(tidyr)

#Reading in all csv files 
farm_details <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/FarmDetails.csv")
farms <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/Farms.csv")

#Merging farm_details and farms by their FarmID,including all rows from both datasets and filling NAs for missing values (if any)
farms_merged <- full_join(farm_details,farms, by = "FarmID")

#Checked to see if there are any NAs,results showed that there are NAs. This is important to note especially when you are going to do calculations 
any(is.na(farms_merged))

#Creating a new colum called DieselConsumption which will be calculated as QuantityDiesel/FarmSize ,assuming QuantityDiesel is in tonnes and FarmSize in Ha
farms_merged <- farms_merged %>% 
  mutate(DieselConsumption = QuantityDiesel/FarmSize) 

#Showing the first 10 rows of farms_merged
head(farms_merged,10)

#Summarizing total diesel consumption by province-municipality-district combination and year 
farms_summary <- farms_merged %>% 
  group_by(Province,Municipality,District,Year) %>% 
  summarise(SummedDieselConsumption = sum(DieselConsumption, na.rm = TRUE)) %>% 
  arrange(,desc(SummedDieselConsumption))
  
head(farms_summary,1)

#Answer 1: Free State-Ngwathe-Fezile Dabi has the most Diesel consumption,specifically year 2020 with a total of 76107.5 for that year 

##############Question: 2

# Only selecting entries where the province = KwaZulu-Natal from farms_merged 
kwaZuluNatal <- farms_merged %>% 
  filter(Province == "KwaZulu-Natal") 

#Obtaining the mean Diesel consumption for KwaZulu-Natal, ignoring all NAs
Ave_KwazuluNatal <- mean(kwaZuluNatal$DieselConsumption, na.rm = TRUE) 

#Answer 2: The average diesel consumption for KwaZulu-Natal is 120.6709 
print(Ave_KwazuluNatal)



##############Question: 3 

above_ave <-  list(kwaZuluNatal %>% 
  filter(DieselConsumption > Ave_KwazuluNatal) %>% 
  select(FarmName) %>% 
  distinct() ) 

#Answer 3: The farms in KwaZulu-Natal that has used more than the average Diesel Conumption of KwaZulu-Natal are:       

print(above_ave) 


##############Question: 4a  

#Asuming to only calculate the carbon emissions rate from diesel only 

#Diesel emission factor 
diesel_ef <- 0.002886 

#Calculating diesel comsumption in liters (assuming Diesel quantity is given in tonnes),so to get to liters I am only going to times Diesel by 1176.4706, assuming diesel has a density: 850 kg/m3.
farms_merged2 <- farms_merged %>% 
  mutate(Diesel_liters = QuantityDiesel*1176.47) 

#Showing the first five rows of farms_merged
head(farms_merged2,5)

#Calculating the co2 emissions from diesel (EFF)
farms_merged2 <- farms_merged2 %>% 
  mutate(EFF = Diesel_liters*diesel_ef)

#Calculating the co2 emission rate which is just EFF/farmsize
farms_merged2 <- farms_merged2 %>% 
  mutate(co2_emission_rate = EFF/FarmSize)

#Getting the farm with the lowest co2 emissions rate 
farm_lowest <- farms_merged2 %>% 
  filter(co2_emission_rate == min(co2_emission_rate,na.rm = TRUE)) %>% 
  select(FarmID, FarmName, Year, co2_emission_rate)

#Answer 4a: FarmID=570	Farm=Rietfontein	Year=2022	co2_emission_rate=0.09921795
print(farm_lowest) 

##############Question: 4b  

#Here I am filtering for years between 2017-2021 and then grouping by FarmID, then creating 2 new groups,all entries from years 2017-2019 as one group and then grouping all entries from 2020-2021 as the second group,I am then calculating the average co2_emission_rate per FarmID per period group.
  periods <- farms_merged2 %>%
  filter(Year %in% 2017:2021) %>%
  group_by(FarmID, period = ifelse(Year <= 2019, "2017-2019", "2020-2021")) %>%
  summarize(avg_CO2_rate = mean(co2_emission_rate, na.rm = TRUE)) %>% 
  pivot_wider(names_from = period, values_from = avg_CO2_rate) 

#Showing the first 10 rows from periods 
head(periods,10) 

#Calculating the difference between the averages of co2_emission_rates between the two periods
periods <- periods %>%
  mutate(delta = `2020-2021` - `2017-2019`) 

#Showing the first 10 rows from periods 
head(periods,10) 

#Arranging periods from largest to smallest 
max_delta_farm <- periods %>%
     arrange(,desc(delta))

#Showing the first 10 rows of max_delta_farm
#Answer 4b: FarmID=552 with a delta of 21909.577
head(max_delta_farm,10)


```

## **Task 2: Business Intelligence and Statistical Analysis**

[**Questions:**]{.underline}

**1.Create a meaningful visual representation/s from the two datasets provided of crop Insights and trends. You may focus on any aspects you wish. Show us your skills!**

Answer 1: See code

**2.Using the Crop details Yield dataset calculate the mean, median and standard deviation of each of crop type.**

Answer 2:

|                   |           |        |                    |
|-------------------|-----------|--------|--------------------|
| CropType          | Mean      | Median | Standard Deviation |
| Barley            | 2.841563  | 3.00   | 1.1350874          |
| Cowpea            | 0.000000  | 0.00   | 0.0000000          |
| Dry bean          | 2.353443  | 2.50   | 0.2412837          |
| Groundnut         | 1.495000  | 1.70   | 0.7950681          |
| Kikuyu            | 3.070741  | 0.00   | 5.8948164          |
| Lucerne (Alfalfa) | 16.909498 | 0.00   | 35.6801068         |
| Maize             | 8.963470  | 7.80   | 7.6242254          |
| Millet            | 10.082353 | 10.00  | 2.6740034          |
| Oats              | 1.604579  | 0.00   | 8.3857792          |
| Pea               | 0.800000  | 1.00   | 0.4472136          |
| Potato            | 33.204225 | 35.00  | 2.4072548          |
| Rye-grass         | 10.313676 | 12.00  | 8.0654036          |
| Sorghum           | 16.556647 | 10.00  | 14.6983838         |
| Soybeans          | 3.029310  | 2.50   | 1.5259881          |
| Sugarcane         | 80.970356 | 85.00  | 12.0043945         |
| Sunflower         | 1.839595  | 1.92   | 0.6419304          |
| Vetch (Fodder)    | 16.117647 | 18.00  | 6.1020729          |
| Wheat             | 2.934564  | 2.74   | 1.6788732          |

**3.Using the Crop details dataset perform a hypothesis test to determine if there is a statistical difference ( p \< 0.05) between the PPN-Fixing yields/ha in 2018 and 2021**

Answer 3: There is a significant difference between the means of the two groups

```{r}
#| warning: FALSE
#| message: FALSE
#| fig-width: 28 
#| fig-height: 15
#| code-overflow: wrap 

############################### TASK2 #########################################################

##############Question: 1

#Loading relevant libraries 
library(ggplot2)
library(pheatmap)
library(stringr)
library(scales)

#Reading in all csv files 
crop_details <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/CropDetails.csv") 
fields <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/Fields.csv") 

#Merging crop_details and fields by their FieldID,including all rows from both datasets and filling NAs for missing values (if any)
fields_merged <- full_join(crop_details,fields ,by = "FieldID") 

#Calculating the yield per ha 
fields_merged <- fields_merged %>% 
  mutate(yields_per_ha = Yield/FieldSize)

#Note that many fieldSizes have a value of 0, and when dividing by 0 you would get undefined,so now I am going to replace these values with NA (it would get ignored during calculations) 
fields_merged$yields_per_ha[fields_merged$yields_per_ha == "Inf"] <- NA 

#viewing the total yield for every crop type per year 
#Note that when scaling the y-axis to log10, on the y-axis the values are log10 transformed, but the values are still the same, so the values are not changed, only the scale is changed

#Calculatind the total yield for every croptype per year and removing any NAs
data1 <- fields_merged %>% 
  group_by(CropType,Year) %>% 
  summarise(TotalYield_perYear = sum(Yield, na.rm = TRUE)) %>% 
  mutate_at(c("CropType","Year"),as.factor) %>% 
  mutate(Year = as.character(Year)) %>% 
  na.omit() 

ggplot(data1, aes(x=Year, y= TotalYield_perYear, group=CropType)) +
  geom_line(aes(color=CropType)) +  
  geom_point(aes(color=CropType)) +
  facet_wrap(~CropType,scales = "free") +
  scale_y_continuous(limits = c(0.0,NA)) +
theme(plot.title = element_text(size = 35 ,hjust = 0.5), plot.subtitle = element_text(size=30,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 25),legend.text = element_text(size = 20),axis.text.x = element_text(size = 20,angle = 45),axis.text.y = element_text(size = 20),strip.text = element_text(size=30),legend.key.spacing.y = unit(20, "pt")) +
  labs(title = "Yield trends over the years:",subtitle = "Total yield for every crop type per year", x = "Year", y = "Yield", color = "Crop Type") 

#Viewing every croptype on the same graph and log transforming the Yield to make it more comparable
ggplot(data1, aes(x=Year, y= (TotalYield_perYear), group=CropType)) + 
  geom_line(aes(color=CropType),linewidth =1) + 
  geom_point(aes(color=CropType),size = 3) +
 scale_y_log10() +
  theme(plot.title = element_text(size = 35 ,hjust = 0.5), plot.subtitle = element_text(size=30,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 25),legend.text = element_text(size = 20),axis.text.x = element_text(size = 20,angle = 45),axis.text.y = element_text(size = 20),legend.key.spacing.y = unit(20, "pt")) +
  labs(title = "Yield trends over the years",subtitle ="Total yield for every crop type per year",x = "Year", y = "Log 10 Transformed Yield", color = "Crop Type") 
  

#viewing the average yield for every crop type 
###Calculating the average yield for every crop type and rounding to 2 decimal places 
data2 <- fields_merged %>% 
  group_by(CropType) %>% 
  summarise(AveYield = round(mean(Yield,na.rm = TRUE),2)) %>% 
  mutate_at("CropType",as.factor) %>% 
  na.omit() 
  

ggplot(data2, aes(x=CropType, y= AveYield,fill = CropType)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = AveYield),size = 7) +
  theme(plot.title = element_text(size = 37 ,hjust = 0.5), plot.subtitle = element_text(size=30,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 30),legend.text = element_text(size = 20),axis.text.x = element_text(size = 20,angle = 25),axis.text.y = element_text(size = 25),legend.key.spacing.y = unit(20, "pt"))  +
  labs(title = "Yield trends:",subtitle = "Average yield for every crop type", x = "CropType", y = "Average Yield", color = "Crop Type") 


####viewing the average yield_perHa for every crop type per year
#Calculating the average yield per ha for every crop type per year and rounding to 2 decimal places 
data3 <- fields_merged %>% 
  group_by(CropType,Year) %>% 
  summarise(AveYield_perHa = round(mean(yields_per_ha,na.rm = TRUE),2)) %>% 
  mutate_at("CropType",as.factor) %>% 
  mutate(Year = as.character(Year)) %>% 
 na.omit() 
 
ggplot(data3, aes(x=Year, y= AveYield_perHa, group=CropType)) +
  geom_line(aes(color=CropType)) +  
  geom_point(aes(color=CropType)) +
  facet_wrap(~CropType,scales = "free") +
  scale_y_continuous(limits = c(0.0,NA)) +
theme(plot.title = element_text(size = 35 ,hjust = 0.5), plot.subtitle = element_text(size=30,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 25),legend.text = element_text(size = 20),axis.text.x = element_text(size = 20,angle = 45),axis.text.y = element_text(size = 20),strip.text = element_text(size=30),legend.key.spacing.y = unit(20, "pt")) +
  labs(title = "Yield trends over the years:",subtitle = "Average yield per hectare for every crop type per year", x = "Year", y = "Yield", color = "Crop Type") 

#Viewing every croptype on the same graph and log transforming the average Yield/ha to make it more comparable
ggplot(data3, aes(x=Year, y= AveYield_perHa, group=CropType)) + 
  geom_line(aes(color=CropType),linewidth =1) + 
  geom_point(aes(color=CropType),size = 3) +
 scale_y_log10(labels = comma) +
  theme(plot.title = element_text(size = 35 ,hjust = 0.5), plot.subtitle = element_text(size=30,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 25),legend.text = element_text(size = 20),axis.text.x = element_text(size = 20,angle = 45),axis.text.y = element_text(size = 20),strip.text = element_text(size=30),legend.key.spacing.y = unit(20, "pt")) +
  labs(title = "Yield trends over the years",subtitle ="Average yield per hectare for every crop type per year",x = "Year", y = "Log 10 Transformed average Yield per hectare", color = "Crop Type") 


data3.2 <- fields_merged %>% 
  group_by(CropType) %>% 
  na.omit() 

#Note the triangle represents the mean of the yield per hectare for every crop type 

ggplot(data3.2, aes(x=CropType, y= yields_per_ha, group_by=CropType)) + 
 geom_boxplot(aes(color = CropType)) +
  theme(plot.title = element_text(size = 40 ,hjust = 0.5), plot.subtitle = element_text(size=35,hjust = 0.5), axis.title.x= element_text(size = 30),axis.title.y =element_text(size = 30),legend.title = element_text(size = 30),legend.text = element_text(size = 25),axis.text.x = element_text(size = 20,angle = 15),axis.text.y = element_text(size = 20),legend.key.spacing.y = unit(20, "pt")) +
 scale_y_log10(labels = comma) +
stat_summary(fun = "mean", geom = "point",shape = 2, size = 3) +
 labs(title = "Yield per hectare trends:",subtitle ="Summary of yield per hectare for every crop",x = "CropType", y = "Yield per hectare", color = "Crop Type") 


##############Question: 2 

crop_summarized <- crop_details %>% 
  group_by(CropType) %>% 
  summarise(mean = mean(Yield), median = median(Yield), standard_deviation = sd(Yield)) 

#Answer 2: 
crop_summarized

##############Question: 3 
#3.Using the Crop details dataset perform a hypothesis test to determine if there is a statistical difference ( p < 0.05) between the PPN-Fixing yields/ha in 2018 and 2021

#By only using the crop details dataset I am assuming that yields are yields/ha, as there is no details about farmsize or fieldsize in the crop details dataset. I am also assuming that all yields are PPN-Fixing yields because there is no information regarding which FieldID or CropType is a PPn-Fixing yield

#Getting all yields from year 2018
yield_2018 <- crop_details %>% 
  filter(Year == 2018) %>% 
  pull(Yield)  

#Getting all yields from year 2021
yield_2021 <- crop_details %>% 
  filter(Year == 2021) %>% 
  pull(Yield) 

# If the p-value is less than 0.05, you would reject the null hypothesis and conclude that there is a significant difference between the means of the two groups.
test <- t.test(yield_2018,yield_2021)

#Answer 3: There is a significant difference between the means of the two groups
print(test) 


```

## **Task 3: Basic Data Science Skills (Code, Problem Solving, ...)**

[**Questions**]{.underline}

**1.Calculate the Euclidean distance between each point in Dataset_1 and the nearest point in Dataset_2.**

Answer 1: See code

**2.If the distance between a point in Dataset_1 and its nearest point in Dataset_2 is less than a user-defined threshold (which you can change), move the point from Dataset_1 to Dataset_2.**

Answer 2: See code

**3.Repeat this process until no more points in Dataset_1 can be moved within the threshold distance.**

Answer 3: See code

```{r}
#| warning: FALSE
#| message: FALSE 
#| fig-width: 28 
#| fig-height: 15
#| code-overflow: wrap 
############################### TASK3 #########################################################

#Loading relevant libraries 
library(tidyverse)
library(magrittr)

cart1 <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/Cartesian_Dataset_1.csv") 
cart2 <- read.csv(file = "/Users/tiqvah/Desktop/Skill Assessment/Cartesian_Dataset_2.csv") 

#Writing a function to calculate the euclidean distance between a point in dataset1(cart1) and dataset2(cart2)
Euclidean <- function(a,b){ 
  return(sqrt((a$X-b$X)^2 + (a$Y -b$Y)^2))
  }

#Creating 2 lists to store the distances 
list1 <- list()
list2 <- list()  

#Calculating the distance between each point in cart1 and every other point in cart2
for (a in 1:nrow(cart1)) { 
  
  for (b in 1:nrow(cart2)) { 
    #list1 contains all the euclidean distances for every point in cart1 to a specific point in cart2
    list1[[b]] <- Euclidean(cart1[b,],cart2[a,])
    
  } 
  
  #taking all the values from list1 and creating a column (the colum contains 50 rows which represents all the points in cart1 in relation to a second point in cart2)
  list2[[a]] <- as.data.frame(do.call(rbind,list1)) 
  }  

#creating a dataframe 50x50,
cart_distances <- as.data.frame(do.call(cbind,list2)) 

#Each row represents a point in cart1, and each column represents a point in cart2 
colnames(cart_distances) <- cart2$Object_ID 

#showing the first 20 rows of cart_distances, row1(objectID 1 from cart1) and colum1 (objectID 51 from cart2) have a euclidean distance of 1249.5
head(cart_distances,5)

#showing a quick statistical summary of each column of cart_distances (only the first 5 columns)
summary(cart_distances[,c(1:5)]) 

#created a function to normalize the data
data_norm <- function(x) {
  ((x-min(x))/ (max(x)-min(x)))
}

#normalized the data and saved as cart_distances_normalized
cart_distances_normalized <- cart_distances
cart_distances_normalized <- as.data.frame(lapply(cart_distances_normalized,data_norm)) 

#showing a quick statistical summary of the normalized data (only the first 5 columns)
summary(cart_distances_normalized[,c(1:5)])

#Since there is no mention of normalizing the data in the task description, i am just going to use the original values

##############Question: 2
  
#Setting a threshold    
#Creating a function to get the top 5 nearest points from cart1 for a specific point in dataset2 (cart2)   
    top_5_nearest <- function(x){
            sort(x)[1:5]
   }
  
#Setting my threshold as mean of the 5 nearest points,but can change to anything if needed
top_5_values <- apply(cart_distances,2,top_5_nearest)
my_threshold <- mean(top_5_values)
my_threshold 

#Moving the lowest points from dataset1 (cart1) to dataset2 (cart2) if the distance is below 523.3511
#Writing a function to get the index of points that are lower than the threshold 
index <- function(x){ 
  which(x < my_threshold) 
  }

#Getting the indices of points that are lower than the threshold 
points_to_move <- apply(cart_distances,2,index) 

#Point to move shows the indices of the points of cart1 (dataset1) that are the closest (below the threshold) to each each point in cart2 (dataset2),example objectIDs 6,7,10,33,49,50 from cart1 are the closesest to ObjectID 51

head(points_to_move)

#Removing duplicate indices
points_to_move <- sort(unique(unlist(points_to_move))) 

#Only adding the points that were below the threshold from cart1 to cart2,note the modified cart2 is called cart2.1
for (x in points_to_move) { 
  
  if(x == 1) {
  cart2.1 <- rbind(cart2,cart1[x,]) 
  
  } else {
  cart2.1 <- rbind(cart2.1,cart1[x,])
  
  }
} 

print(cart2.1) 

#Removing the indices that were moved from cart1 and calling it cart1.1
cart1.1 <- cart1 
cart1.1 <- cart1.1[-c(points_to_move),]

print(cart1.1)
 
#Saving original points from cart1 and cart2 in a scatter plot
cart_combined <- rbind(cart1,cart2) 

cart_combined <- cart_combined %>% 
  mutate(cart = ifelse(Object_ID < 51,1,2)) 
cart_combined$cart <- as.factor(cart_combined$cart)

ggplot(cart_combined, aes(x = X, y = Y, color = cart)) +
  geom_point(size = 5) +
  geom_text(aes(label = Object_ID), vjust = -0.5, size = 6 , color = "black") +
  labs(title = "Scatter Plot of the dataset1 and dataset2",
       x = "X Distance",
       y = "Y Distance",
       color = "Dataset") +
  theme_minimal() + 
   theme(plot.title = element_text(size = 30 ,hjust = 0.5), plot.subtitle = element_text(size=45,hjust = 0.5), axis.title.x= element_text(size = 35),axis.title.y =element_text(size = 35),legend.title = element_text(size = 45),legend.text = element_text(size = 40),legend.key.size = unit(20, "pt"),axis.text.x = element_text(size = 38),axis.text.y = element_text(size = 38)) +
  guides(color = guide_legend(override.aes = list(size = 10))) 
  

#Showing the points after they have been moved from cart1 to cart2
cart_combined2 <- rbind(cart1.1,cart2.1) 

cart_combined2 <- cart_combined2 %>% 
  mutate(cart = ifelse(Object_ID == c(3,20,25,32,35),1,2)) 

cart_combined2$cart <- as.factor(cart_combined2$cart)

ggplot(cart_combined2, aes(x = X, y = Y, group = cart)) +
  geom_point(aes(color = cart),size = 5) +
  geom_text(aes(label = Object_ID), vjust = -0.5, size = 6 , color = "black") +
  labs(title = "Scatter Plot of the dataset1 and dataset2 after point have been moved",
       x = "X Distance",
       y = "Y Distance",
       color = "Dataset") +
 theme_minimal()  +
   theme(plot.title = element_text(size = 30 ,hjust = 0.5), plot.subtitle = element_text(size=45,hjust = 0.5), axis.title.x= element_text(size = 35),axis.title.y =element_text(size = 35),legend.title = element_text(size = 45),legend.text = element_text(size = 40),legend.key.size = unit(20, "pt"),axis.text.x = element_text(size = 38),axis.text.y = element_text(size = 38)) +
  guides(color = guide_legend(override.aes = list(size = 10))) 
  
  
```
