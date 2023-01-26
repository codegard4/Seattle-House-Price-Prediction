Task 1
library(tidyverse)
library(dplyr)
house<- read.csv(file.choose())
write.csv(house, "house")
summary(house)
house1 <- house %>% select(-lat)
house2<- house1 %>% select(-long)
house3<- house2 %>% select(-sqft_lot)
house4<- house3 %>% select(-yr_renovated)
house5<- house4 %>% select(-condition)

#Built in the last 10 yrs
#Lot in the top 10% of sizes
#bedrooms^2
#bathrooms^2
#sqrt living sqft

house6 <- house5 %>% mutate(sqft_liv_chg = sqft_living - sqft_living15)
house6 <- house6[!house6$sqft_liv_chg == "NULL",]
house6 <- house6 %>% select(-sqft_living15)
house7 <- house6 %>% mutate(bathsqr = bathrooms^2, bedsqr = bedrooms^2, rootLiving = sqrt(sqft_living), newBuild = ifelse(2015-yr_built <= 10, "NEW", ifelse(2015-yr_built <= 30, "AGING", "OLD")))
house7$zipcode = as.factor(house7$zipcode)
write.csv(house7, "house_cleaned")
modelNew <- lm(price ~.,house7)
summary(modelNew)
houseNew <- house7 %>% filter(newBuild == "NEW")
modelNew1 <- lm(price~sqft_living,houseNew)
summary(modelNew1)
houseAging <- house7 %>% filter(newBuild == "AGING")
modelAging1 <- lm(price~sqft_living,houseAging)
summary(modelAging1)
houseOld <- house7 %>% filter(newBuild == "OLD")
modelOld1 <- lm(price~sqft_living,houseOld)
summary(modelOld1)

ggplot(data = house7, aes(x = sqft_living, y = price, color = newBuild)) + geom_point() + geom_smooth(method = lm) + 
  labs(y = "Home Price ($)", x = "House Square Footage", title = "Figure 1.1: Square Footage vs House Price by Home Age")

Figure 1.1 shows the correlation between square footage and the price of a home divided into new, middle age and old homes. For the purposes of the model old homes were defined as built more than 30 years ago, new homes were built within the last 10 years and 10-30 year old homes were "aging"
#list of what you cleaned
The housing data set used to create our model starts with 20 variables. Latitude and Longitude will not be useful for this dataset because all of the houses are located in a relatively small area in King County, WA. 
Without Latitude and Longitude we eliminated variables one by one based on their respective p-values, starting with zipcode (p-value: .94), square footage of the lot (p-value: .75), year the house was renovated (p-value: .29), condition of the house (p-value: .14), and number of floors (p-value: .12). 
After narrowing down our dataset to 15 variables we added 5 new variables based on factors that we already know. These factors are number of bedrooms and bathrooms squared, square root of living area, sqare footage change (if the house underwent renovations), and a categorical variable to describe the age of the home
These variables increased our R-squared value by .04, but more importantly add more significance based on the p-value of their respective slopes, than existing variables we have.



library(tidyverse)
library(dplyr)
house<- read.csv("house")
house7<- read.csv("house_cleaned")
ggplot(data = house7, aes(x = bedrooms, y = price)) + geom_jitter(color = "blue") + geom_smooth() +
  labs(y = "Home Price ($)", x = "Bedrooms", title = "Figure 1.2: Bedrooms vs House Price")

house8<- house7 %>% mutate(highbed= ifelse(bedrooms >= 5, "5+ Beds", "<5 Beds"))

ggplot(data = house8, aes(x = bedrooms, y = price, color = highbed)) + geom_jitter() + geom_smooth(method =lm) +
  labs(y = "Home Price ($)", x = "Bedrooms", title = "Figure 1.2: Bedrooms vs House Price", color = "Number of ")




Carefully select your variables using appropriate methods
Give full regression diagnostics for your final model.
Include the correlation coefficient for your final model and describe what it means in context.
Include the training RMSE for your final model.
Include an estimate of the RMSE for new data. (i.e. testing RMSE)
Include plots and discussion demonstrating that linear regression was appropriate

Task 3
library(tidyverse)
library(readr)
library(GGally)
library(olsrr)
house7<- read.csv("house_cleaned")

modelAll <- lm(price~.,house7)
backSelect <- ols_step_forward_p(modelAll)

house8 <- house7 %>% 
  select(-sqft_above,-sqft_basement,-bedsqr,-bedrooms,-yr_built,-sqft_lot15)
write.csv(house8, "HOUSING_CLEANED")
house8$zipcode = as.factor(house8$zipcode)
#summary(house8)
modelSorted <- lm(price~.,house8)



library(car)

trainSelect <- ols_step_forward_p(modelSorted)

finalSix <- house8 %>%
  mutate(bathsqR = ifelse(bathsqr > 0, bathsqr, .0001)) %>% 
  select(waterfront, view, grade, zipcode, bathsqR,price, sqft_living,price) %>%
  mutate(gradeS = grade^2, gradeL = log10(grade), 
         baths = sqrt(bathsqR), bathL = log10(baths),
         sqft_livingS = sqft_living^2, sqft_livingL = log10(sqft_living))
#Split into training and testing now that we have all of our variables created
set.seed(100)
df<- finalSix
training_rows <- sample(1:nrow(df), floor(0.75*nrow(df)))
Training <- df[training_rows, ]
Testing <- df[-training_rows, ]
modelTrain <- lm(price~.,Training)
modelTest <- lm(price~.,Testing)


modelFinal <- lm(price~., finalSix)

forwardSelectFinal <- ols_step_forward_p(modelFinal)
#forwardSelectFinal
#vif(modelFinal)
finalSix1 <- finalSix %>% select(gradeS,zipcode,sqft_livingS, waterfront, view, bathsqR,price)
modelFinal1 <- lm(price~., finalSix1)
#summary(modelFinal1)
#vif(modelFinal1)
#waterfront, sqft
#waterfront, view, bath, grade, year built
finalHousingSet <- finalSix1
plotDataset <- finalHousingSet %>% filter(price > 0)
modelFinalUpdate <- lm(price~.,plotDataset)
finalHousingSet1 <- finalHousingSet %>% 
  summarize(gradeSquared = gradeS, onWater = waterfront, price = price,
            sqftSquared = sqft_livingS, viewed = view, bathsSquared = bathsqR,
            wealthyzipcode = ifelse(zipcode == 98003 | 
                                      zipcode == 98022 | 
                                      zipcode == 98023 | 
                                      zipcode == 98070 | 
                                      zipcode == 98092 | 
                                      zipcode == 98198, FALSE, TRUE))
modelZipSum <- lm(price~.,finalHousingSet1)
finalHousingSet2 <- finalHousingSet1 %>% select(-bathsSquared)
modelZipSum2 <- lm(price~., finalHousingSet2)
summary(modelZipSum2)
finalSixPredictions <- finalHousingSet2 %>% filter(is.na(price))


predictions <- predict.lm(modelZipSum2, newdata = finalSixPredictions)
ggplot(finalSixPredictions, aes(x = sqftSquared, y = predictions)) + geom_point() +geom_smooth() 


write.csv(finalHousingSet, "final_housing_set")
Training<- Training %>%  filter(zipcode != 98010, zipcode != 98148)
Testing<- Testing %>%  filter(zipcode != 98010, zipcode != 98148)
trainPredictions <- predict.lm(modelFinalUpdate, newdata = Training)  
testPredictions <- predict.lm(modelFinalUpdate, newdata = Testing)
trainRMSE <- sqrt(mean( (trainPredictions - Training$price)^2 , na.rm=TRUE))
testRMSE <- sqrt(mean( (testPredictions - Testing$price)^2 , na.rm=TRUE))
ggplot(data = plotDataset, aes(x = sqft_livingS, y = price)) + geom_point(color = 'dark green') + 
  geom_smooth(aes(x = sqft_livingS, y = modelFinalUpdate$fitted.values)) +
  labs(x = "SQFT", y = "House Price ($)", title = "Price vs. SQFT with Final Model")
#trainRMSE
#testRMSE
#Testing RMSE of 114742.7
#Training RMSE of 137552.2

Task 4

library(tidyverse)
library(readr)
library(GGally)
library(olsrr)
house7<- read.csv("house_cleaned")
house <- house7 %>% select(price, grade,waterfront, view, bathsqr, houseID, sqft_living, zipcode)
house <- house %>% 
  summarize(houseID = houseID, gradeSquared = grade^2, onWater = waterfront, price = price,
           sqftSquared = sqft_living^2, viewed = view, 
           wealthyzipcode = ifelse(zipcode == 98003 | 
           zipcode == 98022 | zipcode == 98023 | 
           zipcode == 98070 | zipcode == 98092 | 
           zipcode == 98198, FALSE, TRUE))
modelHouse <- lm(price~wealthyzipcode+sqftSquared+gradeSquared+onWater+viewed,house)
summary(modelHouse)
houseBlank <- house %>% filter(is.na(price))
predictions <- predict.lm(modelHouse, newdata = houseBlank)
housePreds <- cbind(houseBlank, predictions)
summary(housePreds)
hp <- housePreds %>% summarize(houseID=houseID, price = predictions)
head(hp)
write.csv(hp, "Cole_Odegard")
Task 5

First: Using the same linear model you used for the competition data 
(let's call this Model A), find the predicted values for house prices
for your subset of data.

library(tidyverse)
library(readr)
library(GGally)
library(olsrr)
newSubset <- read.csv(file.choose())
write.csv(newSubset,"Cole_HousingSubset4000s")
newsubset1 <- newSubset %>% select(-lat)%>% select(-long) %>% 
  select(-sqft_lot)%>% select(-yr_renovated) %>% select(-condition)
newsubset1 <- newsubset1 %>% mutate(sqft_liv_chg = sqft_living - sqft_living15)
newsubset1 <- newsubset1[!house6$sqft_liv_chg == "NULL",]
newsubset1 <- newsubset1 %>% select(-sqft_living15)
newsubset1 <- newsubset1 %>% mutate(bathsqr = bathrooms^2, bedsqr = bedrooms^2, rootLiving = sqrt(sqft_living), newBuild = ifelse(2015-yr_built <= 10, "NEW", ifelse(2015-yr_built <= 30, "AGING", "OLD")))
newsubset1$zipcode = as.factor(newsubset1$zipcode)

newsubset2 <- newsubset1 %>% select(price, grade,waterfront, view, bathsqr, houseID, sqft_living, zipcode)
newsubset2 <- newsubset2 %>% 
  summarize(houseID = houseID, gradeSquared = grade^2, onWater = waterfront, price = price,
            sqftSquared = sqft_living^2, viewed = view, 
            wealthyzipcode = ifelse(zipcode == 98003 | 
                                      zipcode == 98022 | zipcode == 98023 | 
                                      zipcode == 98070 | zipcode == 98092 | 
                                      zipcode == 98198, FALSE, TRUE))
newPredictions <- predict.lm(modelHouse, newdata = newsubset2)
newSubsetwPredictions <- cbind(newPredictions,
                               newsubset2)
summary(newSubsetwPredictions)
ggplot(data = newSubsetwPredictions, aes(x = sqftSquared, y = price)) + geom_point()+
  geom_smooth(aes(y = newPredictions)) +
  labs(x = "SQFT Squared", y = "Price", title = "Figure 3.1: Price vs SQFT")
testRMSE <- sqrt(mean( (newPredictions - newSubsetwPredictions$price)^2 , na.rm=TRUE))
#testRMSE
#345,014 RMSE

Second: Make a new model that fits your new data better. 
Call this Model B. Explain how Model B is different than Model A. 
library(tidyverse)
library(readr)
library(GGally)
library(olsrr)
newSubset1 <- read.csv("Cole_HousingSubset4000s")
newsubset1 <- newSubset %>% select(-lat)%>% select(-long) %>% 
  select(-sqft_lot)%>% select(-yr_renovated) %>% select(-condition)
newsubset1 <- newsubset1 %>% mutate(sqft_liv_chg = sqft_living - sqft_living15)
newsubset1 <- newsubset1[!house6$sqft_liv_chg == "NULL",]
newsubset1 <- newsubset1 %>% select(-sqft_living15)
newsubset1 <- newsubset1 %>% mutate(bathsqr = bathrooms^2, bedsqr = bedrooms^2, rootLiving = sqrt(sqft_living), newBuild = ifelse(2015-yr_built <= 10, "NEW", ifelse(2015-yr_built <= 30, "AGING", "OLD")))
modelNew <- lm(price~.,newsubset1)
forselect <- ols_step_forward_p(modelNew)
forselect
newsubset2 <- newsubset1 %>% select(sqft_living, waterfront, grade, yr_built, newBuild, floors, bathsqr, bedrooms, price, view)
modelNew1 <- lm(price~.,newsubset2)
summary(modelNew1)
newsubset2 <- newsubset2 %>% select(-view, -floors,-newBuild)
modelNew1 <- lm(price~.,newsubset2)
summary(modelNew1)
forselect <- ols_step_forward_p(modelNew1)
forselect
modelnewUpdated <- modelNew1
summary(modelnewUpdated)

newsubset3 <- newSubset %>% select(-lat)%>% select(-long) %>% 
  select(-sqft_lot)%>% select(-yr_renovated) %>% select(-condition)
newsubset3 <- newsubset3 %>% mutate(sqft_liv_chg = sqft_living - sqft_living15)
newsubset3 <- newsubset3[!house6$sqft_liv_chg == "NULL",]
newsubset3 <- newsubset3 %>% select(-sqft_living15)
newsubset3 <- newsubset3 %>% mutate(bathsqr = bathrooms^2, bedsqr = bedrooms^2, rootLiving = sqrt(sqft_living), newBuild = ifelse(2015-yr_built <= 10, "NEW", ifelse(2015-yr_built <= 30, "AGING", "OLD")))
newsubset3$zipcode = as.factor(newsubset3$zipcode)

newsubset3 <- newsubset3 %>% select(price, grade,waterfront, view, bathsqr, bedrooms, houseID, sqft_living, zipcode, yr_built)
newsubset3 <- newsubset3 %>% 
  summarize(houseID = houseID, yr_built = yr_built, grade = grade, gradeSquared = grade^2, waterfront = waterfront, price = price,
            sqftSquared = sqft_living^2, bedrooms = bedrooms, viewed = view, sqft_living = sqft_living, bathsqr = bathsqr,
            wealthyzipcode = ifelse(zipcode == 98003 | 
                                      zipcode == 98022 | zipcode == 98023 | 
                                      zipcode == 98070 | zipcode == 98092 | 
                                      zipcode == 98198, FALSE, TRUE))
modelHouse <- lm(price~wealthyzipcode+sqftSquared+gradeSquared+waterfront+viewed,newsubset3)
newPredictions <- predict.lm(modelHouse, newdata = newsubset3)
newPredictionsUpdate <- predict.lm(modelnewUpdated, newdata = newsubset3)
newSubsetwPredictions <- cbind(newPredictions,
                               newPredictionsUpdate,
                               newsubset3)
ggplot(data = newSubsetwPredictions, aes(x = sqftSquared, y = price)) + geom_point()+
  geom_smooth(aes(y = newPredictions), color = "red") + geom_smooth(aes(y = newPredictionsUpdate), color = "green")+
  labs(x = "SQFT Squared", y = "House Price ($)", title = "Figure 3.1: Price vs SQFT")
