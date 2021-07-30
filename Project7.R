hospital <- read.csv(file.choose())
View(hospital)

#1)age category frequently visits the hospital:
hist(hospital$AGE,main = 'Age category',
     breaks = 5,
     freq = T,
     col = 'steel blue', border = 'white')

#age category which has highest expenditure:
age_exp <- aggregate(hospital$TOTCHG,list(hospital$AGE),sum)
age_exp

age_freq <- aggregate(hospital$TOTCHG,list(hospital$AGE),length)
age_freq


#2)diagnosis related group which has more frequent visits:
dia_freq <- aggregate(hospital$TOTCHG,list(hospital$APRDRG),length)
dia_freq
#diagnosis related group which has more expenditure:
hist(hospital$APRDRG,main = 'DIagnosis related category',
     breaks = 5,
     freq = T,col = 'red')
dia_exp <- aggregate(hospital$TOTCHG,list(hospital$APRDRG),sum)
dia_exp

#3)race reltion with hospitalization costs:
plot(hospital$RACE,hospital$TOTCHG,
     main = 'RACE vs Hospitalization costs',
     xlab = 'RACE',
     ylab = 'Hospitalization costs')



cov(hospital$RACE,hospital$TOTCHG,use = "complete.obs")

#there is a negative correlation between race and hospitalization costs
cor(hospital$RACE,hospital$TOTCHG,use = "complete.obs")

#4)severity of hospital costs by Gender:
hos_gender <- aggregate(hospital$TOTCHG,list(hospital$FEMALE),sum)
names(hos_gender) <- c('Gender','Hospital costs')
hos_gender
barplot(as.matrix(hos_gender),xlab='Gender',ylab='Hospital Costs',beside = T)

#4)severity of hospital costs by Age:
age_expenditure <- aggregate(hospital$TOTCHG,list(hospital$AGE),sum)
names(age_expenditure) <- c('Age','Hospital costs')
age_expenditure

barplot(as.matrix(age_expenditure),xlab = 'Age', ylab = 'Hospital costs',beside = T)


#5)finding out whether length of stay can be predicted by age,gender,race
cor(hospital$LOS,hospital$AGE,use="complete.obs")
cor(hospital$LOS,hospital$FEMALE,use="complete.obs")
cor(hospital$LOS,hospital$RACE,use="complete.obs")

#Length of stay has very weak positive correlation with Gender and negative correlation with Age and Race
#Linear regression for predicting LOS values
set.seed(12)
train_indices <- sample(1:nrow(hospital),nrow(hospital)*0.75)
train_indices
train_hos <- hospital[train_indices,]
test_hos <- hospital[-train_indices]

linear <- lm(LOS~.,data = train_hos)
summary(linear)

library(dplyr)
fit <- hospital %>% 
  select(-TOTCHG,-APRDRG) 

final_linear <- lm(LOS~.,data=fit)
summary(final_linear)

#predicting LOS values using all variables in the data:
predict_linear <- round(predict(linear,data=train_hos),0)
predict_linear

predict_lineartest <- round(predict(linear,data=test_hos),0)
predict_lineartest

#predicting LOS values using AGE,GENDER & RACE variables in the data:
final_fit <- round(predict(final_linear,data=test_hos),0)
final_fit


#6)the agency wants to find the variable that mainly affects hospital costs
cor(hospital$TOTCHG,hospital$AGE,use="complete.obs")
cor(hospital$TOTCHG,hospital$FEMALE,use="complete.obs")
cor(hospital$TOTCHG,hospital$LOS,use="complete.obs")
cor(hospital$TOTCHG,hospital$RACE,use="complete.obs")
cor(hospital$TOTCHG,hospital$APRDRG,use="complete.obs")

#LOS affects hospital costs much with correlation value of 0.623193(postive correlation)
plot(hospital$LOS,hospital$TOTCHG,
     main = 'LOS vsHospitalization costs',
     xlab = 'LOS',
     ylab = 'Hospitalization costs')






