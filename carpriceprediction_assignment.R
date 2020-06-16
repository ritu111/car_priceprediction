library(tidyr)
library(dplyr)
library(MASS)
library(car)
carprice<- read.csv("CarPrice_Assignment.csv")
View(carprice)
str(carprice)
summary(carprice)
## DATA CLEANING AND PREPARATION ##
which(duplicated(carprice))              #Checking for duplicate records(No duplicates found)
which(duplicated(carprice$car_ID))       #Checking for duplicate car_ID(No duplicates found)
which(is.na(carprice))                   #Checking for NA Values(No NA values found)
which(carprice==" ")                     #No Blank values

#Separating Company and Model Name
carprice<-separate(carprice,CarName,into="Company",sep=" ",remove=FALSE)
View(carprice$Company)

##Treating mis-spelled Company names
carprice$Company<-gsub("maxda","mazda",carprice$Company)
carprice$Company<-gsub("nissan","Nissan",carprice$Company)
carprice$Company<-gsub("toyouta","toyota",carprice$Company)
carprice$Company<-gsub("vw","volkswagen",carprice$Company)
carprice$Company<-gsub("vokswagen","volkswagen",carprice$Company)
carprice$Company<-gsub("porcshce","porsche",carprice$Company)

##Treating Outliers ##
quantile(carprice$wheelbase,probs=seq(0,1,0.01))
boxplot(carprice$wheelbase)         ##outliers
carprice$wheelbase[which(carprice$wheelbase>114.200)]<-114.200

quantile(carprice$carlength,probs=seq(0,1,0.01))
boxplot(carprice$carlength)         ##outliers
carprice$carlength[which(carprice$carlength>202.480)]<-202.480
carprice$carlength[which(carprice$carlength<155.900)]<-155.900

quantile(carprice$carwidth,probs=seq(0,1,0.01))
boxplot(carprice$carwidth)          ##outliers
carprice$carwidth[which(carprice$carwidth>71.700)]<-71.700
carprice$carwidth[which(carprice$carwidth<62.536)]<-62.536

quantile(carprice$carheight,probs=seq(0,1,0.01))
boxplot(carprice$carheight)         ##no outliers

quantile(carprice$curbweight,probs=seq(0,1,0.01))
boxplot(carprice$curbweight)        
carprice$curbweight[which(carprice$curbweight<1819.72)]<-1819.72
carprice$curbweight[which(carprice$curbweigth>3503.00)]<-3503.00

quantile(carprice$enginesize,probs=seq(0,1,0.01))
boxplot(carprice$enginesize)        ##outliers
carprice$enginesize[which(carprice$enginesize>209.00)]<-209.00

quantile(carprice$boreratio,probs=seq(0,1,0.01))
boxplot(carprice$boreratio)         ##outliers
carprice$boreratio[which(carprice$boreratio<2.9100)]<-2.9100

quantile(carprice$stroke,probs=seq(0,1,0.01))
boxplot(carprice$stroke)            ## Outliers
carprice$stroke[which(carprice$stroke<2.6400)]<-2.6400
carprice$stroke[which(carprice$stroke>3.6400)]<-3.6400

quantile(carprice$compressionratio,probs=seq(0,1,0.01))
boxplot(carprice$compressionratio)  ## Outliers
carprice$compressionratio[which(carprice$compressionratio>10.0000)]<-10.0000

quantile(carprice$horsepower,probs=seq(0,1,0.01))
boxplot(carprice$horsepower)        ## Outliers
carprice$horsepower[which(carprice$horsepower>184.00)]<-184.00

quantile(carprice$peakrpm,probs=seq(0,1,0.01))
boxplot(carprice$peakrpm)           ## Outliers
carprice$peakrpm[which(carprice$peakrpm>6000)]<-6000

quantile(carprice$citympg,probs=seq(0,1,0.01))
boxplot(carprice$citympg)           ## Outliers
carprice$citympg[which((carprice$citympg>38.00))]<-38.00

quantile(carprice$highwaympg,probs=seq(0,1,0.01))
boxplot(carprice$highwaympg)        ## Outliers
carprice$highwaympg[which(carprice$highwaympg>49.88)]<-49.88

#Creating levels for Symboling variable
carprice$symboling<-as.factor(carprice$symboling)
levels(carprice$symboling)[1]<-"safe"
levels(carprice$symboling)[2:4]<-"moderatelysafe"
levels(carprice$symboling)[3:4]<-"risky"

#Creating derived variable Average mpg
carprice$avgmpg<-as.numeric(carprice$citympg+carprice$highwaympg)/2

#Treating Categorical Variables
dummy_1<-data.frame(model.matrix(~Company,data=carprice))
dummy_1<-dummy_1[,-1]
View(dummy_1)

dummy_2<-data.frame(model.matrix(~carbody,data=carprice))
dummy_2<-dummy_2[,-1]
View(dummy_2)

dummy_3<-data.frame(model.matrix(~drivewheel,data=carprice))
dummy_3<-dummy_3[,-1]
View(dummy_3)

dummy_4<-data.frame(model.matrix(~enginetype,data=carprice))
dummy_4<-dummy_4[,-1]
View(dummy_4)

dummy_5<-data.frame(model.matrix(~cylindernumber,data=carprice))
dummy_5<-dummy_5[,-1]
View(dummy_5)

dummy_6<-data.frame(model.matrix(~fuelsystem,data=carprice))
dummy_6<-dummy_6[,-1]
View(dummy_6)

dummy_7<-model.matrix(~symboling,data=carprice)
dummy_7<-dummy_7[,-1]

#Combining dummy variables to dataset carprice
carprice<-cbind(carprice[,-4],dummy_1)
carprice<-cbind(carprice[,-7],dummy_2)
carprice<-cbind(carprice[,-7],dummy_3)
carprice<-cbind(carprice[,-13],dummy_4)
carprice<-cbind(carprice[,-13],dummy_5)
carprice<-cbind(carprice[,-14],dummy_6)
carprice<-cbind(carprice[,-2],dummy_7)

#door number,aspiration,enginelocation-levels
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]

levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]

levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

levels(carprice$fueltype)<-c(1,0)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]

carprice<-carprice[,-1]                                               ##Removing column carId
carprice<-carprice[,-1]                                               ##Removing column carname

## MODEL BUILDING ##
View(carprice)
set.seed(100)
trainindices<-sample(1:nrow(carprice),0.7*nrow(carprice))          
train.carprice<-carprice[trainindices,]                               ## Creating 70% Train data
test.carprice<-carprice[-trainindices,]                               ## Creating 30% Train data

# MODEL 1
model1<-lm(price~.,data=train.carprice)
summary(model1)#Multiple R-squared:  0.9776,	Adjusted R-squared:  0.9629
#Performing StepAIC
step<-stepAIC(model1,direction="both")
step

# MODEL 2
model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + stroke + horsepower + Companybmw + 
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train.carprice)
summary(model_2)  #Multiple R-squared:  0.976,	Adjusted R-squared:  0.9682
vif(model_2)

# MODEL 3
# Can't remove cylindernumberfour as it has VIF but also high significance.
# So,removing carbodysedan variable which has low significance
model_3<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + stroke + horsepower + Companybmw + 
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train.carprice)
summary(model_3) #Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9669
vif(model_3)

# MODEL 4
#Can't remove cylindernumberfour,cylindernumbersix,curbweight because they have high significance with high VIF
#So,Removing carwidth because it has high VIF 
model_4<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + stroke + horsepower + Companybmw + 
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive + cylindernumberfour + cylindernumbersix, 
            data = train.carprice)
summary(model_4)  #Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9644
vif(model_4)

# MODEL 5
cor(carprice$cylindernumbersix,carprice$cylindernumberfour)
#as cylindernumberfour is highly correlated(67%) with cylindernumbersix we can remove cylindernumbersix
model_5<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_5)##Multiple R-squared:  0.9653,	Adjusted R-squared:  0.9553 
vif(model_5)

# MODEL 6
cor(carprice$horsepower,carprice$curbweight)
#removed curbweight as horsepwer and curbweight are highly correlated(77%) and less significance
model_6<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_6)#Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9417 
vif(model_6)

# MODEL 7
#Can't remove variables which have high vif because they also have high significance.
#Removing carbodyhardtop as it has highest p-value
model_7<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo +carbodyhatchback + 
              carbodywagon + drivewheelrwd + enginetyperotor + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_7)#Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9422 
vif(model_7)

# MODEL 8
#Can't remove variables which have high vif because they also have high significance.
#Removing enginetyperotor because of high p-value
model_8<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo +carbodyhatchback + 
              carbodywagon + drivewheelrwd + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_8)#Multiple R-squared:  0.9544,	Adjusted R-squared:  0.9427 
vif(model_8)

# MODEL 9
#Can't remove variables which have high vif because they also have high significance.
#Removing carbodyhatchback because of high p-value and no stars(doesn't have significance)
model_9<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo +
              carbodywagon + drivewheelrwd + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_9)#Multiple R-squared:  0.9543,	Adjusted R-squared:  0.943 

# MODEL 10
#Can't remove variables which have high vif because they also have high significance.
#Removing carbodywagon because high p-value and no stars(doesn't have significance)
model_10<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              stroke + Companybmw + horsepower+
              Companybuick + Companychevrolet + Companydodge + Companyhonda + 
              Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
              Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
              Companyrenault + Companysaab + Companysubaru + Companytoyota + 
              Companyvolkswagen + Companyvolvo + drivewheelrwd + 
              cylindernumberfive +cylindernumberfour, 
            data = train.carprice)
summary(model_10)#Multiple R-squared:  0.9537,	Adjusted R-squared:  0.9428 
vif(model_10)

# MODEL 11
#Removing stroke because high VIF,high p-value and no stars(doesn't have significance)
model_11<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + Companyvolvo + drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_11)#Multiple R-squared:  0.953,	Adjusted R-squared:  0.9425
vif(model_11)

# MODEL 12
#Can't remove variables which have higher VIF because they have significance
#Removing aspiration because high p-value and no stars(doesn't have significance)
model_12<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + CompanyNissan + Companypeugeot + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + Companyvolvo + drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_12)#Multiple R-squared:  0.9523,	Adjusted R-squared:  0.9421
vif(model_12)

# MODEL 13
#Can't remove other variables with high VIF as they also have significance.
#Removing Companypeugeot because of high VIF and high p-value(doesn't have significance)
model_13<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen + Companyvolvo + drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_13)#Multiple R-squared:  0.9514,	Adjusted R-squared:  0.9415 
vif(model_13)

# MODEL 14
#Can't remove other variables with high VIF as they also have significance.
#Removing companyvolvo because it doesn't have significance
model_14<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyisuzu + Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen +drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_14)#Multiple R-squared:  0.9512,	Adjusted R-squared:  0.9418
vif(model_14)

# MODEL 15
#Can't remove other variables with high VIF as they also have significance.
#Removing Companyisuzu with less significance among the variables
model_15<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + Companymercury + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen +drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_15)#Multiple R-squared:  0.9494,	Adjusted R-squared:  0.9402
vif(model_15)

# MODEL 16
#Can't remove other variables with high VIF as they also have significance.
#Removing Companymercury with less significance among the variables
model_16<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companyrenault + Companysaab + Companysubaru + Companytoyota + 
               Companyvolkswagen +drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_16)#Multiple R-squared:  0.9475,	Adjusted R-squared:  0.9383 
vif(model_16)

# MODEL 17
#Removing Companysaab with less significance among the variables
model_17<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companyrenault + Companysubaru + Companytoyota + 
               Companyvolkswagen +drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_17)#Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9367
vif(model_17)

# MODEL 18
#Can't remove other variables with high VIF as they also have significance.
#Removing Companyrenualt with less significance among the variables
model_18<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               Companyvolkswagen +drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_18)#Multiple R-squared:  0.9429,	Adjusted R-squared:  0.9341 

# MODEL 19
#Removing Volkswagen with less significance among the variables
model_19<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companychevrolet + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_19)#Multiple R-squared:  0.9403,	Adjusted R-squared:  0.9316 
vif(model_19)

# MODEL 20
#drivewheerwd has high vif, but can't remove it as Companychevrolet has high p-value with same significance as drivewheelrwd
#Removing Companychevrolet with less significance among the variables(more p value than drivewheelrwd)
model_20<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               drivewheelrwd + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_20)# Multiple R-squared:  0.9383,	Adjusted R-squared:  0.9299 
vif(model_20)

# MODEL 21
#Can't remove other variables with high VIF as they have high significance
#Removing drivewheelrwd with less significance among the variables
model_21<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + Companyhonda + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_21)#Multiple R-squared:  0.9362,	Adjusted R-squared:  0.9281 ,predicted-0.8471646
vif(model_21)

# MODEL 22
#Can't remove other variables with high vif as they have high significance
#Removing companyhonda with less significance among the variables
model_22<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + 
               Companyjaguar + Companymazda + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_22)#Multiple R-squared:  0.9331,	Adjusted R-squared:  0.9252 ,predicted-0.8476037
vif(model_22)

# MODEL 23
#Can't remove other variables with high vif as they have high significance
#Removing Companymazda with less significance among the variables
model_23<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + 
               Companyjaguar + 
               Companymitsubishi + CompanyNissan + Companyplymouth + 
               Companysubaru + Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_23)#Multiple R-squared:  0.9313,	Adjusted R-squared:  0.9238 ,predicted-0.8539957
vif(model_23)

# MODEL 24
#Can't remove other variables with high vif as they have high significance
#Removing CompanyNissan with less significance and no stars
model_24<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + 
               Companyjaguar + 
               Companymitsubishi +Companyplymouth + 
               Companysubaru + Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_24)#Multiple R-squared:   0.93,	Adjusted R-squared:  0.9229,predicted-0.8432932
vif(model_24)

# MODEL 25
#Can't remove other variables with high vif as they have significance
#Removing Companysubaru with less significance and no stars
model_25<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + 
               Companyjaguar + 
               Companymitsubishi +Companyplymouth + 
               Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_25)#Multiple R-squared:  0.9281,	Adjusted R-squared:  0.9214 ,predicted-0.8397524
vif(model_25)

# MODEL 26
#Can't remove other variables with high vif as they have significance
#Removing Companyplymouth with less significance among the variables
model_26<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + Companydodge + 
               Companyjaguar + 
               Companymitsubishi +
               Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_26)#Multiple R-squared:  0.9257,	Adjusted R-squared:  0.9194, predicted-0.8410303
vif(model_26)

# MODEL 27
#Can't remove other variables with high vif as they have significance
#Removing Companydodge with less significance
model_27<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + 
               Companyjaguar + 
               Companymitsubishi +
               Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_27)#Multiple R-squared:  0.9235,	Adjusted R-squared:  0.9177,predicted-0.8412255
vif(model_27)

# MODEL 28
#Can't remove other variables with high vif as they have high significance
#Removing Companymitsubishi with less significance
model_28<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + 
               Companyjaguar + 
               Companytoyota + 
               cylindernumberfive +cylindernumberfour, 
             data = train.carprice)
summary(model_28)#Multiple R-squared:  0.9214,	Adjusted R-squared:  0.9161 ,predicted-0.8350947
vif(model_28)

# MODEL 29
#Can't remove other variables with high vif as they have high significance
#Removing cylindernumberfive with less significance
model_29<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + 
               Companyjaguar + 
               Companytoyota + 
               cylindernumberfour, 
             data = train.carprice)
summary(model_29)#Multiple R-squared:  0.9186,	Adjusted R-squared:  0.9137 ,predicted-0.8484068
vif(model_29)

# MODEL 30
#Can't remove other variables with high vif as they have high significance
#Removing Companytoyota with less significance
model_30<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + 
               Companyjaguar + 
               cylindernumberfour, 
             data = train.carprice)
summary(model_30)#Multiple R-squared:  0.9161,	Adjusted R-squared:  0.9117,predicted-0.8485466
vif(model_30)

# MODEL 31
#Can't remove horsepower with high VIF, because it has high significance
#Removing cylindernumberfour with less significance
model_31<-lm(formula = price ~ enginelocation + wheelbase + 
               Companybmw + horsepower+
               Companybuick + 
               Companyjaguar , 
             data = train.carprice)
summary(model_31)#Multiple R-squared:  0.9106,	Adjusted R-squared:  0.9067 ,predicted-0.8464315

# MODEL 32(FINAL MODEL)
#As the intercept has only one star(less significant), so we need to remove one more variable
#Removing Companyjaguar with high p-value amongst all 6 variables
model_32<-lm(formula = price ~ enginelocation +wheelbase+
               Companybmw + horsepower+ Companybuick, 
             data = train.carprice)
summary(model_32)# Multiple R-squared:  0.8915,	Adjusted R-squared:  0.8875 , Predicted- 0.8067576
                 # With all variables highly significant and intercept is also highly significant
                 # 5 Important Variables which are included in the final model are Enginelocation, 
                 #  Wheelbase, Companybmw, horsepower, Companybuick
vif(model_32)    # vif for all 5 variables is less than 2.

Predict<-predict(model_32,test.carprice[,-18])
(cor(test.carprice$price, Predict))^2  ## Predicted value is 0.8067576


