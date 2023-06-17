##Working Directory Set Up
data <- read.csv("C:/Users/arpit/Downloads/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv")
names(data)
head(data,n=20)
str(data)
glimpse(data)
### Working Data
library(dplyr)
unique(data$Discharge.Year)
LOSdata <- select(data,-Hospital.County,-Zip.Code...3.digits,-Operating.Certificate.Number,-Facility.Id,-Discharge.Year,-CCS.Diagnosis.Description,-CCS.Procedure.Description,-APR.DRG.Description,
                  -APR.MDC.Description,
                  -APR.Severity.of.Illness.Description)
names(LOSdata)
glimpse(LOSdata)
head(LOSdata)
unique(data$APR.Severity.of.Illness.Description)

## Data Cleanup and  Manipulation
LOSdata$Total.Charges <- as.numeric(gsub( "\\$", "", paste(LOSdata$Total.Charges)))
LOSdata$Total.Costs <- as.numeric(gsub( "\\$", "", paste(LOSdata$Total.Costs)))
LOSdata$Length.of.Stay <- as.integer(LOSdata$Length.of.Stay)
LOSdata$CCS.Diagnosis.Code <- factor(LOSdata$CCS.Diagnosis.Code)
LOSdata$APR.MDC.Code <- factor(LOSdata$APR.MDC.Code)
LOSdata$CCS.Procedure.Code <- factor(LOSdata$CCS.Procedure.Code)
LOSdata$APR.DRG.Code <- factor(LOSdata$APR.DRG.Code)
LOSdata$Facility.Id <- as.factor(LOSdata$Facility.Id)
#LOSdata1 <- filter(LOSdata,Type.of.Admission!= "Not Available")
LOSdata1 <- filter(LOSdata,Patient.Disposition != 'Expired')
#LOSdata4 <- select(LOSdata2,-Hospital.County,-Zip.Code...3.digits,-Total.Charges,-Total.Costs)


#### Modelling for MDC code =6 i.e Digestive Issues
LOSdata2 <- filter(LOSdata1,APR.MDC.Code == "6")
LOSdata2$CCS.Diagnosis.Code <- factor(LOSdata2$CCS.Diagnosis.Code)
LOSdata2$APR.MDC.Code <- factor(LOSdata2$APR.MDC.Code)
LOSdata2$CCS.Procedure.Code <- factor(LOSdata2$CCS.Procedure.Code)
LOSdata2$APR.DRG.Code <- factor(LOSdata2$APR.DRG.Code)


### Removing Unneccessary Columns
LOSdata3 <- select(LOSdata2,-Facility.Name,-Emergency.Department.Indicator,-Total.Charges,-Total.Costs,-Birth.Weight,-Abortion.Edit.Indicator,
                   -Payment.Typology.2,-Payment.Typology.3)


### Data Cleaning -- Imputing the Missing Values
install.packages("forcats")
library(forcats)## for explicit defining implicit missing values
LOSdata6$Type.of.Admission <- fct_explicit_na(LOSdata6$Type.of.Admission)## for explicit defining implicit missing values

LOSdata3%>%
  filter(Payment.Typology.1 == 'Unknown')%>%
LOSdata3$Payment.Typology.1[LOSdata3$Payment.Typology.1=='Unknown'] <- 'Self-Pay'
LOSdata3$Type.of.Admission[LOSdata3$Type.of.Admission=='Not Available'] <- 'Emergency'
LOSdata3$Health.Service.Area[LOSdata3$Health.Service.Area == '(Missing)'] <- 'New York City'
LOSdata3$Ethnicity[LOSdata3$Ethnicity=='Unknown'] <- 'Not Span/Hispanic'

data.frame(LOSdata3 %>%
  group_by(Ethnicity)%>%
  summarize(count= n())%>%
    arrange(desc(count)))

unique(LOSdata4$Health.Service.Area)
LOSdata3%>%
  group_by(Payment.Typology.3)%>%
    summarize(count= n())

str(LOSdata3)

## Data Manipulation-- Dropping used Levels from Columns
LOSdata3$Health.Service.Area <- factor(LOSdata3$Health.Service.Area)
LOSdata3$Ethnicity <- factor(LOSdata3$Ethnicity)
LOSdata3$Type.of.Admission <- factor(LOSdata3$Type.of.Admission)
LOSdata3$Patient.Disposition <- factor(LOSdata3$Patient.Disposition)
LOSdata3$Payment.Typology.1 <- factor(LOSdata3$Payment.Typology.1)
LOSdata3$APR.Medical.Surgical.Description <- factor(LOSdata3$APR.Medical.Surgical.Description)
LOSdata3$APR.Risk.of.Mortality <- factor(LOSdata3$APR.Risk.of.Mortality)

LOSdata3$APR.Severity.of.Illness.Code <- as.factor(LOSdata3$APR.Severity.of.Illness.Code)

#######Removing After Patient Admission columns

LOSdata4 <- select(LOSdata3,-Patient.Disposition,- CCS.Procedure.Code,-APR.MDC.Code,-APR.DRG.Code,-APR.Risk.of.Mortality,-APR.Severity.of.Illness.Code,-CCS.Diagnosis.Code)
str(LOSdata5)

### Removing LOS as 1 data
LOSdata5 <- filter(LOSdata4,Length.of.Stay != 1)
unique(LOSdata5$Length.of.Stay)
###Visualization -- Exploratory Data Analysis
install.packages("ggplot2")
library(ggplot2)
ggplot(LOSdata3,aes(x= Health.Service.Area ,y=Length.of.Stay)) + geom_boxplot()

ggplot(LOSdata6,aes(x= Patient.Disposition ,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(x=Gender,y=Length.of.Stay)) + geom_point()
ggplot(LOSdata6,aes(x=Age.Group,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata2,aes(x=APR.MDC.Code,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(x=Health.Service.Area,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(APR.Medical.Surgical.Description)) +xlab("Medical/Surgical")+ ylab("Frequency") +geom_bar(COLOR = "green",fill = "#FF6666",width = 0.3)
ggplot(LOSdata6,aes(Age.Group)) +xlab("Age Group")+ ylab("Frequency") +geom_bar(color = "red",fill = "Blue",width = 0.3)

ggplot(LOSdata4,aes(x=Type.of.Admission,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata19,aes(x=APR.DRG.Code,y=Length.of.Stay))+geom_bar(stat = "identity")

ggplot(LOSdata6,aes(x=Gender,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(x=Age.Group,y=Length.of.Stay)) + geom_boxplot
ggplot(LOSdata2,aes(x=APR.MDC.Code,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(x=Health.Service.Area,y=Length.of.Stay)) + geom_boxplot()
ggplot(LOSdata6,aes(Type.of.Admission)) +xlab("Medical/Surgical")+ ylab("Frequency") +geom_bar(fill = "#FF6666")
ggplot(LOSdata6,aes(Type.of.Admission)) +xlab("Type of Admission")+ ylab("Frequency") +geom_bar(color = "red",fill = "brown",width = 0.3)




str(LOSdata6)
### Linear Model
set.seed(3333)
train_df <- sample(nrow(LOSdata5), 0.7*nrow(LOSdata5),replace=FALSE)
TrainSet <- LOSdata5[train_df,]
ValidSet <- LOSdata5[-train_df,]
linear <- lm(Length.of.Stay~. ,data=TrainSet)#this is model fitting
names(LOSdata5)
linear1 <- aov(Length.of.Stay~. ,data=TrainSet)
linear
linear <- aov(Length.of.Stay~Age.Group*Gender*Type.of.Admission*APR.Medical.Surgical.Description*Payment.Typology.1 ,data=TrainSet)
plot(linear,1)
plot(factanova,1)
plot(factanova,2)
plot(linear1,2)
leveneTest(Length.of.Stay~Age.Group*Gender*Type.of.Admission*APR.Medical.Surgical.Description*Payment.Typology.1 ,data=TrainSet)
library(car)
install.packages("car")
library(car)
leveneTest(len ~ supp*dose, data = my_data)
factanova <- aov(Length.of.Stay~Age.Group*Type.of.Admission + Age.Group*APR.Medical.Surgical.Description+Type.of.Admission*APR.Medical.Surgical.Description+APR.Medical.Surgical.Description*Payment.Typology.1+Gender,data=TrainSet)
# Extract the residuals
aov_residuals <- residuals(object = linear)
aov_residuals <- residuals(object = factanova)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
summary(factanova)
anova(linear)
summary(linear)
model.tables(linear, type="means", se = TRUE)
###anova
predict_lm <- predict(linear,ValidSet)
RMSE_lm <- sqrt(mean(predict_lm-ValidSet$Length.of.Stay)^2)
RMSE_lm
MAE_lm <- mean(abs(predict_lm-ValidSet$Length.of.Stay))
MAE_lm
##factorial Anova
predict_lm <- predict(factanova,ValidSet)
RMSE_lm <- sqrt(mean(predict_lm-ValidSet$Length.of.Stay)^2)
RMSE_lm
MAE_lm <- mean(abs(predict_lm-ValidSet$Length.of.Stay))
MAE_lm
qqnorm(TrainSet$Length.of.Stay)
qqline(TrainSet$Length.of.Stay)
library(ggplot2)
ggplot(TrainSet,aes(x=Length.of.Stay)) +geom_histogram()
str(TrainSet)

LOSdata6 <- select(LOSdata6,-Race,-Ethnicity,-Health.Service.Area)

##### Recategorization for Modelling as per parameter significance.
LOSdata6$Type.of.Admission[LOSdata6$Type.of.Admission == '(Missing)'] <- 'Other'
LOSdata6$Health.Service.Area <- factor(LOSdata6$Health.Service.Area)
unique(LOSdata6$Payment.Typology.1)
levels <- levels(LOSdata6$Payment.Typology.1)
levels[length(levels) + 1] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1 <- factor(LOSdata6$Payment.Typology.1, levels = levels)
LOSdata6$Payment.Typology.1 <- factor(LOSdata6$Payment.Typology.1)
LOSdata6$Type.of.Admission <- factor(LOSdata6$Type.of.Admission)
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Blue Cross/Blue Shield'] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Private Health Insurance'] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Miscellaneous/Other'] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Federal/State/Local/VA'] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Department of Corrections'] <- "SelfPay/Private Insurance"
LOSdata6$Payment.Typology.1[LOSdata6$Payment.Typology.1=='Self-Pay'] <- "SelfPay/Private Insurance"



LOSdata6$Type.of.Admission[LOSdata6$Type.of.Admission=='Trauma'] <- 'Other'
LOSdata6$Type.of.Admission <- fct_explicit_na(LOSdata6$Type.of.Admission)## for explicit defining implicit missing values

###Correlation Test
tb1 <- table(LOSdata5$Gender,LOSdata5$Payment.Typology.1)
tb1
chisq.test(tb1)
summary(chitest)
###Decison Trees
install.packages("rpart")
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
set.seed(3333)
train <- sample(nrow(LOSdata5), 0.7*nrow(LOSdata5),replace=FALSE)
TrainSet_dt <- LOSdata5[train,]
ValidSet_dt <- LOSdata5[-train,]
dt <- rpart(Length.of.Stay~.,data=TrainSet_dt,method = "anova",control = rpart.control(cp=0.001,maxdepth = 8,minsplit = 3))
predict_dt <- predict(dt,ValidSet_dt)#rpart for decision treees
print(predict_dt)
### Examining the results
printcp(dt)
rpart.plot(dt)
print(dt)
summary(TrainSet_dt)
prp(dt)

####RMSE AND MAE for Decision Trees
RMSE_dt <- sqrt(mean(predict_dt - ValidSet_dt$Length.of.Stay)^2)
RMSE_dt
MAE_dt <- mean(abs(predict_dt - ValidSet_dt$Length.of.Stay))
MAE_dt

###Pruned Trees
install.packages("rattle")
library(rattle)
min.xerror <- dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"]
min.xerror

dt.pruned <- prune(dt,cp = min.xerror)
fancyRpartPlot(dt.pruned) ###through Rattle Package

predict_pr <- predict(dt.pruned,ValidSet_dt)
RMSE_pruned <- sqrt(mean(predict_pr - ValidSet_dt$Length.of.Stay)^2)
RMSE_pruned
MAE_pruned <- mean(abs(predict_pr - ValidSet_dt$Length.of.Stay))
MAE_pruned

### Random Forests

install.packages("randomForest")
library(randomForest)
set.seed(100)
train <- sample(nrow(LOSdata5), 0.7*nrow(LOSdata5),replace=FALSE)
TrainSet_rf <- LOSdata5[train,]
ValidSet_rf <- LOSdata5[-train,]
model2 <- randomForest(Length.of.Stay ~ ., data = TrainSet_rf, importance = TRUE,na.action = na.omit)
lapply(train, function(x) any(is.na(x)))
sum(na.omit(LOSdata20$APR.DRG.Code) == " ") #34282
lapply(train, function(x) any(is.na(x)))
summary(model2)
print(model2)

##Distribution of Dependent Variable -- Length Of Stay
install.packages("moments")##for Skewness and Kurtosis
library(moments)
skewness(LOSdata20$Length.of.Stay) #0.239
kurtosis(LOSdata20$Length.of.Stay) #2.527
summary(LOSdata20$Length.of.Stay) # for Mean and Median
summary()
## function to calculate Mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(LOSdata19$Length.of.Stay) ## using userdefined function




library(dplyr)
LOSdata6 %>%
  group_by(CCS.Diagnosis.Code) %>%
    summarize(count = n())%>%
      arrange(desc(count))%>%
      print(n=100)

LOSdata7 <- LOSdata6 %>%
  group_by(CCS.Diagnosis.Code) %>%
  summarize(count = n())%>%
    filter(count > 4)
print(LOSdata7,n=100)




########## Anova Understanding########
###### LOSdata6-  Final Dataset Obtained#######
###### Application on Gender#########

### Assumption 1 : factor Levels should be normally distributed####

names(LOSdata6)
library(ggplot2)
library(dplyr)

group_by(LOSdata6, Gender) %>%
  summarise(
    count = n(),
    mean = mean(Length.of.Stay, na.rm = TRUE),
    sd = sd(Length.of.Stay, na.rm = TRUE)
  )


##### Visualisation

install.packages("ggpubr")
library(ggpubr)

ggboxplot(LOSdata6, x = 'Gender', y = 'Length.of.Stay',
          color = 'Gender', palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c('M', 'F'),
          ylab = 'Length of Stay', xlab = 'Gender')

ggline(LOSdata6, x = 'Gender', y = 'Length.of.Stay', 
       add = c("mean_se", "jitter"), 
       order = c('M', 'F'),
       ylab = 'Length of Stay', xlab = 'Gender')

res.aov <- aov(Length.of.Stay ~ Age.Group*Gender*Type.of.Admission*APR.Medical.Surgical.Description*Payment.Typology.1, data = LOSdata6)

summary(res.aov)

###Interpret the result of one-way ANOVA tests
###As the p-value is less than the significance level 0.05, 
###we can conclude that there are significant differences between 
###the groups highlighted with "*" in the model summary.

TukeyHSD(res.aov)

names(LOSdata6)

ggplot(LOSdata6,aes(x = Length.of.Stay)) + geom_histogram()
