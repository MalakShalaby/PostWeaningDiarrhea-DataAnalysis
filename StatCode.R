myData=PWD

#Summarizing my data
summary(myData)
str(myData)

#Descriptive statistics
#Calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable). 


#Pen
mean(myData$Pen,na.rm=TRUE) # remove the NA values.
median(myData$Pen,na.rm=TRUE)
min(myData$Pen,na.rm=TRUE)
max(myData$Pen,na.rm=TRUE)
quantile(myData$Pen,na.rm=TRUE,c(0.25,0.75))

#Feeder
mean(myData$Feeder,na.rm=TRUE)
median(myData$Feeder,na.rm=TRUE)
min(myData$Feeder,na.rm=TRUE)
max(myData$Feeder,na.rm=TRUE)
quantile(myData$Feeder,na.rm=TRUE,c(0.25,0.75))

#W0
mean(myData$W0,na.rm=TRUE)
median(myData$W0,na.rm=TRUE)
min(myData$W0,na.rm=TRUE)
max(myData$W0,na.rm=TRUE)
quantile(myData$W0,na.rm=TRUE,c(0.25,0.75))

#P0
mean(myData$P0,na.rm=TRUE)
median(myData$P0,na.rm=TRUE)
min(myData$P0,na.rm=TRUE)
max(myData$P0,na.rm=TRUE)
quantile(myData$P0,na.rm=TRUE,c(0.25,0.75))

#ADWG0021
mean(myData$ADWG0021,na.rm=TRUE)
median(myData$ADWG0021,na.rm=TRUE)
min(myData$ADWG0021,na.rm=TRUE)
max(myData$ADWG0021,na.rm=TRUE)
quantile(myData$ADWG0021,na.rm=TRUE,c(0.25,0.75))

#ADWG2150
mean(myData$ADWG2150,na.rm=TRUE)
median(myData$ADWG2150,na.rm=TRUE)
min(myData$ADWG2150,na.rm=TRUE)
max(myData$ADWG2150,na.rm=TRUE)
quantile(myData$ADWG2150,na.rm=TRUE,c(0.25,0.75))

#ADWG0050
mean(myData$ADWG0050,na.rm=TRUE)
median(myData$ADWG0050,na.rm=TRUE)
min(myData$ADWG0050,na.rm=TRUE)
max(myData$ADWG0050,na.rm=TRUE)
quantile(myData$ADWG0050,na.rm=TRUE,c(0.25,0.75))

#For the categorical variable existing, calculate a frequency table 

table(myData$Sex)
table(myData$Treatment)

#Calculate the correlation coefficient (ADWG0021 and ADWG2150) and (ADWG0021and ADWG0050)

#Correlation coefficient (ADWG0021 and ADWG2150)

#pearson
corrp1=cor(myData$ADWG0021,myData$ADWG2150 ,use="complete.obs",method="pearson")
corrp1
#0.2270356

#Correlation coefficient (ADWG0021and ADWG0050)

#pearson
corrp2=cor(myData$ADWG0021 , myData$ADWG0050 ,use="complete.obs", method = 'pearson') 
corrp2
#0.4427165
#Positive relationship



#Graphics

#1-bar chart of a categorical variable for the gender (Sex parameter)
myData$Sex = factor(myData$Sex, levels=c(1,2), labels = c("Female","Male"))

barplot(table(myData$Sex), xlab="Gender",ylab="Frequency", col=c("hotpink","blue"))



#2-Generate a bar chart graph with mean ADWG0021in  males and females 

barplot(tapply(myData$ADWG0021, myData$Sex, mean, na.rm = TRUE),
        xlab = "Sex", ylab = "Mean ADWG0021", col = c("hotpink", "blue"))



#3-Make a histogram of a continuous variable: “ADWG2150” as well as “ADWG0021”.
hist(myData$ADWG2150,col="yellow", xlab="ADWG2150", main="Distribution ofADWG2150")
hist(myData$ADWG0021,col="skyblue", xlab="ADWG0021", main="Distribution of ADWG0021")

#4-scatterplot of 2 continuous variables ADWG0050 and ADWG0021, and add the regression lines for each gender
#making data frame for each gender

male_data <- myData[myData$Sex == "Male",]
female_data <- myData[myData$Sex == "Female", ]

#creating scatter plots for males and females
# Scatterplot for Males
plot(male_data$ADWG0050, male_data$ADWG0021, xlab = "ADWG0050", ylab = "ADWG0021",
     main = "Scatterplot of ADWG0050 vs. ADWG0021", pch = 16, col = "red")
abline(lm(male_data$ADWG0021 ~ male_data$ADWG0050), col = "blue")

# Scatterplot for Females
points(female_data$ADWG0050, female_data$ADWG0021, pch = 2, col = "black")
abline(lm(female_data$ADWG0021 ~ female_data$ADWG0050), col = "hotpink")

# Add legend
legend("topright", c("Males", "Females"), cex = 0.8, col = c("red", "black"), pch = c(16, 2))


#5-Make a boxplot of ADWG0021 and a separate boxplots per Treatment (as.factors).
# Create boxplot for ADWG0021

boxplot(myData$ADWG0021,main="ADWG0021 Boxplot",xlab="ADWG0021",col="limegreen")

# Create separate boxplots for each treatment
boxplot(myData$ADWG0021 ~ as.factor(myData$Treatment), xlab = "Treatment", ylab = "ADWG0021",
        main = "Boxplot of ADWG0021 by Treatment",col=c("lightslateblue","chartreuse","brown1","mediumspringgreen","purple"))

# Outlier detection
# Outlier detection for each variable
outliers <- boxplot.stats(myData$ADWG0021)$out
outliers <- boxplot.stats(myData$ADWG2150)$out
outliers <- boxplot.stats(myData$ADWG0050)$out

# Print the identified outliers
print(outliers)


box0021 <- boxplot(ADWG0021~Treatment,data=myData, plot = FALSE)
box0021$out
box2150 <- boxplot(ADWG2150~Treatment,data=myData, plot = FALSE)
box2150$out
box0050 <- boxplot(ADWG0050~Treatment,data=myData, plot = FALSE)
box0050$out
boxplot(myData$ADWG2150 ~ as.factor(myData$Treatment), xlab = "Treatment", ylab = "ADWG0021", main = "Boxplot of ADWG0021 by Treatment",col=c("coral1","darkorchid1","mediumspringgreen","violetred4","slateblue2"))
#their are two outliers in ADWG0021 per Treatment(A) with values 178.5714 and 110.1190 located outside the whiskers of the box plot 
#their is an outlier in ADWG2150 and ADWG0050 per Treatment(B) with values 375 and 275.625 located outside the whiskers of the box plot 

#check normality for ADWG0021,ADWG0050,ADWG2150 using qqplot
qqnorm(myData$ADWG0021, main="ADWG0021 QQ Plot")
qqline(myData$ADWG0021)
qqnorm(myData$ADWG0050, main="ADWG0050 QQ Plot")
qqline(myData$ADWG0050)
qqnorm(myData$ADWG2150, main="ADWG2150 QQ Plot")
qqline(myData$ADWG2150)

#check normality for ADWG0021,ADWG0050,ADWG2150 using shapiro test
shapiro.test(myData$ADWG0021)
shapiro.test(myData$ADWG0050)
shapiro.test(myData$ADWG2150)
#p-value for DWG0021,ADWG0050,ADWG2150 are greater than the signficance level of 0.05

#check the homoscedasticity using levene's test 

leveneTest(myData$ADWG0021, myData$Treatment)
leveneTest(myData$ADWG0050, myData$Treatment)
leveneTest(myData$ADWG2150, myData$Treatment)
#check the homoscedasticity using Bartlett's test 
bartlett.test(myData$ADWG0021, myData$Treatment)
bartlett.test(myData$ADWG0050, myData$Treatment)
bartlett.test(myData$ADWG2150, myData$Treatment)

#according to bartlett.test ADWG0021,ADWG0050,ADWG2150 aganist treatment
#all the values are greater than the significance level of 0.05
#This means that we do not have sufficient evidence to reject null hypothesis


#5
table(myData$Sex)
tapply(myData$ADWG0021,list(sex=myData$Sex),mean,na.rm=T)



# Calculate confidence intervals for means of ADWG0021 by Sex
library(dplyr)

result <- myData %>%
  group_by(Sex) %>%
  summarize(
    n = n(),
    mean = mean(ADWG0021),
    sd = sd(ADWG0021),
    sem = sd / sqrt(n),
    ci90_lower = mean - qt(0.95, n - 1) * sem,
    ci90_upper = mean + qt(0.95, n - 1) * sem,
    ci95_lower = mean - qt(0.975, n - 1) * sem,
    ci95_upper = mean + qt(0.975, n - 1) * sem,
    ci99_lower = mean - qt(0.995, n - 1) * sem,
    ci99_upper = mean + qt(0.995, n - 1) * sem,
    interval_width90 = ci90_upper - ci90_lower,
    interval_width95 = ci95_upper - ci95_lower,
    interval_width99 = ci99_upper - ci99_lower
    
  )

#we can describe the inferences by stating that we are 90%, 95%, and 99% confident that the true mean ADWG0021 for males falls within the calculated confidence intervals. 
#As the confidence level increases, the width of the confidence interval also increases, indicating increased uncertainty but higher confidence in capturing the true mean.


#6.	Hypothesis testing

#Null Hypothesis (H0): There is no difference in ADWG0021 between males and females.
#Alternative Hypothesis (H1): There is a difference in ADWG0021 between males and females.
#since our data were noramlly disturbuted,we used 
#the t-test which is  an appropriate and widely used method to compare means between two groups.
# T-test to compare ADWG0021 between males and females
t_test_result=t.test(ADWG0021 ~ Sex, data = myData)
print(t_test_result)
#the p-value = 0.7557 which is greater than alpha of value 0.05,which means that we don't
#have enough evidence to reject the null Hypothesis that 
#There is no difference in ADWG0021 between males and females

hist(myData$ADWG0021[myData$Sex == "Male"], col = "blue", main = "Histogram of ADWG0021 for Males")
hist(myData$ADWG0021[myData$Sex == "Female"], col = "pink", main = "Histogram of ADWG0021 for Females")

qqnorm(myData$ADWG0021[myData$Sex == "Male"], main = "QQ Plot of ADWG0021 for Males")
qqline(myData$ADWG0021[myData$Sex == "Male"])
qqnorm(myData$ADWG0021[myData$Sex == "Female"], main = "QQ Plot of ADWG0021 for Females")
qqline(myData$ADWG0021[myData$Sex == "Female"])

shapiro.test(myData$ADWG0021[myData$Sex == "Male"])
shapiro.test(myData$ADWG0021[myData$Sex == "Female"])


library(car)
leveneTest(ADWG0021 ~ Sex, data = myData)
# F-test for equality of variances
var.test(ADWG0021 ~ Sex, data = myData)

################################################
#We hypothesis that ADWG0021is “different” in the group receiving 
#Treatment A (normalfeed + ZnO) compared to the Treatment B (normal feed + nutraceuticals).
#Can you test thishypothesis assuming heteroscedasticity.
table(PWD$Treatment)
A=(PWD$ADWG0021[PWD$Treatment=="A"])
B=(PWD$ADWG0021[PWD$Treatment=="B"])
#Paired t-test:
  t.test(A, B, paired= T, var.equal = F)
  # mean difference 24.59077
  #mean difference is not equal to 0
  
#We use Bartlett's test of homoscedasticity:
bartlett.test(list(A[A == A], B[B == B]))
# p-value = 0.955
# our data is not heteroscedastic (they have the same variance).

###############################################
# We hypothesis that ADWG0021is different between the different Treatments . Can you perform comparison between the different groups, after assessing the assumptions and performing post-hoc testing (assuming normality and homoscedasticity).

#Check the normality using (Histogram, QQ plot, Shapiro test)

#Histogram
#Treatment A
hist(PWD[PWD$Treatment == "A",]$ADWG0021, main='Treatment A')
#Treatment B
hist(PWD[PWD$Treatment == "B",]$ADWG0021, main='Treatment B')
#Treatment C
hist(PWD[PWD$Treatment == "C",]$ADWG0021, main='Treatment C')
#Treatment D
hist(PWD[PWD$Treatment == "D",]$ADWG0021, main='Treatment D')
#Treatment E
hist(PWD[PWD$Treatment == "E",]$ADWG0021, main='Treatment E')

#QQ plot
qqnorm(myData[myData$Treatment == "A",]$ADWG0021, main='Treatment A')
qqline(myData[myData$Treatment == "A",]$ADWG0021)

qqnorm(myData[myData$Treatment == "B",]$ADWG0021, main='Treatment B')
qqline(myData[myData$Treatment == "B",]$ADWG0021)

qqnorm(myData[myData$Treatment == "C",]$ADWG0021, main='Treatment C')
qqline(myData[myData$Treatment == "C",]$ADWG0021)

qqnorm(myData[myData$Treatment == "D",]$ADWG0021, main='Treatment D')
qqline(myData[myData$Treatment == "D",]$ADWG0021)

qqnorm(myData[myData$Treatment == "E",]$ADWG0021, main='Treatment E')
qqline(myData[myData$Treatment == "E",]$ADWG0021)

#Shapiro test
shapiro.test(PWD[PWD$Treatment == "A",]$ADWG0021) #p-value = 0.0395 
shapiro.test(PWD[PWD$Treatment == "B",]$ADWG0021) #p-value = 0.8312
shapiro.test(PWD[PWD$Treatment == "C",]$ADWG0021) #p-value = 0.6954 
shapiro.test(PWD[PWD$Treatment == "D",]$ADWG0021) #p-value = 0.7126 
shapiro.test(PWD[PWD$Treatment == "E",]$ADWG0021) #p-value = 0.86 

#We will use Kruskal test because Treatment A is not normal
kruskal.test(ADWG0021 ~Treatment , data=PWD) #p-value = 0.09972

#Check the homoscedasticity by using (Boxplot and levene test)
#Boxplot
boxplot(ADWG0021~Treatment, data=myData)
#levene
leveneTest(ADWG0021~Treatment, data=myData)#F value=0.2676, Pr(>F)=0.8968 #Equal variance


#Anova test
AnovaModel=aov(ADWG0021~Treatment, data= myData)
AnovaModel
summary(AnovaModel)
library(dunn.test)
library(report)
report(AnovaModel)
#The main effect of Treatment is statistically not significant and large (F(4, 35) = 2.10, p= 0.101; Eta2 = 0.19, 95% CI [0.00, 1.00])
coef(AnovaModel)

#posthoc using tukey test
tukey=TukeyHSD(AnovaModel)
tukey
plot(tukey)
pairwise.t.test(myData$ADWG0021,myData$Treatment, p.adjust.method = "bonferroni")
dunn.test(myData$ADWG0021, myData$Treatment, method = "bonferroni")



#7.linear model

#Fitting a linear regression model
#Continuous with categorical
PWD$Sex=as.integer(PWD$Sex)
plot(PWD$Sex, PWD$ADWG0021)
regression=lm(ADWG0021 ~ Sex , data= PWD)
regression #intercept = 145.99 and slope = -1.92
summary(regression)
abline(regression, col="blue")


# Interpretation of regression coefficient
coef_lm <- coef(regression)
gender_coefficient <- coef_lm[2]



# 95% confidence interval for the regression slope
conf_interval <- confint(regression, level = 0.95)

# Estimating the average ADWG0021 change with changing gender from 1 to 2
gender_change <- 2 - 1
average_change <- gender_change * gender_coefficient
cat("Estimating the Average ADWG0021 Change for Changing Gender from 1 to 2:", round(average_change, 2), "\n")






