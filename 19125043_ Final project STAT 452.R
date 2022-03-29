## data prep
setwd("C:/Users/DELL/Downloads/Data Project 2/Data 2")
data1<-read.csv("wage1.csv", header = FALSE)
names(data1) <- c("wage","educ","exper","tenure","nonwhite","female","married","numdep", "smsa","northcen","south","west","construc","ndurman","trcommpu","trade","services","profserv","profocc", "clerocc","servocc","lwage","expersq","tenursq")
finaldata<-data1[,c(1,2,3,5,6,7)]
finaldata$nonwhite<-factor(finaldata$nonwhite)
levels(finaldata$nonwhite) <- c("0 (White)", "1 (Nonwhite)")
finaldata$female<-factor(finaldata$female)
levels(finaldata$female) <- c("0 (Male)", "1 (Female)")
finaldata$married<-factor(finaldata$married)
levels(finaldata$married) <- c("0 (Single)", "1 (Married)")
##summary data
summary(finaldata)

attach(finaldata)
##Qual_desc
race <- table(nonwhite)
race
pie(race,main = "White to nonwhite worker")


gender <- table(female)
gender
pie(gender,main = "workers' gender")

marriage <- table(married)
marriage
pie(marriage,main = "worker's marriage status")

marriedgender <- table(married, female)
marriedgender
barplot(marriedgender, beside = TRUE)
##Quan_desc

salaryrace <- table(wage,nonwhite)
salaryrace
hist(salary ,main = "workers' wage")
barplot(salaryrace,main = "workers' wage by race", beside = TRUE,xlab= "sal")

hist(finaldata$wage, freq = FALSE,add=TRUE,)

hist(wage)
boxplot(wage)
plot(density(wage))

hist(exper)
boxplot(exper)
plot(density(exper))

hist(educ)
boxplot(educ)
plot(density(educ))

plot(wage~educ)
cor(wage,educ)

plot(wage~exper)
cor(wage, exper)

plot(exper~educ)
cor(exper,educ)
##avg test -- use p-val
boxplot(wage~married)
t.test(wage~married)
##reject everything

boxplot(educ~nonwhite)
t.test(educ~nonwhite, alternative = "great")
##accept at 99, 95

##p-value test 
pie(marriedgender,label=names(marriedgender))
prop.test(marriedgender,alternative="two.sided",correct = FALSE)

racegender <- table(nonwhite, female)
barplot(racegender,beside=TRUE)
prop.test(racegender,alternative="two.sided",correct = FALSE)

##regresssion-no1
model<-lm(wage~educ+exper)
summary(model)
confint(model, level = 0.95)
##goodness of fitness
type.wage = 1:length(wage)
 for (i in 1:length(wage)){
 if (wage[i] <= 3.5){
    type.wage[i] <- "1. Low";
    }
  else {
      if (wage[i] > 3.5 && wage[i] <= 10){
          type.wage[i] <- "2. Med";
      }
      else{
        type.wage[i] <- "3. High";
      }
    }
}
table(type.wage)
chisq.test(type.wage,p=c(1/3,1/2,1/6))
