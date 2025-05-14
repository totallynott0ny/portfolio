adults = read.csv('fil_adult.csv')
View(adults)
age = adults$age
eduyrs = adults$educational.num
gender = adults$gender_Male
hours = adults$hours.per.week
income = adults$income_.50K

#fitting model
LogModel = glm(income~age+eduyrs+gender+hours, family = "binomial")
summary(LogModel)

#log odds
adults$prob <- predict(LogModel, type = "response")  
adults$logit <- log(adults$prob / (1 - adults$prob))

# Age vs Logit
plot(age, adults$logit, 
     main = "Log Odds vs Age", 
     xlab = "Age", 
     ylab = "Log Odds", 
     pch = 20, col = "steelblue")

# Education Years vs Logit
plot(eduyrs, adults$logit, 
     main = "Log Odds vs Education Years", 
     xlab = "Education (Years)", 
     ylab = "Log Odds", 
     pch = 20, col = "forestgreen")

# Hours per Week vs Logit
plot(hours, adults$logit, 
     main = "Log Odds vs Hours Worked per Week", 
     xlab = "Hours/Week", 
     ylab = "Log Odds", 
     pch = 20, col = "darkorange")

#matrix 
predictors <- data.frame(
  age = adults$age,
  eduyrs = adults$educational.num,
  gender = adults$gender_Male,
  hours = adults$hours.per.week
)

# Compute the correlation matrix
cor(predictors)

#fitting with transf
LogModel = glm(income ~ log(age) + eduyrs + gender + hours, family = "binomial")
plot(sqrt(age), adults$logit, 
     main = "Log Odds vs Sqrt Age", 
     xlab = " Sqrt Age", 
     ylab = "Log Odds", 
     pch = 20, col = "red")
summary(LogModel)

#model w/o yrs edu
LogModel2 = glm(income~age+gender+hours, family = "binomial")
summary(LogModel2)
