df = read.csv("Filtered_Walkability.csv")
View(df)

# check assumptions

# fit linear model
Walkability = df$NatWalkInd
No_Auto = df$AutoOwn0
One_Auto = df$AutoOwn1
TwoorMoreAuto = df$AutoOwn2p
TotEmp = df$TotEmp

Amodel = lm(Walkability ~ No_Auto)
BModel = lm(Walkability ~ One_Auto)
Cmodel = lm(Walkability ~ TwoorMoreAuto)
DModel = lm(Walkability ~ TotEmp)

# Walkability vs Households with 0 cars
plot(No_Auto, Walkability, 
     main = "Walkability vs. Auto Ownership (0 Cars)",
     xlab = "Auto Ownership (0 Cars)", ylab = "Walkability Index",
     pch = 16, col = "blue")

abline(lm(Walkability ~ One_Auto, data = df), col = "red", lwd = 2)

# Walkability vs Households with 1 car
plot(One_Auto, Walkability, 
     main = "Walkability vs. Auto Ownership (1 Car)",
     xlab = "Auto Ownership (1 Car)", ylab = "Walkability Index",
     pch = 16, col = "blue")


abline(lm(Walkability ~ One_Auto, data = df), col = "red", lwd = 2)

# Walkability vs Households with 2 or more cars
plot(TwoorMoreAuto, Walkability, 
     main = "Walkability vs. Auto Ownership (2 or more Cars)",
     xlab = "Auto Ownership (2 or more Cars)", ylab = "Walkability Index",
     pch = 16, col = "blue")


abline(lm(Walkability ~ TwoorMoreAuto, data = df), col = "red", lwd = 2)

# Walkability vs Total Employment
plot(TotEmp, Walkability, 
     main = "Walkability vs. Total Employment",
     xlab = "Total Employment", ylab = "Total Employment",
     pch = 16, col = "blue")


abline(lm(Walkability ~ TotEmp, data = df), col = "red", lwd = 2)

#multilinear model
Lmodel = lm(Walkability ~ No_Auto + One_Auto + TwoorMoreAuto + TotEmp)
Lmodel$coefficients

summary(Lmodel)

#checking normality
hist(Lmodel$residuals)
qqnorm(Lmodel$residuals)

#checking constant variance
plot(Lmodel$fitted.values, Lmodel$residuals, 
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 16, col = "blue")

abline(h = 0, col = "red", lwd = 2)


#checking correlation between Walkability and households w 2 or more cars
cor(df$AutoOwn2p, df$NatWalkInd)




# CI for One_Auto coefficient 
beta_hat <- 0.006488  # Coefficient for One_Auto
SE_beta <- 0.001064  # Standard error for One_Auto
df <- 933  # Degrees of freedom
confidence_level <- 0.95

# Critical t-value for 95% confidence interval
t_critical <- qt((1 + confidence_level) / 2, df)

# Compute confidence interval
lower_bound <- beta_hat - t_critical * SE_beta
upper_bound <- beta_hat + t_critical * SE_beta

# Print confidence interval
cat("95% Confidence Interval for One_Auto: (", lower_bound, ",", upper_bound, ")\n")

