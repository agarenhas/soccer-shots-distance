################################################################################
################## SOCCER SHOT DISTANCE - GLM MODELLING ########################
################################################################################


## The database we will work with contains records of shots taken in different 
## soccer matches from various competitions:
##
## ==========================================================
## AVAILABLE COMPETITIONS AND SEASONS
## ==========================================================
##
## Champions League:
##    2014/15, 2015/16, 2016/17, 2017/18, 2018/19
##
## Copa América:
##    2024
##
## Copa del Rey:
##    1977/78, 1982/83, 1983/84
##
## U-20 World Cup:
##    1979
##
## World Cup:
##    2018
##
## La Liga:
##    2016/17, 2017/18, 2018/19, 2019/20, 2020/21
##
## Liga Profesional:
##    1980/81, 1997/98
##
## Ligue 1:
##    2021/22, 2022/23
##
## Major League Soccer (MLS):
##    2023
##
## North American League:
##    1977
##
## NWSL (US Women's League):
##    2018
##
## Europa League:
##    1988/89
##
## UEFA Women's EURO:
##    2022, 2025
##
## Women's World Cup:
##    2019, 2023
## 
## The objective of this work is to fit the number of shots from outside the box 
## (more than 15 meters from the goal) recorded as a function of distance (and 
## the square of the distance) to a Poisson or similar model that models a count variable


################################################################################
################################################################################

###################### DATA LOADING AND STRUCTURING ############################


## Setting the working directory where the data is located

# Checking that the directory has been set correctly
getwd()


## Saving the data frame from the CSV file into an R object

df <- read.csv("all_competitions_combined.csv")
# Summary of the data in the data frame
summary(df)
# Convert each column of the data frame into a variable
attach(df)
# The variable we will use for the study is "distance"


## Count of the number of shots once distances are discretized

# Discretize distance values meter by meter
dist <- floor(distance)
# Count the shots that occurred at each distance
data <- as.data.frame(table(dist))
# Change column names for convenience
colnames(data) <- c("x","y")
# Save the x and y values from the data frame
attach(data)
# Convert values to numeric
x <- as.numeric(x)
y <- as.numeric(y)

## Graphical representation of the scatter plot

plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)


################################################################################
################################################################################

############################# POISSON FIT Y~X ##################################


## We perform the fit to a Poisson model with log link function taking x as the 
## only explanatory variable
## We fit lambda(x,beta)=e^(beta0+beta1*x)

poisson_1 <- glm(y~x, family=poisson(link="log"))
summary(poisson_1)
# Initially we can see that both beta0 and beta1 are statistically
# significant at any usual significance level

## Save the estimated coefficient values

beta_p1 <- coef(poisson_1)
# Since beta_1 is less than 0, the estimate of lambda decreases with x
# Also save the exponentials of the coefficients
ebeta_p1 <- exp(beta_p1)
# e^(beta_0) is the estimate of the number of shots for x=0, i.e., right at 
# the goal. We can already see in the graph how this estimate doesn't fit
# the experimental data very well.
# e^(beta_1) is the ratio between lambda at x+1 and lambda at x. In this case,
# lambda is reduced to 0.95*lambda for each meter further away (according to the estimate)

## Confidence intervals for the estimators

# We can obtain the standard deviation of each estimator from the summary
summary(poisson_1)
sd_beta <- c(0.012619, 0.000508)
# Define a vector with the two parameters we are estimating
param <- c("beta0", "beta1")
# Define another vector with the confidence levels for the intervals
niveles <- c(0.95, 0.99)
# Also define another vector with the types of intervals to calculate
tipo <- c("PL", "As.")
# Create an empty data frame to store the interval values
intervalos <- data.frame(
  param = character(),
  tipo = character(),
  nivel_confianza = numeric(),
  inferior = numeric(),
  superior = numeric(),
  exp_inferior = numeric(),
  exp_superior = numeric(),
  stringsAsFactors = FALSE
)
# Calculate the interval endpoints using a loop
# For each parameter
for (i in seq_along(param)) {
  # For each level
  for (nivel in niveles) {
    # Calculate the PL interval using the "confint" command
    ic <- confint(poisson_1, level=nivel)[i,]
    # Also calculate the interval for the exponential of the parameter
    eic <- exp(ic)
    # Save the values in the data frame
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[1],
        nivel_confianza = nivel,
        inferior = ic[1],
        superior = ic[2],
        exp_inferior = eic[1],
        exp_superior = eic[2]
      )
    )
    # Calculate the asymptotic interval using the standard deviation
    alpha <- 1 - nivel
    # By symmetry, we just need the 1-alpha/2 quantile
    z <- qnorm(1 - alpha / 2, mean = beta_p1[i], sd = sd_beta[i])
    # Endpoints of the parameter interval
    ic_inf <- beta_p1[i] - z*sd_beta[i]
    ic_sup <- beta_p1[i] + z*sd_beta[i]
    # Endpoints of the interval for the exponential of the parameter
    e_ic_inf <- exp(ic_inf)
    e_ic_sup <- exp(ic_sup)
    # Save the values in the data frame
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[2],
        nivel_confianza = nivel,
        inferior = ic_inf,
        superior = ic_sup,
        exp_inferior = e_ic_inf,
        exp_superior = e_ic_sup
      )
    )
  }
}
# Remove row names, which we don't need
rownames(intervalos) <- NULL
# The obtained intervals are as follows
print(intervalos)
# The exp_inferior values may appear in exp_superior and vice versa


## Graphical comparison of the intervals

# Create a graphical representation of the 4 intervals for each parameter
# Save the values for beta0 in a data frame
ic_0_p1 <- data.frame(
  metodo = paste0(intervalos$tipo[intervalos$param=="beta0"]," ",
                  intervalos$nivel_confianza[intervalos$param=="beta0"]),
  inf = intervalos$inferior[intervalos$param=="beta0"],
  sup = intervalos$superior[intervalos$param=="beta0"],
  einf = intervalos$exp_inferior[intervalos$param=="beta0"],
  esup = intervalos$exp_superior[intervalos$param=="beta0"]
)
# Create the object for the graph to represent the intervals
par(mfrow=c(1,1))
plot(NULL,
     xlim = range(ic_0_p1$inf, ic_0_p1$sup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[0]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[0])
)
# Represent them graphically with horizontal lines
for (i in 1:nrow(ic_0_p1)) {
  segments(ic_0_p1$inf[i], i, ic_0_p1$sup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_p1$metodo[i]), "blue", "darkgreen"))
  points(c(ic_0_p1$inf[i], ic_0_p1$sup[i]), rep(i, 2), pch = 16)
}
# Labels on the Y-axis
axis(2, at = 1:4, labels = ic_0_p1$metodo, las = 1)
# Vertical line indicating the point estimator
abline(v = beta_p1[1], lty = 2, col = "grey30")
# A new graphical object to represent the intervals (of the exponential)
plot(NULL,
     xlim = range(ic_0_p1$einf, ic_0_p1$esup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[0])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[0]))
)
# Graphical representation
for (i in 1:nrow(ic_0_p1)) {
  segments(ic_0_p1$einf[i], i, ic_0_p1$esup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_p1$metodo[i]), "blue", "darkgreen"))
  points(c(ic_0_p1$einf[i], ic_0_p1$esup[i]), rep(i, 2), pch = 16)
}
# Labels 
axis(2, at = 1:4, labels = ic_0_p1$metodo, las = 1)
# Vertical line indicating the point estimator
abline(v = ebeta_p1[1], lty = 2, col = "grey30")
# This can also be done for beta1 and for the exponentials of the 
# parameters by changing some values in the previous lines
ic_1_p1 <- data.frame(
  metodo = paste0(intervalos$tipo[intervalos$param=="beta1"]," ",
                  intervalos$nivel_confianza[intervalos$param=="beta1"]),
  inf = intervalos$inferior[intervalos$param=="beta1"],
  sup = intervalos$superior[intervalos$param=="beta1"],
  einf = intervalos$exp_inferior[intervalos$param=="beta1"],
  esup = intervalos$exp_superior[intervalos$param=="beta1"]
)
plot(NULL,
     xlim = range(ic_1_p1$inf, ic_1_p1$sup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[1]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[1])
)
for (i in 1:nrow(ic_1_p1)) {
  segments(ic_1_p1$inf[i], i, ic_1_p1$sup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_p1$metodo[i]), "blue", "darkgreen"))
  points(c(ic_1_p1$inf[i], ic_1_p1$sup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_p1$metodo, las = 1)
abline(v = beta_p1[2], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_1_p1$einf, ic_1_p1$esup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[1])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[1]))
)
for (i in 1:nrow(ic_1_p1)) {
  segments(ic_1_p1$einf[i], i, ic_1_p1$esup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_p1$metodo[i]), "blue", "darkgreen"))
  points(c(ic_1_p1$einf[i], ic_1_p1$esup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_p1$metodo, las = 1)
abline(v = ebeta_p1[2], lty = 2, col = "grey30")


## Graphical representation of the fit

# Represent the scatter plot again
plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)
# Create a sequence of distances for prediction
x_seq <- seq(min(x), max(x), length.out = 200)
# Create a sequence with the model predictions for the previous sequence
y_pred <- predict(poisson_1, newdata = data.frame(x = x_seq), type = "response")
# Add the curve with the fit
lines(x_seq, y_pred, col = "magenta", lwd = 2)


## Calculation of residuals

# Raw residuals
res.b_p1 <- residuals(poisson_1, type="response")
# Pearson residuals
res.p_p1 <- residuals(poisson_1, type="pearson")
# Deviance residuals
res.d_p1 <- residuals(poisson_1, type="deviance")


## Representation of residuals

# The simplest way is with the plot function
# Generate 4 spaces for plots in the window
par(mfrow=c(2,2))
# Add the plots to study the residuals
plot(poisson_1)
# We see that there is some relationship between the predictions and the residuals, 
# so it seems that the log-linearity assumption is not met
# Make other manual graphical representations of the residuals
# Save the predictions of exp(x'beta)
pred_p1 <- predict(poisson_1, type="response")
# Also the predictions of x'beta
pred.l_p1 <- predict(poisson_1, type="link") 
# Generate again 4 spaces for the plots
par(mfrow=c(2,2))
# Raw residuals vs. predictions
plot(pred_p1,res.b_p1) ; abline(0,0,col="gray", lwd=2)
# Here heteroscedasticity is expected, but not that the residuals follow a 
# trend like the one observed
# Pearson residuals vs. predictions
plot(pred_p1,res.p_p1) ; abline(0,0,col="gray", lwd=2)
# Here homoscedasticity is expected and, again, that the residuals don't follow a 
# trend. Neither condition is met
# Deviance residuals vs. predictions
plot(pred_p1,res.d_p1) ; abline(0,0,col="gray", lwd=2)
# Again, no trend is expected
# Deviance residuals vs. linear predictions
plot(pred.l_p1,res.d_p1) ; abline(0,0,col="gray", lwd=2)
# We continue to detect the trend in the data, which leads us to think that 
# the log-linear model was not correctly fitted (despite the 
# significance of its coefficients)


## Comparison with model without dependence on the explanatory variable

# We can compare the fitted model with the same model without considering
# the dependence on the explanatory variable, i.e., lambda(beta)=e^(beta_0)
# Actually, since we already saw the statistical significance of beta1, this wouldn't be
# necessary. Still, we add it to the study for completeness.
# Create the model without dependence on the explanatory variable
poisson_0 <- glm(y~1, family=poisson(link=log))
summary(poisson_0)
# Logically, we get a different estimate for beta0
# Perform the following hypothesis test
# H0 = poisson_0
# Ha = poisson_1
anova(poisson_0, poisson_1)
# We reject the null hypothesis at any usual significance level, so
# we keep the poisson_1 model, i.e., with the dependence on
# the variable x


################################################################################
################################################################################

######################### POISSON FIT Y~X+X^2 ##################################


## Given that the previous model didn't fit the data very well, 
## we add a new variable to the model to increase its complexity. In this case,
## the variable will be the square of the distance, i.e., x^2. Thus,
## what we fit is: lambda(x,beta)=e^(beta0+beta1*x+beta2*x^2)

poisson_2 <- glm(y~x+I(x^2), family=poisson(link="log"))
summary(poisson_2)
# We see that coefficients beta0 and beta1 are still statistically 
# significant. Additionally, beta2 is also significant, which makes us think that this
# model will fit the data better than the previous one

## Coefficient estimates

beta_p2 <- coef(poisson_2)
# Since beta2 is less than 0, the effect added to lambda is similar to
# adding a Gaussian bell curve, since the parameter is in the exponential
ebeta_p2 <- exp(beta_p2)
# We see how exp(beta0), the estimated value for lambda at x=0, goes from 766 to 
# 83, which fits the model data somewhat better. This is thanks to the
# effect of adding the beta0 parameter


## Confidence intervals for the estimators

# Procedure analogous to that performed in the previous model
summary(poisson_2)
sd_beta <- c(0.03468, 0.003631, 0.00008836)
param <- c("beta0", "beta1", "beta2")
niveles <- c(0.95, 0.99)
tipo <- c("PL", "As.")
intervalos <- data.frame(
  param = character(),
  tipo = character(),
  nivel_confianza = numeric(),
  inferior = numeric(),
  superior = numeric(),
  exp_inferior = numeric(),
  exp_superior = numeric(),
  stringsAsFactors = FALSE
)
for (i in seq_along(param)) {
  for (nivel in niveles) {
    ic <- confint(poisson_2, level=nivel)[i,]
    eic <- exp(ic)
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[1],
        nivel_confianza = nivel,
        inferior = ic[1],
        superior = ic[2],
        exp_inferior = eic[1],
        exp_superior = eic[2]
      )
    )
    alpha <- 1 - nivel
    z <- qnorm(1 - alpha / 2, mean = beta_p2[i], sd = sd_beta[i])
    ic_inf <- beta_p2[i] - z*sd_beta[i]
    ic_sup <- beta_p2[i] + z*sd_beta[i]
    e_ic_inf <- exp(ic_inf)
    e_ic_sup <- exp(ic_sup)
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[2],
        nivel_confianza = nivel,
        inferior = ic_inf,
        superior = ic_sup,
        exp_inferior = e_ic_inf,
        exp_superior = e_ic_sup
      )
    )
  }
}
# Remove row names, which we don't need
rownames(intervalos) <- NULL
# The obtained intervals are as follows
print(intervalos)


## Graphical comparison of the intervals

ic_0_p2 <- data.frame(
  metodo = paste0(intervalos$tipo[intervalos$param=="beta0"]," ",
                  intervalos$nivel_confianza[intervalos$param=="beta0"]),
  inf = intervalos$inferior[intervalos$param=="beta0"],
  sup = intervalos$superior[intervalos$param=="beta0"],
  einf = intervalos$exp_inferior[intervalos$param=="beta0"],
  esup = intervalos$exp_superior[intervalos$param=="beta0"]
)
par(mfrow=c(1,1))
plot(NULL,
     xlim = range(ic_0_p2$inf, ic_0_p2$sup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[0]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[0])
)
for (i in 1:nrow(ic_0_p2)) {
  segments(ic_0_p2$inf[i], i, ic_0_p2$sup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_0_p2$inf[i], ic_0_p2$sup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_p2$metodo, las = 1)
abline(v = beta_p2[1], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_0_p2$einf, ic_0_p2$esup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[0])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[0]))
)
for (i in 1:nrow(ic_0_p2)) {
  segments(ic_0_p2$einf[i], i, ic_0_p2$esup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_0_p2$einf[i], ic_0_p2$esup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_p2$metodo, las = 1)
abline(v = ebeta_p2[1], lty = 2, col = "grey30")
ic_1_p2 <- data.frame(
  metodo = paste0(intervalos$tipo[intervalos$param=="beta1"]," ",
                  intervalos$nivel_confianza[intervalos$param=="beta1"]),
  inf = intervalos$inferior[intervalos$param=="beta1"],
  sup = intervalos$superior[intervalos$param=="beta1"],
  einf = intervalos$exp_inferior[intervalos$param=="beta1"],
  esup = intervalos$exp_superior[intervalos$param=="beta1"]
)
plot(NULL,
     xlim = range(ic_1_p2$inf, ic_1_p2$sup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[1]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[1])
)
for (i in 1:nrow(ic_1_p2)) {
  segments(ic_1_p2$inf[i], i, ic_1_p2$sup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_1_p2$inf[i], ic_1_p2$sup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_p2$metodo, las = 1)
abline(v = beta_p2[2], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_1_p2$einf, ic_1_p2$esup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[1])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[1]))
)
for (i in 1:nrow(ic_1_p2)) {
  segments(ic_1_p2$einf[i], i, ic_1_p2$esup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_1_p2$einf[i], ic_1_p2$esup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_p2$metodo, las = 1)
abline(v = ebeta_p2[2], lty = 2, col = "grey30")
ic_2_p2 <- data.frame(
  metodo = paste0(intervalos$tipo[intervalos$param=="beta2"]," ",
                  intervalos$nivel_confianza[intervalos$param=="beta2"]),
  inf = intervalos$inferior[intervalos$param=="beta2"],
  sup = intervalos$superior[intervalos$param=="beta2"],
  einf = intervalos$exp_inferior[intervalos$param=="beta2"],
  esup = intervalos$exp_superior[intervalos$param=="beta2"]
)
plot(NULL,
     xlim = range(ic_2_p2$inf, ic_2_p2$sup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[2]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[2])
)
for (i in 1:nrow(ic_2_p2)) {
  segments(ic_2_p2$inf[i], i, ic_2_p2$sup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_2_p2$inf[i], ic_2_p2$sup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_p2$metodo, las = 1)
abline(v = beta_p2[3], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_2_p2$einf, ic_2_p2$esup),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[2])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[2]))
)
for (i in 1:nrow(ic_2_p2)) {
  segments(ic_2_p2$einf[i], i, ic_2_p2$esup[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_p2$metodo[i]), "blue", "darkgreen"))
  points(c(ic_2_p2$einf[i], ic_2_p2$esup[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_p2$metodo, las = 1)
abline(v = ebeta_p2[3], lty = 2, col = "grey30")


## Graphical representation of the fit

plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)
x_seq <- seq(min(x), max(x), length.out = 200)
y_pred <- predict(poisson_2, newdata = data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "red", lwd = 2)
# We see that this model has a trend more similar to that of the 
# sample data compared to the model dependent only on x. This is
# due to the beta2 parameter which, as mentioned before, adds a trend
# similar to a Gaussian because it's negative. Therefore, the model 
# can capture the initial trend of the data to have small y values
# for small x values, reach a maximum and then decrease again
# Still, as we will see with the residuals, it doesn't completely capture the trend
# of the data. With the residuals it's easy to see as they capture a trend of
# these vs. the predictions
# We can also compare it graphically with the previous model
y_pred <- predict(poisson_1, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "magenta", lwd = 2)


## Calculation of residuals

res.b_p2 <- residuals(poisson_2, type="response")
res.p_p2 <- residuals(poisson_2, type="pearson")
res.d_p2 <- residuals(poisson_2, type="deviance")


## Representation of residuals

par(mfrow=c(2,2))
plot(poisson_2)
# There is again a clear trend of the residuals as a function of the values
# predicted by the model (seems exponential), which makes us think that the 
# model with log link might be wrong. By the shape of the residuals it seems
# that the model predicts better for high values of x, and it fails more near 0
pred_p2 <- predict(poisson_2, type="response")
pred.l_p2 <- predict(poisson_2, type="link") 
par(mfrow=c(2,2))
plot(pred_p2,res.b_p2) ; abline(0,0,col="gray", lwd=2)
# We see heteroscedasticity but there's still a slight trend
plot(pred_p2,res.p_p2) ; abline(0,0,col="gray", lwd=2)
# The highest residuals are those obtained for low values of e(x'beta)
plot(pred_p2,res.d_p2) ; abline(0,0,col="gray", lwd=2)
plot(pred.l_p2,res.d_p2) ; abline(0,0,col="gray", lwd=2)
# A large number of values accumulate at x'beta with very disparate residual
# quantities. This shouldn't happen if the model fitted the data trend well. 
# Additionally, most residuals are greater than 0, which reflects that there is a certain trend in them


## Comparison with previous Poisson model

# We can compare the fitted model with the same model without considering
# the dependence on the variable x^2, i.e., lambda(beta)=e^(beta_0+beta_1*x)
# Actually, since we already saw the statistical significance of beta2, this wouldn't be
# necessary. Still, we add it to the study for completeness.
# Perform the following hypothesis test
# H0 = poisson_1
# Ha = poisson_2
anova(poisson_1, poisson_2)
# We reject the null hypothesis at any usual significance level, so
# we keep the poisson_2 model, i.e., with the dependence on
# the variable x and the variable x^2


################################################################################
################################################################################

############################# NB FIT Y~X+X^2 ###################################


## We can change the hypothesis that lambda is the mean of a Poisson 
## distribution. We try fitting to a Negative Binomial distribution (which 
## would also correct possible overdispersion). Additionally, by changing to a 
## Negative Binomial and not to an ad-hoc estimation of overdispersion 
## we preserve the property of the estimators being maximum likelihood,
## so we still obtain an AIC value for the model and the asymptotic
## normal tendency is still verified

# Required library for the fit
library(MASS)
bn <- glm.nb(y ~ x + I(x^2), init.theta=0.5, link=log)
# An initial value for the theta parameter of the negative binomial had to be given 
# for the iterative algorithm to converge. 
summary(bn)
# We obtain an estimated theta value of 1.164, with a standard error of 0.187
# It can be seen that the deviance of the null model and the model are quite different
# To compare this model with the previous one (added in PDF) we can compare:
# - AIC
# - 2*Log-likelihood (log-likelihood)
# In our case it will be done with AIC, based on deviance (this is what 
# the anova command is based on)


## Model coefficients

beta_bn <- coef(bn)
ebeta_bn <- exp(beta_bn)


## Confidence intervals for the estimators

summary(bn)
sd_beta <- c(0.03436922, 0.0226294, 0.0003156)
param <- c("beta0", "beta1", "beta2")
niveles <- c(0.95, 0.99)
tipo <- c("PL", "As.")
intervalos <- data.frame(
  param = character(),
  tipo = character(),
  nivel_confianza = numeric(),
  inferior = numeric(),
  superior = numeric(),
  exp_inferior = numeric(),
  exp_superior = numeric(),
  stringsAsFactors = FALSE
)
for (i in seq_along(param)) {
  for (nivel in niveles) {
    ic <- confint(bn, level=nivel)[i,]
    eic <- exp(ic)
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[1],
        nivel_confianza = nivel,
        inferior = ic[1],
        superior = ic[2],
        exp_inferior = eic[1],
        exp_superior = eic[2]
      )
    )
    alpha <- 1 - nivel
    z <- qnorm(1 - alpha / 2, mean = beta_bn[i], sd = sd_beta[i])
    ic_inf <- beta_bn[i] - z*sd_beta[i]
    ic_sup <- beta_bn[i] + z*sd_beta[i]
    e_ic_inf <- exp(ic_inf)
    e_ic_sup <- exp(ic_sup)
    intervalos <- rbind(
      intervalos,
      data.frame(
        param = param[i],
        tipo = tipo[2],
        nivel_confianza = nivel,
        inferior = ic_inf,
        superior = ic_sup,
        exp_inferior = e_ic_inf,
        exp_superior = e_ic_sup
      )
    )
  }
}
# Remove row names, which we don't need
rownames(intervalos) <- NULL
# The obtained intervals are as follows
print(intervalos)
# Intervals for theta could also be added. The one based on PL cannot be
# obtained directly with the confint command, we would first have to manually calculate the 
# likelihood profile. On the other hand, asymptotic 
# intervals can be easily calculated since, being a
# maximum likelihood estimator, the asymptotic normal tendency is verified


## Graphical comparison of the intervals

ic_0_bn <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta0"]," ",
                  intervals$confidence_level[intervals$param=="beta0"]),
  lower = intervals$lower[intervals$param=="beta0"],
  upper = intervals$upper[intervals$param=="beta0"],
  exp_lower = intervals$exp_lower[intervals$param=="beta0"],
  exp_upper = intervals$exp_upper[intervals$param=="beta0"]
)
par(mfrow=c(1,1))
plot(NULL,
     xlim = range(ic_0_bn$lower, ic_0_bn$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[0]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[0])
)
for (i in 1:nrow(ic_0_bn)) {
  segments(ic_0_bn$lower[i], i, ic_0_bn$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_0_bn$lower[i], ic_0_bn$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_bn$method, las = 1)
abline(v = beta_bn[1], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_0_bn$exp_lower, ic_0_bn$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[0])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[0]))
)
for (i in 1:nrow(ic_0_bn)) {
  segments(ic_0_bn$exp_lower[i], i, ic_0_bn$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_0_bn$exp_lower[i], ic_0_bn$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_bn$method, las = 1)
abline(v = ebeta_bn[1], lty = 2, col = "grey30")
ic_1_bn <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta1"]," ",
                  intervals$confidence_level[intervals$param=="beta1"]),
  lower = intervals$lower[intervals$param=="beta1"],
  upper = intervals$upper[intervals$param=="beta1"],
  exp_lower = intervals$exp_lower[intervals$param=="beta1"],
  exp_upper = intervals$exp_upper[intervals$param=="beta1"]
)
plot(NULL,
     xlim = range(ic_1_bn$lower, ic_1_bn$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[1]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[1])
)
for (i in 1:nrow(ic_1_bn)) {
  segments(ic_1_bn$lower[i], i, ic_1_bn$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_1_bn$lower[i], ic_1_bn$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_bn$method, las = 1)
abline(v = beta_bn[2], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_1_bn$exp_lower, ic_1_bn$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[1])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[1]))
)
for (i in 1:nrow(ic_1_bn)) {
  segments(ic_1_bn$exp_lower[i], i, ic_1_bn$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_1_bn$exp_lower[i], ic_1_bn$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_bn$method, las = 1)
abline(v = ebeta_bn[2], lty = 2, col = "grey30")
ic_2_bn <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta2"]," ",
                  intervals$confidence_level[intervals$param=="beta2"]),
  lower = intervals$lower[intervals$param=="beta2"],
  upper = intervals$upper[intervals$param=="beta2"],
  exp_lower = intervals$exp_lower[intervals$param=="beta2"],
  exp_upper = intervals$exp_upper[intervals$param=="beta2"]
)
plot(NULL,
     xlim = range(ic_2_bn$lower, ic_2_bn$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[2]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[2])
)
for (i in 1:nrow(ic_2_bn)) {
  segments(ic_2_bn$lower[i], i, ic_2_bn$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_2_bn$lower[i], ic_2_bn$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_bn$method, las = 1)
abline(v = beta_bn[3], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_2_bn$exp_lower, ic_2_bn$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[2])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[2]))
)
for (i in 1:nrow(ic_2_bn)) {
  segments(ic_2_bn$exp_lower[i], i, ic_2_bn$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_bn$method[i]), "blue", "darkgreen"))
  points(c(ic_2_bn$exp_lower[i], ic_2_bn$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_bn$method, las = 1)
abline(v = ebeta_bn[3], lty = 2, col = "grey30")


## Graphical representation of the fit

plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)
x_seq <- seq(min(x), max(x), length.out = 200)
y_pred <- predict(bn, newdata = data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "orange", lwd = 2)
# We see that the model doesn't fit the data as well (graphically, 
# qualitative interpretation) as the Poisson
# We can also compare it graphically with that model
y_pred <- predict(poisson_2, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "red", lwd = 2)


## Calculation of residuals

res.b_bn <- residuals(bn, type="response")
res.p_bn <- residuals(bn, type="pearson")
res.d_bn <- residuals(bn, type="deviance")


## Representation of residuals

par(mfrow=c(2,2))
plot(bn)
# We still don't get rid of the data trend, so having 
# changed the hypothesis from Poisson to negative binomial hasn't 
# solved our problem with the residuals
pred_bn <- predict(bn, type="response")
pred.l_bn <- predict(bn, type="link") 
par(mfrow=c(2,2))
plot(pred_bn,res.b_bn) ; abline(0,0,col="gray", lwd=2)
plot(pred_bn,res.p_bn) ; abline(0,0,col="gray", lwd=2)
plot(pred_bn,res.d_bn) ; abline(0,0,col="gray", lwd=2)
plot(pred.l_bn,res.d_bn) ; abline(0,0,col="gray", lwd=2)
# Similar conclusions to the previous models


## Comparison with Poisson model

# H0 = poisson_2
# Ha = bn
# Required library for the test
library(lmtest)
lrtest(poisson_2, bn)
# We reject the null hypothesis at any usual significance level, so
# there is statistically significant evidence in favor of the model of 
# the alternative hypothesis. That is, there is evidence that the model with 
# negative binomial gave a better result with this data.
# This leads us to think that there was indeed some overdispersion (additionally,
# we saw that the theta estimator had a not very high value)


################################################################################
################################################################################

######################### NORMAL FIT Y~X+X^2 ###################################


## We try changing the first hypothesis again seeing that the residuals 
## still have a trend they shouldn't have. In this case, we will try with
## a normal model, justified because for high count values a Poisson
## asymptotically approaches a normal. The procedure is analogous to
## that performed for the Poisson and negative binomial models

normal <- glm(y~x+I(x^2), family=gaussian(link="log"))
summary(normal)
# We see that all parameters are significant. We also see that
# a dispersion factor is estimated, as in the negative binomial
# model, which will be the variance (independent of explanatory variables)
# of the normal distribution
# We also have an AIC value to compare models


## Model coefficients

beta_n <- coef(normal)
ebeta_n <- exp(beta_n)
# We can compare the exp(beta) obtained for each of the two-variable models to see that the values obtained for the normal are quite 
# close to those of Poisson, as expected
betas <- rbind(ebeta_p2, ebeta_bn, ebeta_n)


## Confidence intervals for the estimators

summary(normal)
sd_beta <- c(0.1497800, 0.0171066, 0.0004663)
param <- c("beta0", "beta1", "beta2")
levels <- c(0.95, 0.99)
type <- c("PL", "As.")
intervals <- data.frame(
  param = character(),
  type = character(),
  confidence_level = numeric(),
  lower = numeric(),
  upper = numeric(),
  exp_lower = numeric(),
  exp_upper = numeric(),
  stringsAsFactors = FALSE
)
for (i in seq_along(param)) {
  for (level in levels) {
    ic <- confint(normal, level=level)[i,]
    eic <- exp(ic)
    intervals <- rbind(
      intervals,
      data.frame(
        param = param[i],
        type = type[1],
        confidence_level = level,
        lower = ic[1],
        upper = ic[2],
        exp_lower = eic[1],
        exp_upper = eic[2]
      )
    )
    alpha <- 1 - level
    z <- qnorm(1 - alpha / 2, mean = beta_n[i], sd = sd_beta[i])
    ic_lower <- beta_n[i] - z*sd_beta[i]
    ic_upper <- beta_n[i] + z*sd_beta[i]
    e_ic_lower <- exp(ic_lower)
    e_ic_upper <- exp(ic_upper)
    intervals <- rbind(
      intervals,
      data.frame(
        param = param[i],
        type = type[2],
        confidence_level = level,
        lower = ic_lower,
        upper = ic_upper,
        exp_lower = e_ic_lower,
        exp_upper = e_ic_upper
      )
    )
  }
}
rownames(intervals) <- NULL
print(intervals)
# Intervals for sigma could also be added. The one based on PL cannot be
# obtained directly with the confint command, we would first have to manually calculate the 
# likelihood profile. For asymptotic intervals 
# we would need the standard error of said parameter, which is not given to us in the 
# summary. This would be obtained from the Fisher information matrix


## Graphical comparison of the intervals

ic_0_n <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta0"]," ",
                  intervals$confidence_level[intervals$param=="beta0"]),
  lower = intervals$lower[intervals$param=="beta0"],
  upper = intervals$upper[intervals$param=="beta0"],
  exp_lower = intervals$exp_lower[intervals$param=="beta0"],
  exp_upper = intervals$exp_upper[intervals$param=="beta0"]
)
par(mfrow=c(1,1))
plot(NULL,
     xlim = range(ic_0_n$lower, ic_0_n$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[0]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[0])
)
for (i in 1:nrow(ic_0_n)) {
  segments(ic_0_n$lower[i], i, ic_0_n$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_n$method[i]), "blue", "darkgreen"))
  points(c(ic_0_n$lower[i], ic_0_n$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_n$method, las = 1)
abline(v = beta_n[1], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_0_n$exp_lower, ic_0_n$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[0])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[0]))
)
for (i in 1:nrow(ic_0_n)) {
  segments(ic_0_n$exp_lower[i], i, ic_0_n$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_0_n$method[i]), "blue", "darkgreen"))
  points(c(ic_0_n$exp_lower[i], ic_0_n$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_0_n$method, las = 1)
abline(v = ebeta_n[1], lty = 2, col = "grey30")
ic_1_n <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta1"]," ",
                  intervals$confidence_level[intervals$param=="beta1"]),
  lower = intervals$lower[intervals$param=="beta1"],
  upper = intervals$upper[intervals$param=="beta1"],
  exp_lower = intervals$exp_lower[intervals$param=="beta1"],
  exp_upper = intervals$exp_upper[intervals$param=="beta1"]
)
plot(NULL,
     xlim = range(ic_1_n$lower, ic_1_n$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[1]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[1])
)
for (i in 1:nrow(ic_1_n)) {
  segments(ic_1_n$lower[i], i, ic_1_n$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_n$method[i]), "blue", "darkgreen"))
  points(c(ic_1_n$lower[i], ic_1_n$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_n$method, las = 1)
abline(v = beta_n[2], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_1_n$exp_lower, ic_1_n$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[1])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[1]))
)
for (i in 1:nrow(ic_1_n)) {
  segments(ic_1_n$exp_lower[i], i, ic_1_n$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_1_n$method[i]), "blue", "darkgreen"))
  points(c(ic_1_n$exp_lower[i], ic_1_n$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_1_n$method, las = 1)
abline(v = ebeta_n[2], lty = 2, col = "grey30")
ic_2_n <- data.frame(
  method = paste0(intervals$type[intervals$param=="beta2"]," ",
                  intervals$confidence_level[intervals$param=="beta2"]),
  lower = intervals$lower[intervals$param=="beta2"],
  upper = intervals$upper[intervals$param=="beta2"],
  exp_lower = intervals$exp_lower[intervals$param=="beta2"],
  exp_upper = intervals$exp_upper[intervals$param=="beta2"]
)
plot(NULL,
     xlim = range(ic_2_n$lower, ic_2_n$upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(beta[2]),
     ylab = "",
     main = bquote("Confidence intervals for " ~ beta[2])
)
for (i in 1:nrow(ic_2_n)) {
  segments(ic_2_n$lower[i], i, ic_2_n$upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_n$method[i]), "blue", "darkgreen"))
  points(c(ic_2_n$lower[i], ic_2_n$upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_n$method, las = 1)
abline(v = beta_n[3], lty = 2, col = "grey30")
plot(NULL,
     xlim = range(ic_2_n$exp_lower, ic_2_n$exp_upper),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = expression(exp(beta[2])),
     ylab = "",
     main = bquote("Confidence intervals for " ~ exp(beta[2]))
)
for (i in 1:nrow(ic_2_n)) {
  segments(ic_2_n$exp_lower[i], i, ic_2_n$exp_upper[i], i, lwd = 3,
           col = ifelse(grepl("0.99", ic_2_n$method[i]), "blue", "darkgreen"))
  points(c(ic_2_n$exp_lower[i], ic_2_n$exp_upper[i]), rep(i, 2), pch = 16)
}
axis(2, at = 1:4, labels = ic_2_n$method, las = 1)
abline(v = ebeta_n[3], lty = 2, col = "grey30")


## Graphical representation of the fit

plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)
x_seq <- seq(min(x), max(x), length.out = 200)
y_pred <- predict(normal, newdata = data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "green", lwd = 2)
# We see that the fitted model is more similar to Poisson than to Negative
# Binomial, as expected due to the similarity between Normal and Poisson for
# high counts
# We can also compare it graphically with the two previous models
y_pred <- predict(poisson_2, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "red", lwd = 2)
y_pred <- predict(bn, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "orange", lwd = 2)


## Calculation of residuals

res.b_n <- residuals(normal, type="response")
res.p_n <- residuals(normal, type="pearson")
res.d_n <- residuals(normal, type="deviance")


## Representation of residuals

par(mfrow=c(2,2))
plot(normal)
# It seems that there is no trend in the residuals except in points with high
# prediction values, i.e., near the mode of the data (shots near
# the edge of the box). For these points, the dispersion is much greater even
# for standardized residuals. Still, since it's not so clear that the
# residuals follow a trend we might initially think that it's possible that 
# we achieved a better model than the previous ones
pred_n <- predict(normal, type="response")
pred.l_n <- predict(normal, type="link") 
par(mfrow=c(2,2))
plot(pred_n,res.b_n) ; abline(0,0,col="gray", lwd=2)
plot(pred_n,res.p_n) ; abline(0,0,col="gray", lwd=2)
plot(pred_n,res.d_n) ; abline(0,0,col="gray", lwd=2)
plot(pred.l_n,res.d_n) ; abline(0,0,col="gray", lwd=2)
# With these graphs it's easier for us to see that there does seem to be a 
# slight trend in the residuals, as well as heteroscedasticity even in the 
# standardized residuals, so we cannot think that it's a very good
# fit since the hypotheses are not met. Again, the results lead us to 
# think that the log-linearity hypothesis (with which we've been working in
# all models) is not met


## Comparison with Poisson and Negative Binomial models

# H0 = poisson_2
# Ha = normal
library(lmtest)
lrtest(poisson_2, normal)
# We reject the null hypothesis at any usual significance level, so
# there is statistically significant evidence in favor of the model of 
# the alternative hypothesis. That is, there is evidence that the model with 
# normal gave a better result with this data than Poisson.
# H0 = bn
# Ha = normal
lrtest(bn,normal)
# Again, the null hypothesis is rejected at any usual significance
# level, and we keep the Normal model over the Negative Binomial.
# Therefore, among all the models used, we 
# would choose the Normal (even considering that it seems that the 
# log-linearity hypothesis is not met in the data, which would lead us
# to look for other types of models, as we will do next). 


################################################################################
################################################################################

####################### POISSON GAM FIT Y~X+X^2 ###############################


## We've already made several attempts modifying the hypothesis of the distribution 
## that the conditional mean follows. Seeing that the data still don't fit
## the models in a way that the residuals don't follow any trend, we're going to 
## try modifying another hypothesis: that of log-linearity. We'll try to 
## fit a GAM (Generalized Additive Model). Therefore, instead of
## having x'beta, we'll have a sum of functions beta_0+f(x_1)+...+f(x_n), so 
## we move to a non-parametric model (we no longer have the 
## log-linearity hypothesis, we only know that the effect of each variable
## is additive). In this case, to not extend the study too much, we'll use the 
## GAM model only for the Poisson family and with the two variables (x and x^2)
## For this model we'll only show the results, we won't focus on 
## interpretations, confidence intervals, etc. because it deviates from the 
## objectives of the work

# Required library for the fit
library(mgcv)
# Create a GAM that allows us to separate components
poisson_gam <- gam(y ~ s(x, bs = "tp", k = 3), family = poisson)
summary(poisson_gam)
# Get the design matrix of the smooth term
x_seq <- seq(min(x), max(x), length = 100)
newdata <- data.frame(x = x_seq)
# Predict the design matrix
Xp <- predict(poisson_gam, newdata = newdata, type = "lpmatrix")
# The coefficients of the smooth term
coef_smooth <- coef(poisson_gam)[-1]  # exclude intercept
# The complete smooth function
f_total <- Xp[,-1] %*% coef_smooth  # exclude intercept column
# To decompose into polynomial components, we approximate with a 2nd order polynomial
poly_fit <- lm(f_total ~ poly(x_seq, degree = 2, raw = TRUE))
# Components
beta_poly <- coef(poly_fit)
f1_approx <- beta_poly[2] * x_seq  # linear component
f2_approx <- beta_poly[3] * (x_seq^2)  # quadratic component


## Approximate graphical representation of f_1(x) and f_2(x)

# Complete smooth function of the GAM
plot(poisson_gam, se = TRUE, shade = TRUE, rug = FALSE,
     xlab = "Distance to goal (m)", ylab = expression((f[1]+f[2])(x)), ylim = range(f_total))
# Approximate linear component f_1(x)
plot(x_seq, f1_approx, type = "l", lwd = 2, col = "blue",
     xlab = "Distance to goal (m)", ylab = expression(f[1](x)))
# Approximate quadratic component f_2(x)  
plot(x_seq, f2_approx, type = "l", lwd = 2, col = "red",
     xlab = "Distance to goal (m)", ylab = expression(f[2](x)))


## Graphical representation of the model predictions

plot(
  x, y,
  pch = 19, col = "grey30",
  xlab = "Distance to goal (m)", # X-axis
  ylab = "Number of shots", # Y-axis
)
pred_total <- predict(poisson_gam, newdata = newdata, type = "response")
lines(x_seq, pred_total, type = "l", lwd = 2, col = "darkblue")
# We can also compare it with the rest of the models
y_pred <- predict(normal, newdata = data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "green", lwd = 2)
y_pred <- predict(poisson_2, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "red", lwd = 2)
y_pred <- predict(bn, newdata= data.frame(x = x_seq), type = "response")
lines(x_seq, y_pred, col = "orange", lwd = 2)
# Graphically it seems that it doesn't fit as well as Poisson or Gaussian,
# perhaps with more degrees of freedom in the polynomial it would fit better than these 
# others. Regarding the fitted models, we would choose the model that
# assumes that the conditional mean is that of a normal distribution, taking
# into account that it seems that the log-linearity hypothesis is not met
# in the data


## Calculation of residuals

res.b_gam <- residuals(poisson_gam, type="response")
res.p_gam <- residuals(poisson_gam, type="pearson")
res.d_gam <- residuals(poisson_gam, type="deviance")


## Representation of residuals

pred_gam <- predict(normal, type="response")
pred.l_gam <- predict(normal, type="link") 
par(mfrow=c(2,2))
plot(pred_gam,res.b_gam) ; abline(0,0,col="gray", lwd=2)
plot(pred_gam,res.p_gam) ; abline(0,0,col="gray", lwd=2)
plot(pred_gam,res.d_gam) ; abline(0,0,col="gray", lwd=2)
plot(pred.l_gam,res.d_gam) ; abline(0,0,col="gray", lwd=2)
# We see that there is still a trend with the residuals, so with this
# model we haven't solved the problem we had with the other models
# Perhaps giving more freedom to the GAM with the f functions we could achieve a 
# better result