setwd("C:/Users/hiras/OneDrive - The University of Chicago/UChicago/Statistical Models")
getwd()

data <- read.csv("no_nulls_election_data.csv")
nrow(data)
data <- na.omit(data)
nrow(data)

head(data)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggcorrplot")
library(ggcorrplot)

install.packages("car")
library(car)

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

# Choosing which election columns to omit

election_cols <- c("percentage16_Donald_Trump", "percentage16_Hillary_Clinton",
                   "total_votes16",
                   "percentage20_Donald_Trump", "percentage20_Joe_Biden", "total_votes20", 
                   "turnout_difference")

el <- cor(data[election_cols])

ggcorrplot(el[,ncol(el):1],
           legend.title = '',
           color = c('red4', 'white', 'skyblue3'),
           outline.color = 'white',
           lab = TRUE,
           lab_size = 3)

# Keep columns: "percentage20_Joe_Biden", "turnout_difference", "total_votes16"

data$NonWhite <- data$Hispanic + data$Black + data$Native + data$Asian + data$Pacific

demographic_cols <- c("percentage20_Joe_Biden", "NonWhite", "TotalPop", "Hispanic", "White",
                    "Black", "Native", "Asian", "Pacific", "VotingAgeCitizen", "Income",
                    "IncomeErr", "IncomePerCap", "IncomePerCapErr", "Poverty", "ChildPoverty")

dc <- cor(data[demographic_cols])

dc

ggcorrplot(dc[,ncol(dc):1],
           legend.title = '',
           color = c('red4', 'white', 'skyblue3'),
           outline.color = 'white',
           lab = TRUE,
           lab_size = 3)

# Keep columns: "NonWhite", Hispanic", "White", "Black", "Native", "Asian", "Pacific", "VotingAgeCitizen", "Income"

# Choosing which commute columns to omit

commute_cols <- c("percentage20_Joe_Biden", "Drive",
                     "Carpool", "Transit", "Walk", "OtherTransp", "WorkAtHome", "MeanCommute")

cc <- cor(data[commute_cols])

ggcorrplot(cc[,ncol(cc):1],
           legend.title = '',
           color = c('red4', 'white', 'skyblue3'),
           outline.color = 'white',
           lab = TRUE,
           lab_size = 3)

# Keep columns: "Transit"

# Choosing which employment columns to omit

employment_cols <- c("percentage20_Joe_Biden", "Income",
                     "Professional", "Service", "Office", "Construction", "Production",
                     "Employed", "PrivateWork", "PublicWork", "SelfEmployed", "FamilyWork",
                     "Unemployment")

ec <- cor(data[employment_cols])

ggcorrplot(ec[,ncol(ec):1],
           legend.title = '',
           color = c('red4', 'white', 'skyblue3'),
           outline.color = 'white',
           lab = TRUE,
           lab_size = 3)

# Keep columns: "Unemployment"

###########

# 2020 Winner Vote Share vs. % of Voters that are NonWhite - BY STATE
ggplot(data, aes(x = NonWhite, y = percentage20_Joe_Biden)) +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(state), ncol=10) +
  ggtitle('percentage20_Joe_Biden ~ NonWhite')

# Turnout difference between 2016 and 2020 and 2020 Winner Vote Share
ggplot(data, aes(x = turnout_difference, y = percentage20_Joe_Biden)) +
  geom_point(alpha = 0.1)

# 2020 Winner Vote Share vs. COVID cases as % of population - BY STATE
ggplot(data, aes(x = case_pct, y = percentage20_Joe_Biden)) +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(state), ncol=10)

# 2020 Winner Vote Share vs. COVID deaths as % of population - BY STATE
ggplot(data, aes(x = death_pct, y = percentage20_Joe_Biden)) +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(state), ncol=10)

# 2020 Winner Vote Share vs. COVID cases as % of population in county
ggplot(data, aes(x = case_pct, y = percentage20_Joe_Biden))+
  geom_point(alpha = 0.1) 

# 2020 Winner Vote Share vs. COVID deaths as % of population in county
ggplot(data, aes(x = death_pct, y = percentage20_Joe_Biden))+
  geom_point(alpha = 0.1) 

ggplot(data, aes(NonWhite, percentage20_Joe_Biden)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('percentage20_Joe_Biden ~ NonWhite')

ggplot(data, aes(percentage16_Hillary_Clinton, percentage20_Joe_Biden)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('percentage20_Joe_Biden ~ percentage16_Hillary_Clinton')

###########

# 2016 results explain the 2020 results with 0.95 r-squared - but multicollinear
model.a <- lm(percentage20_Joe_Biden ~ percentage16_Hillary_Clinton, data = data)
summary(model.a) 
summary(model.a)$r.squared #0.95

# COVID metrics alone with 0.02 r-squared
model.b <- lm(percentage20_Joe_Biden ~ case_pct + death_pct, data = data)
summary(model.b)
summary(model.b)$r.squared #0.02

# COVID deaths percent has p-value 0.57, not useful when you add in some demographics
model.c <- lm(percentage20_Joe_Biden ~ White + Income + case_pct + death_pct + Men_pct, data = data)
summary(model.c)
summary(model.c)$r.squared #0.40
summary(model.c)$adj.r.squared

# All variables show small p-values, r-squared of 0.45
model.d <- lm(percentage20_Joe_Biden ~ White + Black + Income + Men_pct + Unemployment, data = data)
summary(model.d)
summary(model.d)$r.squared #0.45
summary(model.d)$adj.r.squared #0.45

# Adding in more (useful) variables increases the adjusted r-squared as well
model.e <- lm(percentage20_Joe_Biden ~ White + Black + Income + Men_pct + Unemployment + VotingAgeCitizen +  lat + Transit, data = data)
summary(model.e)
summary(model.e)$r.squared #0.60
summary(model.e)$adj.r.squared #0.60

# Add in State as a factor, and replace White with interaction term of turnout_difference. Unemployment and lat less useful.
model.f <- lm(percentage20_Joe_Biden ~ factor(state) + turnout_difference*White + Black + Income + Men_pct + Unemployment + lat + Transit, data = data)
summary(model.f)
summary(model.f)$r.squared #0.74
summary(model.f)$adj.r.squared #0.7349

# Removing unemployment and latitude as they had highest p-values in model.f and new r-squared is similar.
model.g <- lm(percentage20_Joe_Biden ~ factor(state) + turnout_difference*White + Black + Income + Men_pct + Transit, data = data)
summary(model.g)
summary(model.g)$r.squared #0.738
summary(model.g)$adj.r.squared #0.7337

# State interacting with NonWhite population, adding in COVID metrics.
model.h <- lm(percentage20_Joe_Biden ~ factor(state)*NonWhite + case_pct + death_pct + turnout_difference + Income  + Men_pct + Transit, data = data)
summary(model.h)
summary(model.h)$r.squared # 0.751
summary(model.h)$adj.r.squared # 0.743

# See which race interacting with COVID cases or deaths

with(data, cor(case_pct, Black, use = "complete.obs"))
with(data, cor(case_pct, White, use = "complete.obs"))
with(data, cor(case_pct, NonWhite, use = "complete.obs"))
with(data, cor(death_pct, Black, use = "complete.obs"))
with(data, cor(death_pct, White, use = "complete.obs"))
with(data, cor(death_pct, NonWhite, use = "complete.obs"))

covid.race_cols <- c("Black", "White", "NonWhite", "case_pct", "death_pct")

data_clean <- data[complete.cases(data[covid.race_cols]),]

cr <- cor(data_clean[covid.race_cols])

ggcorrplot(cr[,ncol(cr):1],
           legend.title = '',
           color = c('red4', 'white', 'skyblue3'),
           outline.color = 'white',
           lab = TRUE,
           lab_size = 3)

# Interactions with case_pct is higher than death_pct, so we ignore death_pct

# We remove death_pct and get the same results. 
# We have it interact with black instead of White/NonWhite to avoid multicollinearity. 
# Black and NonWhite have correlation of 0.62, so not necessarily a problem.
model.i <- lm(percentage20_Joe_Biden ~ factor(state)*NonWhite + case_pct*Black + turnout_difference + Income + Men_pct + Transit, data = data)
summary(model.i)
summary(model.i)$r.squared # 0.761
summary(model.i)$adj.r.squared # 0.753
length(coef(model.i)) #107

# Testing for p-values of variables we removed.
model.test <- lm(percentage20_Joe_Biden ~ factor(state)*NonWhite + case_pct + Black + turnout_difference + Income + Men_pct + Transit, data = data)
summary(model.test)
summary(model.test)$r.squared # 0.762
summary(model.test)$adj.r.squared # 0.754
length(coef(model.test)) #110

# See if it makes a difference if we have interaction with state/nonwhite/case together.
# Increases the r-squared by 0.02 but increases model complexity.
model.j <- lm(percentage20_Joe_Biden ~ factor(state)*NonWhite*case_pct + turnout_difference + Income  + Men_pct + Transit, data = data)
summary(model.j)
summary(model.j)$r.squared #0.77
summary(model.j)$adj.r.squared #0.756
length(coef(model.j)) #204

# Adding in voting age citizen population to model.i. Improves r-squared and adjusted r-squared, but not by much.
model.k <- lm(percentage20_Joe_Biden ~ factor(state)*NonWhite + case_pct*Black + VotingAgeCitizen + turnout_difference + Income + Men_pct + Transit, data = data)
summary(model.k)
summary(model.k)$r.squared #0.765
summary(model.k)$adj.r.squared #0.754
length(coef(model.k)) #108

# Final model visuals

final.model <- model.i
summary(final.model)
summary(final.model)$r.squared # 0.761
summary(final.model)$adj.r.squared # 0.753
length(coef(final.model)) #107

plot(final.model$fitted.values, residuals(final.model), 
     xlab = "Predicted Percentage of Joe Biden 2020 Votes", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)

# QQ Plot
r <- residuals(final.model)

hist(r)

qqnorm(r)
qqline(r, col = 'red', lwd = 2)
grid()

bptest(final.model)
bptest(final.model, studentize = TRUE)

# Log of response variable to see if it mitigates heteroscedasticity

ggplot(data, aes(x = NonWhite, y = percentage20_Joe_Biden)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

log.model <- lm(log(percentage20_Joe_Biden) ~ factor(state)*NonWhite + case_pct*Black + turnout_difference + Income + Men_pct + Transit, data = data)
summary(log.model)
summary(log.model)$r.squared # 0.70
summary(log.model)$adj.r.squared # 0.69
length(coef(log.model)) #107

bptest(log.model)
bptest(log.model, studentize = TRUE)

plot(log.model$fitted.values, residuals(log.model), 
     xlab = "Log of Predicted Percentage of Joe Biden 2020 Votes", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)

# QQ Plot
lr <- residuals(log.model)

hist(lr)

qqnorm(lr)
qqline(lr, col = 'red', lwd = 2)
grid()

##### 

residuals <- resid(final.model)
fitted_values <- fitted(final.model)

par(mfrow = c(2, 4))

plot(fitted_values, residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "blue")

plot(data$NonWhite, residuals, main = "Residuals vs NonWhite", xlab = "NonWhite", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$case_pct, residuals, main = "Residuals vs case_pct", xlab = "case_pct", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$Black, residuals, main = "Residuals vs Black", xlab = "Black", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$turnout_difference, residuals, main = "Residuals vs turnout_difference", xlab = "turnout_difference", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$Income, residuals, main = "Residuals vs Income", xlab = "Income", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$Men_pct, residuals, main = "Residuals vs Men_pct", xlab = "Men_pct", ylab = "Residuals")
abline(h = 0, col = "red")

plot(data$Transit, residuals, main = "Residuals vs Transit", xlab = "Transit", ylab = "Residuals")
abline(h = 0, col = "red")

#######

bptest(final.model, studentize = TRUE)
bptest((percentage20_Joe_Biden) ~ NonWhite, data = data)
bptest((percentage20_Joe_Biden) ~ case_pct, data = data)
bptest((percentage20_Joe_Biden) ~ Black, data = data)
bptest((percentage20_Joe_Biden) ~ turnout_difference, data = data)
bptest((percentage20_Joe_Biden) ~ Income, data = data)
bptest((percentage20_Joe_Biden) ~ Men_pct, data = data)
bptest((percentage20_Joe_Biden) ~ Transit, data = data)
