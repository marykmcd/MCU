# Please visit by article at Linkedin for detailed explanation.
# The link to the article is https://www.linkedin.com/pulse/avengers-endgame-surpass-avatar-simple-regression-analysis-kumavat

rm(list = ls())

data = read.csv("MarvelUniverse.csv")
head(data)

dim(data)
str(data)

summary(data)

# Let's now load our test data.

data2 = read.csv("AvengersEndgame.csv")
head(data2)

# I must drop the redundant Movie variable. I'll also drop the 
# OverseasGross and WorldwideGross variables as they are not yet
# determined while we make predictions.

# Final train dataset
X = subset(data, select = -c(Movie,OverseasGross,WorldwideGross))

# Final test dataset
y = subset(data2, select = -c(Movie,OverseasGross,WorldwideGross,DomesticGross))

str(X)
str(y)

plot(data$Budget, data$DomesticGross) 
plot(data$YearOfRelease, data$DomesticGross) 
plot(data$RunningTime, data$DomesticGross) 
plot(data$DomesticScreens, data$DomesticGross) 
plot(data$WeekendGross, data$DomesticGross) 

#hist(data$Budget)
#hist(data$WeekendGross)

# Let's build our first model.

linearmodel = lm(DomesticGross ~ .,data = X)
summary(linearmodel)
# This is giving me ridiculous numbers. I hate it!

# Let's build our regression model to predict our response variable i.e DomesticGross
pred = predict(linearmodel, y)
head(pred)
# As dreaded, yes the prediction is absurd. "I honestly don't line this one!" ;)


# I now decide to keep it simple and select only the continious independent variables.

linearmodel2 = lm(DomesticGross ~ YearOfRelease + RunningTime + DomesticScreens +
                     WeekendGross, data = X)
summary(linearmodel2)

# Well.. "I like this one!" ;)


y = subset(data2, select = c(YearOfRelease, RunningTime, DomesticScreens,
                                WeekendGross))
#summary(y)

pred = predict(linearmodel2, y)
head(pred)


# Predicting Avengers: Infinity war's domestic Gross.
#X
#dim(X)

# Removing Avengers: Infinity war from the train dataset.
X_new = X[-c(13),]
dim(X_new)

y_new = X[c(13),]
y_new

linearmodel_new = lm(DomesticGross ~ YearOfRelease + RunningTime + DomesticScreens +
                    WeekendGross, data = X_new)
summary(linearmodel_new)


pred = predict(linearmodel_new, y_new)
head(pred)

