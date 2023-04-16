# Churn-Reduction
Shows how to determine which factors may be important to customer churn and how to calculate value gained by addressing these factors.

This R script takes an oversampled test dataset, so it first finds the true churn rate from a representative set of data, the oversampled test data churn rate, and then provides a weight for the data.

The script then uses a logistic regression to plot the churn probability of the test data set.

The script calulcates the odds ratio of churn happening with an increase to each variable and the "Importance" of each variable to the churn rate. Importance identifies the variables that will likely play the biggest impact in determining churn rate are identified.

Importance Calc for each variable:

For non-dummy variables (non-binary variables)
x = (odds ratio)^(2 * stadard deviation)
If X > 1, importance = X; else, importance = 1/X

For dummy variables (binary variables)
x = odds ratio
If X > 1, importance = X; else, importance = 1/X

The importance calc gives us an idea of what metrics we should try to adjust through marketing promotions or product changes.

The script then provides two examples of such changes:

First, it looks to reduce the number of days that a user has their game console by offering a promotion to users who have owned a console for longer than a year, enticing new sales and resetting their "consldays" number to 0. This effectively reduces churn probability from 8.8% to 7.79%.

Second, I assume that a multiplayer game gets a new game feature added onto it that drives up the number of minutes played by users over the past 3 months (mchange metric). This would reduce churn probability from 8.8% for the data set to 7.97%.

In the images "LTV calc console day" and "LTV calc multiplayer mins", I show the effect on the Lifetime Value (LTV) of these customers by reducing their churn rate by these amounts. Some assumptions taken into account are a $59.99 monthly service charge, 3% yearly revenue and cost increases, an 8.6% discount rate (risk free rate + market risk premium),a nd a 5 year time horizon on customer value.
