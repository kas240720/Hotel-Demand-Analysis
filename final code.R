---
  author: "Minji Song"
output:
  pdf_document: default
word_document:
  toc: yes
html_document:
  fig_width: 10
toc: yes
urlcolor: cyan
---
  # Explantory Data Analysis
  
  ```{r}
tinytex::install_tinytex()
library(ggplot2)
library(cowplot)
library(ggcorrplot)
library(dplyr)

data = read.csv("C:/Users/sec/Desktop/HW UIUC/SP 20/STAT 425/stat425_fpdata.csv")

data$is_canceled <- as.factor(ifelse(data$is_canceled==1, 'Yes', 'No'))


data$meal = recode(data$meal, SC = 'no meal', Undefined = 'no meal')

data = data[, -1]
set.seed(2)
trn_idx = sample(nrow(data), size = 0.8 * nrow(data))
trn = data[trn_idx, ]
tst = data[trn_idx, ]

```

I changed some values of the is_canceled variable and meal variable. is_canceled variable is a categorical variable with 0 and 1, so I changed it to 0 to No and 1 to Yes to identify values easily.
For meal variables, SC and Undefined have the same meaning but they are different values, thus I combined both values to name it as no meal. 


```{r}
plot_grid( ggplot(trn, aes(x=arrival_date_month))+ geom_bar() , ggplot(trn, aes(x=meal))+ geom_bar() , ggplot(trn, aes(x=market_segment))+ geom_bar() , ggplot(trn, aes(x=reserved_room_type, ))+ geom_bar(), ggplot(trn, aes(x=customer_type, ))+ geom_bar())
```

For categorical variables, I performed Explanatory Data Analysis by plotting each categorical variable to see the relationship between adr and other variables. These bar plots show the proportion of each variable. Meal and reserved room types are right-skewed and the market segment is left-skewed. There is no variable that has a perfect normal graph. 

```{r}
plot_grid(ggplot(trn,aes(x=arrival_date_month, y=adr))+ geom_boxplot(), ggplot(trn,aes(x=meal, y=adr))+ geom_boxplot(), ggplot(trn,aes(x=market_segment, y=adr))+ geom_boxplot(), ggplot(trn,aes(x=reserved_room_type, y=adr))+ geom_boxplot(), ggplot(trn,aes(x=customer_type, y=adr))+ geom_boxplot())
```

For the arrival date month variable, we can see that August is associated with high adr, and January has low adr. Reserved room type H has a higher adr with about 180 adr. Other variables in this plot have similar trend. 

```{r}
plot_grid(ggplot(trn,aes(x=lead_time, y=adr))+ geom_point(),
          ggplot(trn,aes(x=arrival_date_year, y=adr))+ geom_point(),
          ggplot(trn,aes(x=arrival_date_week_number, y=adr))+ geom_point(),
          ggplot(trn,aes(x=arrival_date_day_of_month, y=adr))+ geom_point(),
          ggplot(trn,aes(x=stays_in_weekend_nights, y=adr))+ geom_point(),
          ggplot(trn,aes(x=stays_in_week_nights, y=adr))+ geom_point(),
          ggplot(trn,aes(x=adults, y=adr))+ geom_point(),
          ggplot(trn,aes(x=children, y=adr))+ geom_point(),
          ggplot(trn,aes(x=babies, y=adr))+ geom_point(),
          ggplot(trn,aes(x=total_of_special_requests, y=adr))+ geom_point())
```


It seems lead time and arrival date week numer have trend in each plot so we might think the relationship between the two.

```{r}
correl = round(cor(trn[,c("lead_time", "arrival_date_year", "arrival_date_week_number", "arrival_date_day_of_month", "stays_in_weekend_nights", "stays_in_week_nights", "adults", "children", "babies", "total_of_special_requests")]),1)

ggcorrplot(correl)

```

we see that arrival date week number and arrival date year tend to have a negative correlation more than other variables. 

```{r}

m = lm(adr~. , trn)
```


```{r}
par(mfrow=c(3,4))

plot(trn$lead_time ,resid(m))
abline(0,0)
plot(trn$arrival_date_year ,resid(m))
abline(0,0)
plot(trn$arrival_date_week_number ,resid(m))
abline(0,0)
plot(trn$arrival_date_day_of_month,resid(m))
abline(0,0)
plot(trn$stays_in_weekend_nights,resid(m))
abline(0,0)
plot(trn$stays_in_week_nights,resid(m))
abline(0,0)
plot(trn$adr,resid(m))
abline(0,0)
plot(trn$arrival_date_month ,resid(m))
abline(0,0)
plot(trn$children ,resid(m))
abline(0,0)
plot(trn$babies ,resid(m))
abline(0,0)
plot(trn$total_of_special_requests ,resid(m))
abline(0,0)
```

All plots are well distributed and there is no clear trend. Even though lead time plot looks like to be a heteroscedasticity a bit in a little plot, it is distributed well enough. From the residual plots, none of the numerical variables support the nonlinear trend.



# Linear Regression

```{r}
m = lm(adr~. +lead_time*arrival_date_week_number,trn)
summary(m)
```

I performed linear regression using all variables and interaction terms with lead time and arrival date week numbers. In a summary, the p-value of interaction term is lower than 0.05, so we keep this significant variable in the model.

```{r}
new_mod = step(m, direction="both")
summary(new_mod)
```

I performed the AIC model selection to get rid of some useless variables based on the small AIC value. Through the selection, the final model has 14 variables: is canceled, lead time, arrival date year, arrival date month, arrival date week number, stay weeknights, adults, children, meal, market segment, reserved room type, customer type, a total of special requests and the interaction terms.


```{r}
library(faraway)
vif(new_mod)

new_mod = lm(formula = adr ~ is_canceled + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + stays_in_week_nights +  adults + children + babies + meal + market_segment + reserved_room_type + customer_type + total_of_special_requests + lead_time:arrival_date_week_number, data = trn)
```

Before we confirm the final model, I performed the VIF test to see if there is any multicollinearity with this model. It seems like there is multicollinearity in this model because some values have high VIF values. The highest VIF is arrival date week number with 81.3846, there are few more variables that have over 10 VIFs. However, those variables that have high VIF values can be ignored since those are either categorical variables with more than two levels or part of interaction terms. Other than those variables, all other variables look good. 

```{r}
test = trn[c(1,1,1),]
test$arrival_date_week_number= c(min(trn$arrival_date_week_number), median(trn$arrival_date_week_number), max(trn$arrival_date_week_number))
test
predict(new_mod, test)
```

A large number of arrival date week number has a higher average daily rate than a small number of arrival date week number. When the arrival date week number is 1, 29 and 53, the predicted adr are 21.74, 103.47, and 173.52. 

```{r}
test = trn[c(1,1,1),]
test$lead_time= c(min(trn$lead_time), median(trn$lead_time), max(trn$lead_time))
test
predict(new_mod, test)
```

The shorter lead time has a higher average daily rate by 30% than 537 days longer lead time. The shorter time between booking date and arrival date have a higher rate. 

```{r}
test = trn[c(1,1,1),]
test$arrival_date_year  = c(min(trn$arrival_date_year  ), median(trn$arrival_date_year  ), max(trn$arrival_date_year  ))
test
predict(new_mod, test)
```

This data set is for between 2015 and 2017. In 2015, the average daily rate is much lower than in 2017. In 2017, the rate has increased by about 45% compared to 2015 and it was -6.5 in 2015.

```{r}
test = trn[c(1,1,1),]
test$stays_in_week_nights  = c(min(trn$stays_in_week_nights  ), median(trn$stays_in_week_nights  ), max(trn$stays_in_week_nights  ))
test
predict(new_mod, test)
```


The more nights staying in a week increases the average daily rate. The difference between 0 nights to 10 nights is 28.1% .

```{r}
test = trn[c(1,1,1),]
test$adults  = c(min(trn$adults  ), median(trn$adults  ), max(trn$adults  ))
test
predict(new_mod, test)
```

```{r}
test = trn[c(1,1,1),]
test$children  = c(min(trn$children  ), median(trn$children  ), max(trn$children  ))
test
predict(new_mod, test)
```


More adults and children also increase the average daily rate.

```{r}
test = trn[c(1,1,1),]
test$total_of_special_requests = c(min(trn$total_of_special_requests), median(trn$total_of_special_requests), max(trn$total_of_special_requests))
test
predict(new_mod, test)
```


a lot of special requests increase the daily average rate. A special request increases the rate by about 4.5%.


# Regression Tree


```{r}
library(rpart)
rpmod = rpart(adr ~ is_canceled + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + stays_in_week_nights +  adults + children + babies + meal + market_segment + reserved_room_type + customer_type + total_of_special_requests,cp=0.01, data = trn)
rpmod

plot(rpmod,compress=T,uniform=T,branch=0.4,margin=.10)
text(rpmod)

printcp(rpmod)

plotcp(rpmod)
```

This regression split the model into several steps and the first node of the variable we split on is arrival date month with 181 observations. The second branch is also an arrival date month with 202 observations. It is telling that the most important variable in this regression is the arrival date month.

```{r}
rpmods = rpart(adr ~ is_canceled + lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number + stays_in_week_nights +  adults + children + babies + meal + market_segment + reserved_room_type + customer_type + total_of_special_requests, 
               cp=0.001, data = trn)

printcp(rpmods)
plotcp(rpmods)
```

We can fit a bigger tree by pruning the tree using cp=0.001 instead of cp=0.01. 

```{r}
tb=rpmods$cptable
id.min = which.min(tb[, 'xerror'])
err = tb[id.min,'xerror'] +tb[id.min, 'xstd']
plotcp(rpmods)
abline(h=err, col="red")
```

I chose smallest tree size with CV error within 1 standard error of the smallest CV error to determine the optimal tree size and the red line of the plot indicates the CV error within 1 standard error of the smallest CV error.

```{r}
id = min(which(tb[, 'xerror'] < err))
cp = (tb[id, 'CP'] + tb[id-1, 'CP'])/2
cp

cpp= prune.rpart(rpmods, cp)

plot(cpp)
text(cpp)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(cpp, caption=NULL)
```

This is the final optimal regression tree size based on optimal cv error.


```{r}
library(randomForest)
rfm = randomForest(adr ~ is_canceled + reserved_room_type + market_segment + lead_time + arrival_date_year + arrival_date_week_number + stays_in_week_nights + adults + children + total_of_special_requests, trn)

p = predict(rfm, trn)

#training error
sum((trn$adr - p)^2)

#test(CV) error
sum((trn$adr - rfm$predicted)^2)

```

Using the same predictors as linear regression in a tree model, I fitted the random forests to see how training error and cross-validation test errors are different. The training error is 101966.9 and the test error is 381974.4. 