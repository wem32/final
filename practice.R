# Performing an Exploratory Data analysis

# loading packages
library(modelr)
library(tidyverse)
library(ggplot2)
library(knitr)
library(readxl)


# loading data
credit_c.d <- read_excel("data/default of credit card clients.xls")

str(credit_c.d)

credit_c.d[2,]
# getting a quick glance of the data
View(head(credit_c.d, 5))

# five number summary
lapply(credit_c.d[, -c(1, 3:12)], summary)

# Visualizing credit amount based on gender, education, and marriage status
credit_c.d %>%
  ggplot(aes(x= limit_bal)) +
  geom_freqpoly(aes(color= sex)) +
  facet_grid(educ~marriage)

# visualizing default
credit_c.d %>%
  ggplot(aes(x=default_pay))+
  geom_bar()

# There are many more credit card payment default for next month than payment on schedule. This is somewhat
# to be expected since this was a period approaching the financial crisis where there was easy access to credit.


# visualizing payment cycle per month starting in September 2005 going down to April 2005
credit_c.d %>%
  ggplot(aes(x = repay_0905)) +
  geom_bar()

credit_c.d %>%
  ggplot(aes(x=repay_0805)) +
  geom_bar(color = "blue")

credit_c.d %>%
  ggplot(aes(x=repay_0705)) +
  geom_bar()

credit_c.d %>%
  ggplot(aes(x=repay_0605)) +
  geom_bar()

credit_c.d %>%
  ggplot(aes(x=repay_0505)) +
  geom_bar()

credit_c.d %>%
  ggplot(aes(x=repay_0405)) +
  geom_bar()

# For some reason, the repayment code includes -2 and 0, which are not mentioned in the codebook.

# Visualizing quantitative variables broken down into age sector
credit_c.d %>%
  filter(between(age, 18, 23)) %>%
  ggplot(aes(x = limit_bal/100, y = pay_amt1/100)) +
  geom_point(aes(color = sex)) +
  facet_grid(educ~marriage)


credit_c.d %>%
  filter(between(age, 24, 32)) %>%
  ggplot(aes(x = limit_bal/100, y = pay_amt1/100)) +
  geom_point(aes(color = sex)) +
  labs(x= "limit-bal in hundred dollars", y = " sept paymentin hundred dollars")+
  facet_grid(educ~marriage)

credit_c.d %>%
  filter(between(age, 33, 40)) %>%
  ggplot(aes(x = log(limit_bal), y = log(pay_amt1))) +
  geom_point(aes(color = sex)) +
  labs(x= "limit-bal in hundred dollars", y = "sept payment in hundred dollars")+
  facet_grid(educ~marriage)  

credit_c.d %>%
  filter(between(age, 41, 50)) %>%
  ggplot(aes(x = limit_bal/100, y = pay_amt1/100)) +
  geom_point(aes(color = sex)) +
  labs(x= "limit-bal in hundred dollars", y = "sept payment in hundred dollars")+
  facet_grid(educ~marriage)

credit_c.d %>%
  filter(between(age, 51, 60)) %>%
  ggplot(aes(x = limit_bal/100, y = pay_amt1/100)) +
  geom_point(aes(color = sex)) +
  labs(x= "limit-bal in hundred dollars", y = "sept payment in hundred dollars")+
  facet_grid(educ~marriage)

credit_c.d %>%
  filter(between(age, 60, 80)) %>%
  ggplot(aes(x = limit_bal/100, y = pay_amt1/100)) +
  geom_point(aes(color = sex)) +
  labs(x= "limit-bal in hundred dollars", y = "sept payment in hundred dollars")+
  facet_grid(educ~marriage)

# Focusing particular for people with either who completed grad school or who are currently in grad school
credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt1/100, y = pay_amt1/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "september pay in hundred dollars")

credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt2/100, y = pay_amt2/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "August pay in hundred dollars")

credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt3/100, y = pay_amt3/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "July pay in hundred dollars")

credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt4/100, y = pay_amt4/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "June pay in hundred dollars")

credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt5/100, y = pay_amt5/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "May pay  in hundred dollars")

credit_c.d %>%
  filter(between(age, 24, 32), educ ==c("grad school", "university")) %>%
  ggplot(aes(x = bill_amt6/100, y = pay_amt6/100, color = sex)) +
  geom_point() +
  facet_wrap(~marriage) +
  labs(x= "bill_amount in hundred dollars", y = "April pay in hundred dollars")


# more visualization 
credit_c.d %>%
  ggplot(aes(x=repay_0905, y = limit_bal)) +
  geom_boxplot

credit_c.d %>%
  ggplot(aes(x= repay_0805, y = limit_bal)) +
  geom_boxplot()


credit_c.d %>%
  ggplot(aes(x= repay_0705, y = limit_bal)) +
  geom_boxplot()

credit_c.d %>%
  ggplot(aes(x= repay_0605, y = limit_bal)) +
  geom_boxplot()

credit_c.d %>%
  ggplot(aes(x= repay_0505, y = limit_bal)) +
  geom_boxplot()

credit_c.d %>%
  ggplot(aes(x= repay_0405, y = limit_bal)) +
  geom_boxplot()

# It seems that lower credit limit balance is associated with longer delayed time until repayment. 


## Performing principal component analysis
dimnames(credit_c.d)

# finding the mean and variance

lapply(credit_c.d[, -c(1, 3:12)], mean)
lapply(credit_c.d[, -c(1, 3:12)], var)

# PCA is about the variance;so, the mean should play no role. 

credit_students <- credit_c.d %>%
  filter(educ==c("grad school", "university"), marriage =="single")

# We want to standardize the variable
pca_credit <- prcomp(credit_students[, -c(1, 3:12,16:18,22:25)], scale. = T)
pca_credit 
names(pca_credit)

pca_credit$rotation

# plotting the principal component
biplot(pca_credit, scale =0, cex = 1)

# Due to the large dataset, PCA proves difficult to read. The plot is diffuclt to make sense of.

# Hence a better approach will be to use clustering.

K_means clustering
set.seed(1)
credit_students 
credit_students_km <- kmeans(credit_students[, -c(1, 3:12, 25 )], 6, nstart = 20)
credit_students_km$cluster

