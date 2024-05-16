### Machine Learning
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
# split data/get indexes x times (1), with 50/50 (0.5) split
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome # random selection
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# compare heights in males and females in our data set
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# now try predicting "male" if the height is within 2 SD of the average male
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
# map applies function to each element of list/vector and returns vector of same dimension
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


## ex 2.1.1
mnist <- read_mnist()
ncol(mnist$train$images)

### 2.1
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)


# maximize F-score 
# F score by sensitivity and specificity
# for F score weight which is more important
# better to send a save plane to check up
# rather than having one crash
# F = weighted harmonic average with
#
# F = 1/(beta^2/(1+beta^2)/recall=sensitivity + 1/(1+beta^2)/precision=specificity)
# recall=sensitivity = TP/(TP+FN); precision = TP/(TP+FP)

cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

# prevalence = häufigkeit is important if your algorithm 
# is based on very specific and low prevalence, might be hard to predict


# receiver operating characteristics (ROC) curve
# compares sensitivity (y) to 1-specificity (x)
# -> see that second approach has higher sensitivity for
# given values of specificity
# compare methods!
# but none of them uses prevalence

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

# precision(y) recall(x) plot when prevalence matters


## loss function in continuous variables
## loss function (y_i^(hat) - y_i)^2

# MSE = 1/N * sum((y_i^(hat) - y_i)^2) #mean squared error
# if there are B samples than you might want to
# minimize it across all samples b (cross validation)
# MSE = 1/B sum_b^B(1/N * sum_i^N((y_i^b(hat) - y_i^b)^2))


# exercise 2.1.2
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
# my try. gets the numbers
dat %>% group_by(type,sex) %>% summarize(n=n())
# solution
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

### for inclass female is the guess
### for online male is the guess

# my solution
y_hat <- ifelse(x == "inclass", "Female", "Male") %>%
  factor(levels = levels(y))
mean(y == y_hat)

#online solution
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

#primitive confusion matrix
table(y_hat,y)

library(caret)
sensitivity(y_hat,y)
specificity(y_hat,y)
# prevalence my solution
dat %>% filter(sex=="Female") %>% nrow()/nrow(dat)
# online solution
mean(y == "Female")

##ex 2.1.2.b
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


head(train)
library(ggplot2)
library(tidyverse)
for (n in colnames(train)[1:4]){#[1:4]
  cutoff <- seq(min(train[[n]]), max(train[[n]]),0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train[[n]] > x, "virginica", "versicolor") %>% 
      factor(levels = levels(train$Species))
    mean(y_hat == train$Species)
  }
  )
    print(ggplot(data.frame(cutoff, accuracy),aes(cutoff, accuracy)) + 
    geom_point() + 
    geom_line())
  print(max(accuracy))
  print(n)
}

#first iteration tells that in var 3 is the overall max with 0.96 accuracy
# second only until var 3 tells that the cutoff for that is 5
# for the petal.width the cutoff is 1.6

### well after setting the seed the results agree with the one from the solution online

## this is online solution
foo <- function(x){
  #range gives min and max as first and second argument
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
# apply applies the function foo onto dataframe train, along the columns
predictions <- apply(train[,-5], 2, foo)
sapply(predictions, max)	
# online it says that petal.width has the highest accuracy, but I do not get why...
# it might be on the testing data set there it is, see below 0.94 width vs 0.84 length
# get accuracy for most accurate feature in training now in test
#
y_hat <- ifelse(test$Petal.Length>5,'virginica', 'versicolor')
test_accuracy <- mean(y_hat==test$Species)
test_accuracy

y_hat <- ifelse(test$Petal.Width>1.6,'virginica', 'versicolor')
test_accuracy <- mean(y_hat==test$Species)
test_accuracy

## solution, because my answer of 0.94 is not taken as correct
predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], 'virginica', 'versicolor')
mean(y_hat==test$Species)
## well this code also yields 0.94, just what I got above. online correct value is 0.88 (?)


plot(iris, pch=21, bg=iris$Species)

#combining Petal length and width
#cutoffs are 5 and 1.6
y_hat <- ifelse(test[,3]>5 & test[,4]>1.6, 'virginica', 'versicolor')
mean(y_hat==test$Species)


##
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris, pch=21, bg=iris$Species)

set.seed(76) 
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1], range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange, function(i){
  y_hat <- ifelse(train$Petal.Length>i, 'virginica', 'versicolor')
  mean(y_hat==train$Species)
})
print(length_predictions)
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.6

width_predictions <- sapply(petalWidthRange, function(i){
  y_hat <- ifelse(train$Petal.Width>i, 'virginica', 'versicolor')
  mean(y_hat==train$Species)
})
width_predictions
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff & test$Petal.Width>width_cutoff, 'virginica', 'versicolor')
mean(y_hat==test$Species)

# ex 2.2
set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test)
mean(disease[test==0])
mean(disease[test==1]==1)

mean(test==1)/mean(disease==1)
mean(disease[test==1]==1)/mean(disease==1)


#ex 2.2
library(dslabs)
data("heights")
p <- heights %>% mutate(height=round(height)) %>% 
  group_by(height) %>%
  summarize(p=mean(sex =="Male")) %>%
  qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  #mutate(height=round(height)) %>%
  #mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE))
  mutate(g = cut(height, quantile(height, ps)),include.lowest = TRUE) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

library(MASS)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


### 
### section 3.1
###

# load the dataset
library(tidyverse)
library(dslabs)
data("mnist_27")

# explore the data by plotting the two predictors
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#x_1 is upper left quadrant
#x_2 is lower right quadrant
# y is the actual value of the number, 2 or 7

# smallest and largest values of x1 and x2
if(!exists("mnist")) mnist <- read_mnist()
# gets index of minimum and maximum value of x_1
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
## generates a list with 2 dataframes in it according to grid expand
##
#mnist$train$images is a 60'000 x 784 matrix with grey values 0-255
# selects the according grey scale values to these 2 with minimum and max x1_values from the 60'000
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
# appends the data frames saved in tmp
tmp <- Reduce(rbind, tmp)

#p1 <- plots the grey scales
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_1")

# repeat the same thing for x_2
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
#p2 <- 
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_2")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# fit the model to predict y=7 with data x_1 and x_2
fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

# build a decision rule
library(caret)
## predict the value 2/7 from the trained linear regression with the testing data now
p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# plot the true values
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# visual representation of p_hat/prediction of linear model
p_hat <- predict(fit, newdata = mnist_27$true_p)
p_hat <- scales::squish(p_hat, c(0, 1))
p1 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p2 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
gridExtra::grid.arrange(p1, p2, ncol = 2)

## exercise 3.1
library(tidyverse)
library(caret)
options(digits =3)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rsme <- replicate(100,{
  index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  train <- dat %>% slice(-index)
  test <- dat %>% slice(index)
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit,newdata=test)
  sqrt(mean((y_hat-test$y)^2))
}
)
mean(rsme)
sd(rsme)


func <- function(N){
  rsme <- replicate(N,{
    index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    train <- dat %>% slice(-index)
    test <- dat %>% slice(index)
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit,newdata=test)
    sqrt(mean((y_hat-test$y)^2))
    }
    )
  cat(mean(rsme),sd(rsme))
}
n <- c(100,500,1000,5e3,1e4)
set.seed(1)
sapply(n,func)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
rsme <- replicate(100,{
  index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  train <- dat %>% slice(-index)
  test <- dat %>% slice(index)
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit,newdata=test)
  sqrt(mean((y_hat-test$y)^2))
}
)
mean(rsme)
sd(rsme)


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
train <- dat %>% slice(-index)
test <- dat %>% slice(index)
fit_1 <- lm(y ~ x_1, data = train)
fit_2 <- lm(y ~ x_2, data = train)
fit_3 <- lm(y ~ x_1 +x_2, data = train)

y_hat <- predict(fit_1,newdata=test)
sqrt(mean((y_hat-test$y)^2))

y_hat <- predict(fit_2,newdata=test)
sqrt(mean((y_hat-test$y)^2))

y_hat <- predict(fit_3,newdata=test)
sqrt(mean((y_hat-test$y)^2))


set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
train <- dat %>% slice(-index)
test <- dat %>% slice(index)
fit_1 <- lm(y ~ x_1, data = train)
fit_2 <- lm(y ~ x_2, data = train)
fit_3 <- lm(y ~ x_1 +x_2, data = train)

y_hat <- predict(fit_1,newdata=test)
sqrt(mean((y_hat-test$y)^2))

y_hat <- predict(fit_2,newdata=test)
sqrt(mean((y_hat-test$y)^2))

y_hat <- predict(fit_3,newdata=test)
sqrt(mean((y_hat-test$y)^2))



### 3.2 smoothing
# see that the trend is wobbly
library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

# estimate the time trend in the 2008 US popular vote poll margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)


# bin smoothers
span <- 3.5
tmp <- polls_2008 %>%
  ## crossing gives all possible combinations of days in that case
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) 

tmp %>% filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = center), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~center)

# larger span
span <- 7 
## ksmooth kernel=box applys equal weight for averages
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
#kernel=normal applies weighted average by normal distribution around the center
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 |> mutate(smooth = fit$fitted) |>
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))


total_days <- diff(range(polls_2008$day))
span <- 28/total_days
# degrees fits the maximum rank of fitted polynomial, default = 2 parabolic, degree=1 makes it linear
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()


# ex 3.1
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

dat <- dat %>% mutate(nd = as.numeric(date)) %>% filter(!is.na(deaths))
span=60/nrow(dat)
fit <- loess(deaths ~ nd, degree=1, span = span, data=dat)

dat %>% mutate(smooth=fit$fitted) %>%
  ggplot(aes(date,deaths)) +
  geom_point(size = 3, alpha = 1, color = "grey") +
  geom_line(aes(date, smooth), color="red")

#solution
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")


dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

data("mnist_27")

library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

# fit the model to predict y=7 with data x_1 and x_2
fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  loess(y ~ x_2, degree=1, data = .)

y_hat <- predict(fit,newdata=mnist_27$test,type="response")
y_hat_dat <- ifelse(y_hat > 0.5, "7", "2") %>% factor(levels = levels(mnist_27$test$y))
test <- mnist_27$test %>% mutate(y = ifelse(y == 7, 1, 0)) 
confusionMatrix(y_hat_dat,mnist_27$test$y)$overall[["Accuracy"]]


## solution
train_dat <- mnist_27$train %>% 
  mutate(y1 = ifelse(y=="7", 1, 0))
loess_fit <- loess(y1 ~ x_2, degree=1, data=train_dat)

pred_val_test <- predict(loess_fit, newdata = mnist_27$test, type = "response")
y1_hat_dat <- ifelse(pred_val_test > 0.5, "7", "2") %>% factor(levels = levels(mnist_27$test$y))
confusionMatrix(y1_hat_dat, mnist_27$test$y)$overall[["Accuracy"]]


### 4.1 k-nearest neighbors
library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

knn_fit <- knn3(y ~ ., data = mnist_27$train)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]


knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)


#ex 4.1
library(dslabs)
data("heights")
library(caret)

## important that the seed is always executed with the following code, otherwise results may vary!!!
set.seed(1)
index <- createDataPartition(heights$sex,times=1,p=0.5,list=FALSE)
train <- heights[-index,]
test <- heights[index,]

n <- seq(1,101,3)
f_scores <- sapply(n,function(N){
  fit <- knn3(sex ~ height, data=train,k=N)
  y_hat <- predict(fit,test,type="class") %>%
    factor(levels=levels(test$sex))
  F_meas(data=y_hat, reference=test$sex)
  })
max(f_scores)
plot(n, f_scores)
n[which.max(f_scores)]



library(dslabs)
library(caret)
data("tissue_gene_expression")
ts <- tissue_gene_expression
set.seed(1)

x <- ts$x
y <- ts$y

index <- createDataPartition(y,list=FALSE)
xtrain <- x[-index,]
xtest <- x[index,]
ytrain <- y[-index,]
ytest <- y[index,]

n <- seq(1,11,2)
acc <- map_df(n,function(N){
  fit <- knn3(xtrain,ytrain,k=N)
  y_hat <- predict(fit,newdata=data.frame(x=xtest),
                   type="class")
  mean(y_hat == ytest)
  })


set.seed(1)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})



## 4.2
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

### k should not be selected from minimizing the error on the test set
#
accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)


### cross validation take multiple data sets and calc apparent (calculable MSE) error form it
### this multiplies computing time by k fold
### bootstrapping is such an approach
###


set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
N <- 100
X <- sample(income, N)
median(X)
m

library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + 
  geom_abline()
grid.arrange(p1, p2, ncol = 2)

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})
quantile(M_star, c(0.025, 0.975))


### ex 4.2
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

c <- 0
for (i in 1:10){
  c <- c + sum(indexes[[i]]==3)
}
c
# solution
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)
options(digits=3)

set.seed(1)
qu <- replicate(1e4,{
  y <- rnorm(100,0,1)
  quantile(y,0.75)
})
mean(qu)
sd(qu)


set.seed(1)
y <- rnorm(100, 0, 1)
qu <- replicate(1e4,{
  s <- sample(y,100,replace=TRUE)
  quantile(s,0.75)
})
mean(qu)
sd(qu)

### bootstrapping solution
set.seed(1)
y <- rnorm(100, 0, 1)

set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)


### section 5
library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")

ggplot(train_knn, highlight = TRUE)

data.frame(k = seq(9, 67, 2))

set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

ggplot(train_knn, highlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

names(train_knn_cv$results)

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])


####
####
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1



### exercise 5.1
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results


pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

###
ind <- which(pvals <= 0.01)
length(ind)
###
set.seed(1)
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

###
set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)



#### random forests
# Load tidyverse
library(tidyverse)

# load package for decision tree
library(rpart)

# load the dslabs package
library(dslabs)

# fit a classification tree using the polls_2008 dataset, 
# which contains only one predictor (day)
# and the outcome (margin)
fit <- rpart(margin ~ ., data = polls_2008)

# display the decision tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# examine the fit from the classification tree model
polls_2008 %>%  
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# fit a classification tree on the mnist data using cross validation
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
# and plot it
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

# load library for random forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


### exercise 5.1.2
library(rpart)
library(tidyverse)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat) 

plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)
  

fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")



library(dslabs)
library(rpart)
library(caret)
data("tissue_gene_expression")
dat <- tissue_gene_expression
cp <- seq(0, 0.1, 0.01)

set.seed(1991)
fit <- train(y ~ ., method = "rpart",
      tuneGrid = data.frame(cp = seq(0.0, 0.1, 0.01)),
      data = dat)

set.seed(1991) 

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

confusionMatrix(fit)    
ggplot(fit)


set.seed(1991) 
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
              control = rpart.control(minsplit = 0),
            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

confusionMatrix(fit)    
ggplot(fit)

str(fit)

set.seed(1991) 
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf",
              nodesize = 1,
              control = rpart.control(minsplit = 0),
              tuneGrid = data.frame(mtry = seq(50, 200, 25))))


ggplot(fit)


### exercise 5.2

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

tc <- titanic_clean
set.seed(42)
index <- createDataPartition(tc$Survived,times=1,p=0.2,list=FALSE)
train <- tc[-index,]
test <- tc[index,]
nrow(train)
nrow(test)
mean(train$Survived==1)

###
set.seed(3)
y_hat <- sample(c(0,1),nrow(train),replace=TRUE)
mean(y_hat == train$Survived)

###
train %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))


#ftrain <- train %>% filter(Sex=="female")
#mtrain <- train %>% filter(Sex=="male")
#mean(ftrain$Survived==1)
#mean(mtrain$Survived==1)     


###
#y_hat <- sapply(test$Sex,function(sex){
#  ifelse(sex=="female",1,0)
#})
guess_sex <- ifelse(test$Sex=="female",1,0) %>%
  factor(levels = levels(test$Survived))
guess_sex
mean(guess_sex == test$Survived)

###
train %>%
  group_by(Pclass) %>%
  summarize(Surv = mean(Survived == 1))

###
guess_class <- ifelse(test$Pclass==1,1,0) %>%
  factor(levels = levels(test$Survived))
guess_class
mean(guess_class==test$Survived)

###
train %>%
  group_by(Sex,Pclass) %>%
  summarize(Surv = mean(Survived == 1))

###
guess_sex_class <- ifelse((test$Sex=="female") & (test$Pclass %in% c(1,2)),1,0) %>%
  factor(levels = levels(test$Survived))
mean(guess_sex_class == test$Survived)

###
cm_sex <- confusionMatrix(test$Survived,guess_sex)
cm_class <- confusionMatrix(test$Survived, guess_class)
cm_sex_class <- confusionMatrix(test$Survived, guess_sex_class)


sensitivity(data = guess_sex, reference = test$Survived)
sensitivity(data = guess_class, reference = test$Survived)
sensitivity(data = guess_sex_class, reference = test$Survived)

specificity(data = guess_sex, reference = test$Survived)
specificity(data = guess_class, reference = test$Survived)
specificity(data = guess_sex_class, reference = test$Survived)

#cm_sex
#cm_class#$overall[["Accuracy"]]
#cm_sex_class#$overall[["Accuracy"]]

confusionMatrix(data = factor(guess_sex), reference = factor(test$Survived))
confusionMatrix(data = factor(guess_class), reference = factor(test$Survived))
confusionMatrix(data = factor(guess_sex_class), reference = factor(test$Survived))


###

F_meas(data = guess_sex, reference = test$Survived)
F_meas(data = guess_class, reference = test$Survived)
F_meas(data = guess_sex_class, reference = test$Survived)

###

gamloess <- train(Survived ~ Fare, method="gamLoess",data=train)
y_gamloess <- predict(gamloess,test)
mean(y_gamloess==test$Survived)

###

glm <- train(Survived ~ Age, method="glm",data=train)
y_glm <- predict(glm,test)
mean(y_glm==test$Survived)


SPAF <- train(Survived ~ Sex + Pclass + Age + Fare, method="glm",data=train)
y_SPAF <- predict(SPAF,test)
mean(y_SPAF==test$Survived)

all <- train(Survived ~ ., method="glm",data=train)
y_all <- predict(all,test)
mean(y_all==test$Survived)


###
set.seed(6)

knn <- train(Survived ~., method="knn",data=train,tuneGrid=data.frame(k = seq(3, 51, 2)))
knn$bestTune
ggplot(knn)

###
y_knn <- predict(knn,test)
mean(y_knn == test$Survived)

###
set.seed(8)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Survived ~ ., method = "knn",
                      data = train,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
names(train_knn_cv)

y_cv_knn <- predict(train_knn_cv,test)
mean(y_cv_knn==test$Survived)

####
####
set.seed(10)
tree <- train(Survived ~ .,
      method="rpart",
      tuneGrid=data.frame(cp = seq(0, 0.05, 0.002)),
      data=train)

#tree <- rpart(Survived ~ ., data = train)
tree$bestTune
y_tree <- predict(tree,test)
mean(y_tree == test$Survived)

plot(tree)
text(tree)

###
set.seed(14)
rf <- train(Survived ~.,
            method="rf",
            tuneGrid=data.frame(mtry=seq(1:7)),
            ntree=100,
            data=train
            )

rf$bestTune
y_rf <- predict(rf,test)
mean(y_rf==test$Survived)


### Section 6