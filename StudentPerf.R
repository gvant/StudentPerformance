library(randomForest)

# importing data
studentperformance <- read.csv("R/StudentsPerformance.csv")

# putting together aggregate score
studentperformance$agg <- studentperformance$math.score + studentperformance$reading.score + studentperformance$writing.score

# splitting the data
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(studentperformance), 
                 replace=TRUE, prob=c(0.8,0.2))
train<- studentperformance[sample,]
test<- studentperformance[!sample,]

# random forest
# we are using random forest because there is no assumption about the data or its scale, so it serves as a jumping off point to compare
# other analyses
rf.reg <- randomForest(agg ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course, data=train,
                       ntree=150, maxnodes=30)

# most important factors
print(importance(rf.reg, type=2))

# interesting to note that the three highest factors *do* include things which scale with financial status.

# accuracy
preds <- predict(rf.reg, newdata=test)

accuracy05<- ifelse(abs(test$agg-preds)<0.05*test$agg,1,0) 
accuracy15<- ifelse(abs(test$agg-preds)<0.15*test$agg,1,0)
accuracy25<- ifelse(abs(test$agg-preds)<0.25*test$agg,1,0)
print(accuracy05<- mean(accuracy05))
print(accuracy15<- mean(accuracy15))
print(accuracy25<- mean(accuracy25))

rf.acc <- t(round(data.frame(accuracy05, accuracy15, accuracy25), 3))
rf.acc

# linear reg

sp_lr <- studentperformance
colnames(sp_lr) <- c("gender", "race", "ped", "lunch", "prep", "math", "reading", "writing", "total")
sp_lr$gender <- as.factor(sp_lr$gender)
sp_lr$race <- as.factor(sp_lr$race)
sp_lr$ped <- as.factor(sp_lr$ped)
sp_lr$lunch <- as.factor(sp_lr$lunch)
sp_lr$prep <- as.factor(sp_lr$prep)

summary(sp_lr)

# default
# male, group A, some high school, standard, none
# setting up categories

sp_lr$gender.fm <- ifelse(sp_lr$gender == "male", 0, 1)
sp_lr$race.B <- ifelse(sp_lr$race == "group B", 1, 0)
sp_lr$race.C <- ifelse(sp_lr$race == "group C", 1, 0)
sp_lr$race.D <- ifelse(sp_lr$race == "group D", 1, 0)
sp_lr$race.E <- ifelse(sp_lr$race == "group E", 1, 0)
sp_lr$ped.h <- ifelse(sp_lr$ped == "high school", 1, 0)
sp_lr$ped.sc <- ifelse(sp_lr$ped == "some college", 1, 0)
sp_lr$ped.a <- ifelse(sp_lr$ped == "associate's degree", 1, 0)
sp_lr$ped.b <- ifelse(sp_lr$ped == "bachelor's degree", 1, 0)
sp_lr$ped.m <- ifelse(sp_lr$ped == "master's degree", 1, 0)
sp_lr$lunch.s <- ifelse(sp_lr$lunch == "standard", 0, 1)
sp_lr$prep.n <-  ifelse(sp_lr$prep == "none", 0, 1)

# splitting the data
set.seed(42)
sample <- sample(c(TRUE, FALSE), nrow(sp_lr), 
                 replace=TRUE, prob=c(0.8,0.2))
train<- sp_lr[sample,]
test<- sp_lr[!sample,]

glm.sp <- lm(total ~ gender.fm + race.B + race.C + race.D + race.E + ped.h + ped.sc + ped.a + ped.b + ped.m + lunch.s + prep.n, data=train)

# accuracy
preds <- predict(glm.sp, newdata=test)

accuracy05<- ifelse(abs(test$total-preds)<0.05*test$total,1,0) 
accuracy15<- ifelse(abs(test$total-preds)<0.15*test$total,1,0)
accuracy25<- ifelse(abs(test$total-preds)<0.25*test$total,1,0)
accuracy05<- mean(accuracy05)
accuracy15<- mean(accuracy15)
accuracy25<- mean(accuracy25)
glm.acc <- t(round(data.frame(accuracy05, accuracy15, accuracy25), 3))


rf.acc
glm.acc

# glm outperforms for 15 and 25 % margins.

summary(glm.sp)

# gendered effect (female) of +13 points compared to male

# races D and E found to be significant in terms of point increases (compared to group A)

# compared to "some high school", significant effects were bachelor's and master's degree which led to a 20 and 24 point increase on average

# having free / reduced lunch had a 23 point decreasing effect
# having completed the test prep is another 23 point increase.

# combined effect of master's degree parent, non-reduced lunch, and course preparation gives a total (24+23+23) = 70 point increase
# standard deviation is 42.77 for aggregate scores, so these effects could push students 1.64 standard deviations ahead of others. 
sd(studentperformance$agg)