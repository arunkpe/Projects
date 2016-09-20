##############################################################################
#                                                                            #
#                         HomeWork #2                                        #
#                                                                            #
##############################################################################


######Problem 1c
#y =1 -> x>5; y=0 -> x<5
y <- c(0,0,0,0,0,0,1,1,1,1,1,1)  
x <- c(1,1,2,3,4,4,6,6,7,7,8,9)
logit <- glm(y~ x, family=binomial)
summary(logit)
ypred = predict(logit, as.data.frame(x), type="response")
plot(x,y)
plot(x,ypred)

### 1. Loading the Data

games <- read.csv("http://www.stanford.edu/~wfithian/games.csv",as.is=TRUE)
teams <- read.csv("http://www.stanford.edu/~wfithian/teams.csv",as.is=TRUE)

head(games)
head(teams)

all.teams <- sort(unique(c(teams$team,games$home,games$away)))


games$home <- factor(games$home, levels = all.teams)
games$away <- factor(games$away, levels = all.teams)
teams$team <- factor(teams$team, levels = all.teams)
teams$conference <- factor(teams$conference)

total.margin <- function(team) {
  with(games,
       sum(homeScore[home==team]) 
       + sum(awayScore[away==team]) 
       - sum(homeScore[away==team])  
       - sum(awayScore[home==team]))
}

margins <- sapply(teams$team, total.margin)
names(margins) <- teams$team

rank.table <- cbind("Margin"      = margins,
                    "Margin Rank" = rank(-margins,ties="min"),
                    "AP Rank"     = teams$apRank,
                    "USAT Rank"   = teams$usaTodayRank)

margin.top25 <- order(margins,decreasing=TRUE)[1:25]
rank.table[margin.top25,]



# Linear Regression for Ranking team

y <- with(games, homeScore - awayScore)

X0 <- as.data.frame(matrix(0, nrow(games), length(all.teams)))
names(X0) <- all.teams
for (tm in all.teams) {
  X0[[tm]] <- 1 * (games$home == tm) - 1 * (games$away == tm)
}


### 4. An Identifiability Problem

X <- X0[,names(X0) != "stanford-cardinal"]
reg.season.games <- which(games$gameType=="REG")

mod <- lm(y ~ 0 + ., data=X, subset=reg.season.games)
head(coef(summary(mod)))

summary(mod)$r.squared


### 5. Interpreting the Model

coef(mod)["`alabama-crimson-tide`"] - coef(mod)["`air-force-falcons`"]


### 6. Home Court Advantage - Linear Model

homeAdv <- 1 - games$neutralLocation
homeAdv.mod <- lm(y ~ 0 + homeAdv + ., data=X, subset=reg.season.games)
homeAdv.coef <- coef(homeAdv.mod)[paste("`",teams$team,"`",sep="")]
names(homeAdv.coef) <- teams$team

head(coef(summary(homeAdv.mod)))

homeAdv.estimate <- coef(homeAdv.mod)["homeAdv"]
homeAdv.se <- coef(summary(homeAdv.mod))["homeAdv",2]
(homeAdv.CI <- c("CI Lower"=homeAdv.estimate - 2*homeAdv.se,
                 "CI Upper"=homeAdv.estimate + 2*homeAdv.se))


### 7. Linear Model Rankings Vs. Official Rankings

rank.table <- cbind("Model Score" = homeAdv.coef,
                    "Model Rank"  = rank(-homeAdv.coef,ties="min"),
                    "AP Rank"     = teams$apRank,
                    "USAT Rank"   = teams$usaTodayRank)
rank.table[order(homeAdv.coef,decreasing=TRUE)[1:25],]



### PROBLEM 4a Home Court Advantage - Logit Model
logitY <- ifelse(y > 0, 1, 0)
homeAdv.mod.logit <- glm(logitY ~ 0 + homeAdv + ., data = X0, subset = reg.season.games, 
                         family = binomial)
homeAdv.coef.logit <- coef(homeAdv.mod.logit)[paste("`", all.teams, "`", sep = "")]
names(homeAdv.coef.logit) <- all.teams

head(coef(summary(homeAdv.mod.logit)))

### Logit Model Rankings Vs. Official Rankings
rank.table.logit <- cbind(
                          "Logit Model Score" = homeAdv.coef.logit,
                          "LogitRank"         = rank(-homeAdv.coef.logit,ties="min"))
rank.table.logit[order(homeAdv.coef.logit,decreasing=TRUE)[1:25],]

rank.table.linear <- cbind(
                          "Linear Model Score"  = homeAdv.coef,
                          "LinearRank"          = rank(-homeAdv.coef,ties="min"),
                          "APRank"              = teams$apRank,
                          "USATRank"            = teams$usaTodayRank
                          )

rank.table.compare <- merge(rank.table.logit,rank.table.linear,all.x=TRUE,by ="row.names")
rank.table.compare[order(homeAdv.coef.logit,decreasing=TRUE)[1:25],]

cor(rank.table.compare$APRank, rank.table.compare$USATRank, use="pairwise.complete.obs")
cor(rank.table.compare$APRank, rank.table.compare$LinearRank, use="pairwise.complete.obs")
cor(rank.table.compare$APRank, rank.table.compare$LogitRank, use="pairwise.complete.obs")
cor(rank.table.compare$USATRank, rank.table.compare$LinearRank, use="pairwise.complete.obs")
cor(rank.table.compare$USATRank, rank.table.compare$LogitRank, use="pairwise.complete.obs")

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

plot(rank.table.compare$APRank, rank.table.compare$USATRank,xlab="USAT, Model Rankings Compared", ylab="AP Rank")
points(rank.table.compare$APRank, rank.table.compare$LinearRank, col="green",pch = 2)
points(rank.table.compare$APRank, rank.table.compare$LogitRank, col="red",pch = 3)
add_legend("topright",legend= c("usat","linear","logit"),col = c("black","green","red"),pch = c(1,2,3), horiz=TRUE, bty='n', cex=0.8)
       

plot(rank.table.compare$USATRank, rank.table.compare$APRank,xlab="AP, Model Rankings Compared", ylab="USAT Rank")
points(rank.table.compare$USATRank, rank.table.compare$LinearRank, col="green",pch = 2)
points(rank.table.compare$USATRank, rank.table.compare$LogitRank, col="red",pch = 3)
add_legend("topright",legend= c("AP","linear","logit"),col = c("black","green","red"),pch = c(1,2,3), horiz=TRUE, bty='n', cex=0.8)


### PROBLEM 4b Repeat by excluding teams with < 5 games
#Sum of home and away games = total game count; create a vector
gameCount <- vector()
for (i in 1:length(all.teams)) {
  gameCount[i] <- length(which(games$home == all.teams[i])) + length(which(games$away == all.teams[i]))
}
names(gameCount) <- all.teams


## Recalculate matrix
teams.5 <- all.teams[-which(gameCount < 5)]
X0.ex5 <- as.data.frame(matrix(0, nrow(games), length(teams.5)))
names(X0.ex5) <- teams.5

## Fill in the columns, one by one
for (tm in teams.5) {
  X0.ex5[[tm]] <- 1 * (games$home == tm) - 1 * (games$away == tm)
}

## Stanford is the baseline.
X.ex5 <- X0.ex5[, names(X0.ex5) != "stanford-cardinal"]

linearY.ex5 <- with(games, homeScore - awayScore)
logitY.ex5 <- ifelse(y > 0, 1, 0)

## Model including home advantage
homeAdv <- 1 - games$neutralLocation
homeAdv.mod.lm.ex5   <- lm(linearY.ex5 ~ 0 + homeAdv + ., data = X.ex5, subset = reg.season.games)
homeAdv.mod.logit.ex5 <- glm(logitY.ex5 ~ 0 + homeAdv + ., family = binomial, data = X.ex5, 
                         subset = reg.season.games)

## Calculate Rank.
homeAdv.coef.lm.ex5 <- coef(homeAdv.mod.lm.ex5)[paste("`", teams$team, "`", sep = "")]
names(homeAdv.coef.lm.ex5) <- teams$team

homeAdv.coef.logit.ex5 <- coef(homeAdv.mod.logit.ex5)[paste("`", teams$team, "`", sep = "")]
names(homeAdv.coef.logit.ex5) <- teams$team

## Check.
head(coef(summary(homeAdv.mod.lm.ex5)))
head(coef(summary(homeAdv.mod.logit.ex5)))



## Build rank table.
rank.table.ex5 <- cbind(
                      'Linear Score'          = homeAdv.coef.lm.ex5, 
                      'LinearRank'            = rank(-homeAdv.coef.lm.ex5, ties = "min"),
                      'Logistic Score'        = homeAdv.coef.logit.ex5, 
                      'LogitRank'             = rank(-homeAdv.coef.logit.ex5, ties = "min"), 
                      'APRank'                = teams$apRank, 
                      'USATRank'              = teams$usaTodayRank)
rank.table.ex5[order(homeAdv.coef.logit.ex5, decreasing = TRUE)[1:25], ]


cor(rank.table.ex5[,'APRank'], rank.table.ex5[,'USATRank'], use="pairwise.complete.obs")
cor(rank.table.ex5[,'APRank'], rank.table.ex5[,'LinearRank'], use="pairwise.complete.obs")
cor(rank.table.ex5[,'APRank'], rank.table.ex5[,'LogitRank'], use="pairwise.complete.obs")
cor(rank.table.ex5[,'USATRank'], rank.table.ex5[,'LinearRank'], use="pairwise.complete.obs")
cor(rank.table.ex5[,'USATRank'], rank.table.ex5[,'LogitRank'], use="pairwise.complete.obs")


plot(rank.table.ex5[,'APRank'], rank.table.ex5[,'USATRank'],xlab="USAT, Model Rankings Compared", ylab="AP Rank")
points(rank.table.ex5[,'APRank'], rank.table.ex5[,'LinearRank'], col="green",pch = 2)
points(rank.table.ex5[,'APRank'], rank.table.ex5[,'LogitRank'], col="red",pch = 3)
add_legend("topright",legend= c("usat","linear","logit"),col = c("black","green","red"),pch = c(1,2,3), horiz=TRUE, bty='n', cex=0.8)


plot(rank.table.ex5[,'USATRank'], rank.table.ex5[,'APRank'],xlab="AP, Model Rankings Compared", ylab="USAT Rank")
points(rank.table.ex5[,'USATRank'], rank.table.ex5[,'LinearRank'], col="green",pch = 2)
points(rank.table.ex5[,'USATRank'], rank.table.ex5[,'LogitRank'], col="red",pch = 3)
add_legend("topright",legend= c("AP","linear","logit"),col = c("black","green","red"),pch = c(1,2,3), horiz=TRUE, bty='n', cex=0.8)


#Problem 4c

z = summary(homeAdv.mod)
z = z$coefficients
n = nrow(z)
Pr = z[2:n,4]
Coef = z[2:n,1]
t = Pr < 0.05 & Coef > 0
sum(t)
frac.linear <- sum(t)/nrow(rank.table.linear)
frac.linear

#Repeat for Logistic Model
z = summary(homeAdv.mod.logit); 
z = z$coefficients;
n = nrow(z);
z = z[2:n,];
Pr = z[,4];
stanCoef = z[grep("stanford", rownames(z)), 1]
Coef = z[,1];
t = Pr < 0.05 & Coef > stanCoef;
sum(t)
frac.logit <- sum(t)/nrow(rank.table.logit)
frac.logit



#Problem 4d 10 fold cross validation

#Read the data - essentially what we did earlier in problem 4a-c
setwd("C:/Users/Arun/Desktop/Stats216")
games<-read.csv("games.csv", as.is=TRUE)
teams<-read.csv("teams.csv", as.is=TRUE)
all.teams = sort(unique(c(teams$team,games$home,games$away)))
total.margin <- function(team) 
{
  with(games,
       sum(homeScore[home==team]) 
       + sum(awayScore[away==team]) 
       - sum(homeScore[away==team])  
       - sum(awayScore[home==team]))
}
margins <- sapply(all.teams, total.margin)
names(margins) <- all.teams

num.games <- function(team)
{
  with(games, sum(home==team) + sum(away==team))
}

X0 <- as.data.frame(matrix(0, nrow(games), length(all.teams)))
names(X0) <- all.teams
for (tm in all.teams) {
  X0[[tm]] <- 1 * (games$home == tm) - 1 * (games$away == tm)
}
margins = games$homeScore - games$awayScore
ylin = margins
#Baseline - Stanford
X <- X0[,names(X0) != "stanford-cardinal"]
homeAdv <- 1 - games$neutralLocation
reg.season.games <- games$gameType=="REG"

# Choose only the regular games as before
ylin = subset(ylin, reg.season.games)
X = X[reg.season.games,]
homeAdv = subset(homeAdv, reg.season.games)
margins = subset(margins, reg.season.games)

actualResult = margins
actualResult[actualResult < 0] = 0
actualResult[actualResult > 0] = 1

# 10 fold Cross Validation of the linear response ylin
ylin = subset(ylin, reg.season.games)
k = 10
list <- 1:k
#Initialize this once - this will help to randomly partition the dataset into 10 segments
set.seed(18)
id <- sample(1:k,nrow(X),replace=TRUE) 

#Initialize the preducted result to -1; we can use this as a check to see if the data is partitioned correctly
predictResultLinear=actualResult
predictResultLinear[1:length(ylin)]=-1
for (i in 1:k)
{
  #Train data on 9 folds
  subsetID <- (id!=i)
  fit <- lm(ylin ~ 0 + homeAdv + ., data=X,subset = subsetID)
  # Linear model prediction
  ypred <- predict(fit, newdata=X, type="response")
  yCV = ypred
  yCV[yCV<0] = 0; # Loss
  yCV[yCV>0] = 1; # Win
  #Use the results for the 1 fold - test set
  for (j in 1:nrow(X))
  {
    if(i ==id[j] & predictResultLinear[j]==-1){
      predictResultLinear[j] = yCV[j]
    } 
    
  }
}


# logit model cross validation
ylogit = actualResult
predictResultLogit=actualResult
predictResultLogit[1:length(ylin)]=-1
for (i in 1:10)
{
  # Train model on 9 segments
  subsetID <- (id!=i)
  fit <- glm(ylogit ~ 0 + homeAdv + ., data=X, subset=subsetID, family=binomial)
  # predict logit model
  ypred = predict(fit, newdata=X, type="response")
  yCV = ypred
  yCV[yCV<0.5] = 0  # Loss in probability of loss is <50%
  yCV[yCV>=0.5] = 1 # Win
  
  #Use the results for the 1 fold - test set
  for (j in 1:nrow(X))
  {
    if(i ==id[j] & predictResultLogit[j]==-1){
      predictResultLogit[j] = yCV[j]
    }     
  }
}


n11 = sum(actualResult==predictResultLinear & actualResult==predictResultLogit)
n12 = sum(actualResult==predictResultLinear & actualResult!=predictResultLogit)
n21 = sum(actualResult!=predictResultLinear & actualResult==predictResultLogit)
n22 = sum(actualResult!=predictResultLinear & actualResult!=predictResultLogit)
nMatrix = c(n11,n21,n12,n22)
dim(nMatrix) = c(2,2)
nMatrix <- as.data.frame(nMatrix)
colnames(nMatrix) <- c('logit  right','logit  wrong')
rownames(nMatrix) <- c('linear right','linear wrong')