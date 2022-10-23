#install.packages("ggstatsplot")
#install.packages("aplpack")

setwd("E:/2D/Final/Prob_Stat/simpleAnalysis01/probStat_simpleAnalysis")
dat<-read.csv("water_potability.csv",header=T)
dataRm_Na <- na.omit(dat)
data_Potability_0 <- subset(dataRm_Na, Potability == 0)
data_Potability_1 <- subset(dataRm_Na, Potability == 1)
data_rm <- subset(dataRm_Na, Turbidity<=5)

#Initail col data
ph_Pot_0 <- data_Potability_0$ph
turbidity_Pot_0 <- data_Potability_0$Turbidity
ph_Pot_1 <- data_Potability_1$ph
turbidity_Pot_1 <- data_Potability_1$Turbidity



#### POTABILITY == 0 ####


#Initial Variable
meanPH_Pot0 <- mean(ph_Pot_0)
sdPH_Pot0 <- sd(ph_Pot_0)
meanTurb_Pot0 <- mean(turbidity_Pot_0)
sdTurb_Pot0 <- sd(turbidity_Pot_0)

# fundamental statistical : Min,Max,1st and 3rd Quantile, Median, Mean
summary(data_Potability_0)

#Mode finder function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the Mode finder function.
#pH
resultpH_Pot_0 <- getmode(ph_Pot_0)
if(is.null(resultpH_Pot_0)){
  cat("pH Mode : NULL")
}else{
  cat("pH Mode : ", resultpH_Pot_0)
}
#Turbidity
resultTurbidity_Pot_0 <- getmode(turbidity_Pot_0)
if(is.null(resultTurbidity_Pot_0)){
  cat("Turbidity Mode : NULL")
}else{
  cat("Turbidity Mode : ", resultTurbidity_Pot_0, " NTU")
}

#Quartile
#pH
cat("Quartile of pH")
quantile(ph_Pot_0)
#Turbudity
cat("Quartile of Turbudity")
quantile(turbidity_Pot_0)

#Percentile
#ph
cat("Percentile of pH")
quantile(ph_Pot_0,c(0,.25,.50,.75,1))
#Turbidity
cat("Percentile of Turbudity")
quantile(turbidity_Pot_0,c(0,.25,.50,.75,1))

#Range
#ph
getRangeUser <- function(a){
  maxA <- max(a)
  minA <- min(a)
  rangeA <- maxA - minA
  mList <- list(maxA,minA,rangeA)
  names(mList) <- c("Max","Min","Range")
  return(mList)
}
wordListForRange <- list("Max", "Min", "Range")
range_pH_Pot_0 <- getRangeUser(ph_Pot_0)
sprintf("%s pH : %.8f", wordListForRange[1], range_pH_Pot_0[1])
sprintf("%s pH : %.8f", wordListForRange[2], range_pH_Pot_0[2])
sprintf("%s pH : %.8f", wordListForRange[3], range_pH_Pot_0[3])
#Turbidity
range_Turbidity_Pot_0 <- getRangeUser(turbidity_Pot_0)
sprintf("%s Turbidity : %.8f NTU", wordListForRange[1], range_Turbidity_Pot_0[1])
sprintf("%s Turbidity : %.8f NTU", wordListForRange[2], range_Turbidity_Pot_0[2])
sprintf("%s Turbidity : %.8f NTU", wordListForRange[3], range_Turbidity_Pot_0[3])

#InterQuartile
#ph
interQ_PH_Pot_0 <- IQR(ph_Pot_0)
cat("InterQuartile of pH : " ,interQ_PH_Pot_0)
#Turbidity
interQ_Turb_Pot_0 <- IQR(turbidity_Pot_0)
cat("InterQuartile of Turbidity : " ,interQ_Turb_Pot_0, " NTU")

#Standard Deviation
#pH
std_PH_Pot_0 <- sd(ph_Pot_0)
cat("Standard Diviation of pH : " ,std_PH_Pot_0)
#Turbidity
std_Turb_Pot_0 <- sd(turbidity_Pot_0)
cat("Standard Diviation of Turbidity : " ,std_Turb_Pot_0, " NTU")

#Variance Deviation
#pH
var_PH_Pot_0 <- var(ph_Pot_0)
cat("Variance of pH : " ,var_PH_Pot_0)
#Turbidity
var_Turb_Pot_0 <- var(turbidity_Pot_0)
cat("Variance of Turbidity : " ,var_Turb_Pot_0, " NTU") 

#Histrogram
#pH
hist(ph_Pot_0,
     main = "pH of The Non-Drinkable Water",
     xlab = "pH",
     ylab = "Frequency (Samples)",
     col = "orange",
     border = "brown"
     )
#Turbidity
hist(turbidity_Pot_0,
     main = "Turbidity of The Non-Drinkable Water",
     xlab = "Turbidity(NTU)",
     ylab = "Frequency (Samples)",
     col = "orange",
     border = "brown"
)

#stem & leaf
#ph
stem(ph_Pot_0,width = 400)
#Turbidity
stem(turbidity_Pot_0)

#ScatterPlot
plot(ph_Pot_0,turbidity_Pot_0,
               main = "pH and Turbidity of Non-Drinkable Water",
               xlab = "pH",
               ylab = "Turbidity(NTU)",
               col = "orange",
               border = "brown"
               )

#ScatterPlot
#scatter.smooth(dataRm_Na$Turbidity, dataRm_Na$Potability,
#               main = "pH and Turbidity of Water",
#               xlab = "Turbidity(NTU)",
#               ylab = "pH",
#               col = "brown",
#               border = "black"
#)

#Boxplot
#before remove Outlier
#ph
boxplot(ph_Pot_0,
        main = "pH of The Non-Drinkable Water",
        ylab = "pH",
        col = "orange",
        border = "brown",
        notch = FALSE,
        horizontal = TRUE
        )
#Turbidity
boxplot(turbidity_Pot_0,
        main = "Turbidity of The Non-Drinkable Water",
        xlab = "NTU",
        ylab = "Turbidity",
        col = "orange",
        border = "brown",
        notch = FALSE,
        horizontal = TRUE
        )

#Remove outlier method
#pH
q1_pH_Pot_0 <- quantile(ph_Pot_0, .25)
q3_pH_Pot_0 <- quantile(ph_Pot_0, .75)
iqr_pH_Pot_0 <- IQR(ph_Pot_0)
no_outliers_pH_Pot_0 <- subset(data_Potability_0$ph, ph_Pot_0 > (q1_pH_Pot_0 - 1.5*iqr_pH_Pot_0) & ph_Pot_0 < (q3_pH_Pot_0 + 1.5*iqr_pH_Pot_0))
#Turbidity
q1_Turb_Pot_0 <- quantile(turbidity_Pot_0, .25)
q3_Turb_Pot_0 <- quantile(turbidity_Pot_0, .75)
iqr_Turb_Pot_0 <- IQR(turbidity_Pot_0)
no_outliers_Turb_Pot_0 <- subset(data_Potability_0$Turbidity, turbidity_Pot_0 > (q1_Turb_Pot_0 - 1.5*iqr_Turb_Pot_0) & turbidity_Pot_0 < (q3_Turb_Pot_0 + 1.5*iqr_Turb_Pot_0))

#Boxplot
#after remove Outlier
#ph
boxplot(no_outliers_pH_Pot_0,
        main = "pH of The Non-Drinkable Water",
        ylab = "pH",
        col = "orange",
        border = "brown",
        notch = FALSE,
        horizontal = TRUE
)
#Turbidity
boxplot(no_outliers_Turb_Pot_0,
        main = "Turbidity of The Non-Drinkable Water",
        xlab = "NTU",
        ylab = "Turbidity",
        col = "orange",
        border = "brown",
        notch = FALSE,
        horizontal = TRUE
)


#Prob density func.
dens_pH <- density(ph_Pot_0)
plot(dens_pH,
     main = "Probability Density Function of pH",
     xlab = "pH",
     ylab = "Density",
)
polygon(dens_pH, col = "orange", border = "brown")
#Turbidity
dens_Turb <- density(turbidity_Pot_0)
plot(dens_Turb,
     main = "Probability Density Function of Turbidity",
     xlab = "Turbidity (NTU)",
     ylab = "Density",
)
polygon(dens_Turb, col = "orange", border = "brown")

#Cumulative distribution function
#pH
cdf_pH_Pot_0 <- ecdf(ph_Pot_0)
plot(cdf_pH_Pot_0,
     main = "Cumulative Distribution Function of pH",
     xlab = "pH",
     ylab = "Density",
)
#Turbidity
cdf_Turb_Pot_0 <- ecdf(turbidity_Pot_0)
plot(cdf_Turb_Pot_0,
     main = "Cumulative Distribution Function of Turbidity",
     xlab = "Turbidity (NTU)",
     ylab = "Density",
)


#### POTABILITY == 1 ####

#Initial Variable
meanPH_Pot1 <- mean(ph_Pot_1)
sdPH_Pot1 <- sd(ph_Pot_1)
meanTurb_Pot1 <- mean(turbidity_Pot_1)
sdTurb_Pot1 <- sd(turbidity_Pot_1)

# fundamental statistical : Min,Max,1st and 3rd Quantile, Median, Mean
summary(data_Potability_1)

#Mode finder function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode using the Mode finder function.
#pH
resultpH_Pot_1 <- getmode(ph_Pot_1)
if(is.null(resultpH_Pot_1)){
  cat("pH Mode : NULL")
}else{
  cat("pH Mode : ", resultpH_Pot_1)
}
#Turbidity
resultTurbidity_Pot_1 <- getmode(turbidity_Pot_1)
if(is.null(resultTurbidity_Pot_1)){
  cat("Turbidity Mode : NULL")
}else{
  cat("Turbidity Mode : ", resultTurbidity_Pot_1, " NTU")
}

#Quartile
#pH
cat("Quartile of pH")
quantile(ph_Pot_1)
#Turbudity
cat("Quartile of Turbudity")
quantile(turbidity_Pot_1)

#Percentile
#ph
cat("Percentile of pH")
quantile(ph_Pot_1,c(0,.25,.50,.75,1))
#Turbidity
cat("Percentile of Turbudity")
quantile(turbidity_Pot_1,c(0,.25,.50,.75,1))

#Range
#ph
getRangeUser <- function(a){
  maxA <- max(a)
  minA <- min(a)
  rangeA <- maxA - minA
  mList <- list(maxA,minA,rangeA)
  names(mList) <- c("Max","Min","Range")
  return(mList)
}
wordListForRange <- list("Max", "Min", "Range")
range_pH_Pot_1 <- getRangeUser(ph_Pot_1)
sprintf("%s pH : %.8f", wordListForRange[1], range_pH_Pot_1[1])
sprintf("%s pH : %.8f", wordListForRange[2], range_pH_Pot_1[2])
sprintf("%s pH : %.8f", wordListForRange[3], range_pH_Pot_1[3])
#Turbidity
range_Turbidity_Pot_1 <- getRangeUser(turbidity_Pot_1)
sprintf("%s Turbidity : %.8f NTU", wordListForRange[1], range_Turbidity_Pot_1[1])
sprintf("%s Turbidity : %.8f NTU", wordListForRange[2], range_Turbidity_Pot_1[2])
sprintf("%s Turbidity : %.8f NTU", wordListForRange[3], range_Turbidity_Pot_1[3])

#InterQuartile
#ph
interQ_PH_Pot_1 <- IQR(ph_Pot_1)
cat("InterQuartile of pH : " ,interQ_PH_Pot_1)
#Turbidity
interQ_Turb_Pot_1 <- IQR(turbidity_Pot_1)
cat("InterQuartile of Turbidity : " ,interQ_Turb_Pot_1, " NTU")

#Standard Deviation
#pH
std_PH_Pot_1 <- sd(ph_Pot_1)
cat("Standard Diviation of pH : " ,std_PH_Pot_1)
#Turbidity
std_Turb_Pot_1 <- sd(turbidity_Pot_1)
cat("Standard Diviation of Turbidity : " ,std_Turb_Pot_1, " NTU")

#Variance Deviation
#pH
var_PH_Pot_1 <- var(ph_Pot_1)
cat("Variance of pH : " ,var_PH_Pot_1)
#Turbidity
var_Turb_Pot_1 <- var(turbidity_Pot_1)
cat("Variance of Turbidity : " ,var_Turb_Pot_1, " NTU") 

#Histrogram
#pH
hist(ph_Pot_1,
     main = "pH of The Drinkable Water",
     xlab = "pH",
     ylab = "Frequency (Samples)",
     col = rgb(.182,.66,.23),
     border = rgb(.37,.119,.232)
)
#Turbidity
hist(turbidity_Pot_1,
     main = "Turbidity of The Drinkable Water",
     xlab = "Turbidity(NTU)",
     ylab = "Frequency (Samples)",
     col = rgb(.182,.66,.23),
     border = rgb(.37,.119,.232)
)

#stem & leaf
#ph
stem(ph_Pot_1,width = 400)
#Turbidity
stem(turbidity_Pot_1)

#ScatterPlot
plot(ph_Pot_1,turbidity_Pot_1,
     main = "pH and Turbidity of Drinkable Water",
     xlab = "pH",
     ylab = "Turbidity(NTU)",
     col = rgb(.182,.66,.23),
     border = rgb(.37,.119,.232)
)

#ScatterPlot
#scatter.smooth(dataRm_Na$Turbidity, dataRm_Na$Potability,
#               main = "pH and Turbidity of Water",
#               xlab = "Turbidity(NTU)",
#               ylab = "pH",
#               col = "brown",
#               border = "black"
#)

#Boxplot
#before remove Outlier
#ph
boxplot(ph_Pot_1,
        main = "pH of The Drinkable Water",
        ylab = "pH",
        col = rgb(.182,.66,.23),
        border = rgb(.37,.119,.232),
        notch = FALSE,
        horizontal = TRUE
)
#Turbidity
boxplot(turbidity_Pot_1,
        main = "Turbidity of The Drinkable Water",
        xlab = "NTU",
        ylab = "Turbidity",
        col = rgb(.182,.66,.23),
        border = rgb(.37,.119,.232),
        notch = FALSE,
        horizontal = TRUE
)

#Remove outlier method
#pH
q1_pH_Pot_1 <- quantile(ph_Pot_1, .25)
q3_pH_Pot_1 <- quantile(ph_Pot_1, .75)
iqr_pH_Pot_1 <- IQR(ph_Pot_1)
no_outliers_pH_Pot_1 <- subset(data_Potability_1$ph, ph_Pot_1 > (q1_pH_Pot_1 - 1.5*iqr_pH_Pot_1) & ph_Pot_1 < (q3_pH_Pot_1 + 1.5*iqr_pH_Pot_1))
#Turbidity
q1_Turb_Pot_1 <- quantile(turbidity_Pot_1, .25)
q3_Turb_Pot_1 <- quantile(turbidity_Pot_1, .75)
iqr_Turb_Pot_1 <- IQR(turbidity_Pot_1)
no_outliers_Turb_Pot_1 <- subset(data_Potability_1$Turbidity, turbidity_Pot_1 > (q1_Turb_Pot_1 - 1.5*iqr_Turb_Pot_1) & turbidity_Pot_1 < (q3_Turb_Pot_1 + 1.5*iqr_Turb_Pot_1))

#Boxplot
#after remove Outlier
#ph
boxplot(no_outliers_pH_Pot_1,
        main = "pH of The Drinkable Water",
        ylab = "pH",
        col = rgb(.182,.66,.23),
        border = rgb(.37,.119,.232),
        notch = FALSE,
        horizontal = TRUE
)
#Turbidity
boxplot(no_outliers_Turb_Pot_1,
        main = "Turbidity of The Drinkable Water",
        xlab = "NTU",
        ylab = "Turbidity",
        col = rgb(.182,.66,.23),
        border = rgb(.37,.119,.232),
        notch = FALSE,
        horizontal = TRUE
)


#Prob density func.
dens_pH_Pot_1 <- density(ph_Pot_1)
plot(dens_pH_Pot_1,
     main = "Probability Density Function of pH",
     xlab = "pH",
     ylab = "Density"
)
polygon(dens_pH_Pot_1, col = rgb(.182,.66,.23), border = rgb(.37,.119,.232))
#Turbidity
dens_Turb_Pot_1 <- density(turbidity_Pot_1)
plot(dens_Turb_Pot_1,
     main = "Probability Density Function of Turbidity",
     xlab = "Turbidity (NTU)",
     ylab = "Density"
)
polygon(dens_Turb_Pot_1, col = rgb(.182,.66,.23), border = rgb(.37,.119,.232))

#Cumulative distribution function
#pH
cdf_pH_Pot_1 <- ecdf(ph_Pot_1)
plot(cdf_pH_Pot_1,
     main = "Cumulative Distribution Function of pH",
     xlab = "pH",
     ylab = "Density"
)
#Turbidity
cdf_Turb_Pot_1 <- ecdf(turbidity_Pot_1)
plot(cdf_Turb_Pot_1,
     main = "Cumulative Distribution Function of Turbidity",
     xlab = "Turbidity (NTU)",
     ylab = "Density"
)




#### Confidence Interval
getCI <- function(cl,n,x){
  m <- mean(x)
  s <-sd(x)
  se<- s/sqrt(n)
  z <- qnorm(cl)
  me <- se*z
  ci <- c(m-me,m+me)
  return(ci)
}
rounds = 50
S_round = c(1:rounds)
nsamples = 50
pop_pot0_ph_mean = mean(data_Pot0_no_out$pH)
pop_pot0_ph_sd = sd(data_Pot0_no_out$pH)
pop_pot1_ph_mean = mean(data_Pot1_no_out$pH)
pop_pot1_ph_sd = sd(data_Pot1_no_out$pH)
pop_pot0_turb_mean = mean(data_Pot0_no_out$Turbidity)
pop_pot0_turb_sd = sd(data_Pot0_no_out$Turbidity)
pop_pot1_turb_mean = mean(data_Pot1_no_out$Turbidity)
pop_pot1_turb_sd = sd(data_Pot1_no_out$Turbidity)
cl90 = 0.9
cl95 = 0.95
cl99 = 0.99
#####################
## Confidence Interval 90 pH : Potability = 0 ##
CI_pH_Pot0_90_mean = c()
CI_pH_Pot0_90_lower = c()
CI_pH_Pot0_90_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot0_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot0_90_lower[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot0_90_upper[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot0_90_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot0_90 = data.frame(S_round,CI_pH_Pot0_90_mean,CI_pH_Pot0_90_lower,CI_pH_Pot0_90_upper)

qplot(
  x = CI_pH_Pot0_90_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot0_90,main = "Confidence Interval (CI) of Mean (90) : Average pH of Non-Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot0_90_lower, xmax = CI_pH_Pot0_90_upper, width = 1)) + geom_vline(xintercept = pop_pot0_ph_mean)
## Confidence Interval 90 pH : Potability = 1 ##
CI_pH_Pot1_90_mean = c()
CI_pH_Pot1_90_lower = c()
CI_pH_Pot1_90_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot1_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot1_90_lower[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot1_90_upper[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot1_90_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot1_90 = data.frame(S_round,CI_pH_Pot1_90_mean,CI_pH_Pot1_90_lower,CI_pH_Pot1_90_upper)

qplot(
  x = CI_pH_Pot1_90_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot1_90,main = "Confidence Interval (CI) of Mean (90) : Average pH of Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot1_90_lower, xmax = CI_pH_Pot1_90_upper, width = 1)) + geom_vline(xintercept = pop_pot1_ph_mean)

## Confidence Interval 95 pH : Potability = 0 ##
CI_pH_Pot0_95_mean = c()
CI_pH_Pot0_95_lower = c()
CI_pH_Pot0_95_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot0_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot0_95_lower[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot0_95_upper[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot0_95_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot0_95 = data.frame(S_round,CI_pH_Pot0_95_mean,CI_pH_Pot0_95_lower,CI_pH_Pot0_95_upper)

qplot(
  x = CI_pH_Pot0_95_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot0_95,main = "Confidence Interval (CI) of Mean (95) : Average pH of Non-Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot0_95_lower, xmax = CI_pH_Pot0_95_upper, width = 1)) + geom_vline(xintercept = pop_pot0_ph_mean)
## Confidence Interval 95 pH : Potability = 1 ##
CI_pH_Pot1_95_mean = c()
CI_pH_Pot1_95_lower = c()
CI_pH_Pot1_95_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot1_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot1_95_lower[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot1_95_upper[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot1_95_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot1_95 = data.frame(S_round,CI_pH_Pot1_95_mean,CI_pH_Pot1_95_lower,CI_pH_Pot1_95_upper)

qplot(
  x = CI_pH_Pot1_95_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot1_95,main = "Confidence Interval (CI) of Mean (95) : Average pH of Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot1_95_lower, xmax = CI_pH_Pot1_95_upper, width = 1)) + geom_vline(xintercept = pop_pot1_ph_mean)

## Confidence Interval 99 pH : Potability = 0 ##
CI_pH_Pot0_99_mean = c()
CI_pH_Pot0_99_lower = c()
CI_pH_Pot0_99_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot0_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot0_99_lower[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot0_99_upper[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot0_99_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot0_99 = data.frame(S_round,CI_pH_Pot0_99_mean,CI_pH_Pot0_99_lower,CI_pH_Pot0_99_upper)

qplot(
  x = CI_pH_Pot0_99_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot0_99,main = "Confidence Interval (CI) of Mean (99) : Average pH of Non-Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot0_99_lower, xmax = CI_pH_Pot0_99_upper, width = 1)) + geom_vline(xintercept = pop_pot0_ph_mean)
## Confidence Interval 99 pH : Potability = 1 ##
CI_pH_Pot1_99_mean = c()
CI_pH_Pot1_99_lower = c()
CI_pH_Pot1_99_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_pH = sample(data_Pot1_no_out$pH,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_pH))
  Arr_sample_SD[a] = c(sd(sample_pH))
  Arr_sample_Mean[a] = c(mean(sample_pH))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_pH_Pot1_99_lower[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[1]
  CI_pH_Pot1_99_upper[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[2]
  CI_pH_Pot1_99_mean[b] = mean(Arr_sample_Mean[b])
}

CI_pH_Pot1_99 = data.frame(S_round,CI_pH_Pot1_99_mean,CI_pH_Pot1_99_lower,CI_pH_Pot1_99_upper)

qplot(
  x = CI_pH_Pot1_99_mean,
  y = S_round,
  color = S_round,
  data = CI_pH_Pot1_99,main = "Confidence Interval (CI) of Mean (99) : Average pH of Drinkable Water",
  xlab = "Average pH",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_pH_Pot1_99_lower, xmax = CI_pH_Pot1_99_upper, width = 1)) + geom_vline(xintercept = pop_pot1_ph_mean)
###########################
## Confidence Interval 90 Turbidity : Potability = 0 ##
CI_turb_Pot0_90_mean = c()
CI_turb_Pot0_90_lower = c()
CI_turb_Pot0_90_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot0_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot0_90_lower[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot0_90_upper[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot0_90_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot0_90 = data.frame(S_round,CI_turb_Pot0_90_mean,CI_turb_Pot0_90_lower,CI_turb_Pot0_90_upper)

qplot(
  x =  CI_turb_Pot0_90_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot0_90,main = "Confidence Interval (CI) of Mean (90) : Average Turbidity of Non-Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot0_90_lower, xmax = CI_turb_Pot0_90_upper, width = 1)) + geom_vline(xintercept = pop_pot0_turb_mean)

## Confidence Interval 90 Turbidity : Potability = 1 ##
CI_turb_Pot1_90_mean = c()
CI_turb_Pot1_90_lower = c()
CI_turb_Pot1_90_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot1_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot1_90_lower[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot1_90_upper[b]= getCI(cl90,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot1_90_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot1_90 = data.frame(S_round,CI_turb_Pot1_90_mean,CI_turb_Pot1_90_lower,CI_turb_Pot1_90_upper)

qplot(
  x =  CI_turb_Pot1_90_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot1_90,main = "Confidence Interval (CI) of Mean (90) : Average Turbidity of Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot1_90_lower, xmax = CI_turb_Pot1_90_upper, width = 1)) + geom_vline(xintercept = pop_pot1_turb_mean)

## Confidence Interval 95 Turbidity : Potability = 0 ##
CI_turb_Pot0_95_mean = c()
CI_turb_Pot0_95_lower = c()
CI_turb_Pot0_95_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot0_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot0_95_lower[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot0_95_upper[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot0_95_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot0_95 = data.frame(S_round,CI_turb_Pot0_95_mean,CI_turb_Pot0_95_lower,CI_turb_Pot0_95_upper)

qplot(
  x =  CI_turb_Pot0_95_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot0_95,main = "Confidence Interval (CI) of Mean (95) : Average Turbidity of Non-Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot0_95_lower, xmax = CI_turb_Pot0_95_upper, width = 1)) + geom_vline(xintercept = pop_pot0_turb_mean)

## Confidence Interval 95 Turbidity : Potability = 1 ##
CI_turb_Pot1_95_mean = c()
CI_turb_Pot1_95_lower = c()
CI_turb_Pot1_95_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot1_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot1_95_lower[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot1_95_upper[b]= getCI(cl95,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot1_95_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot1_95 = data.frame(S_round,CI_turb_Pot1_95_mean,CI_turb_Pot1_95_lower,CI_turb_Pot1_95_upper)

qplot(
  x =  CI_turb_Pot1_95_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot1_95,main = "Confidence Interval (CI) of Mean (95) : Average Turbidity of Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot1_95_lower, xmax = CI_turb_Pot1_95_upper, width = 1)) + geom_vline(xintercept = pop_pot1_turb_mean)

## Confidence Interval 99 Turbidity : Potability = 0 ##
CI_turb_Pot0_99_mean = c()
CI_turb_Pot0_99_lower = c()
CI_turb_Pot0_99_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot0_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot0_99_lower[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot0_99_upper[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot0_99_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot0_99 = data.frame(S_round,CI_turb_Pot0_99_mean,CI_turb_Pot0_99_lower,CI_turb_Pot0_99_upper)

qplot(
  x =  CI_turb_Pot0_99_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot0_99,main = "Confidence Interval (CI) of Mean (99) : Average Turbidity of Non-Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot0_99_lower, xmax = CI_turb_Pot0_99_upper, width = 1)) + geom_vline(xintercept = pop_pot0_turb_mean)

## Confidence Interval 99 Turbidity : Potability = 1 ##
CI_turb_Pot1_99_mean = c()
CI_turb_Pot1_99_lower = c()
CI_turb_Pot1_99_upper = c()
Arr_sample_AVG = c()
Arr_sample_Mean = c()
Arr_sample_SD = c()
## random sample
for(a in 1:rounds){
  sample_turb = sample(data_Pot1_no_out$Turbidity,nsamples)
  Arr_sample_AVG[a] = c(data.frame(sample_turb))
  Arr_sample_SD[a] = c(sd(sample_turb))
  Arr_sample_Mean[a] = c(mean(sample_turb))
}
## assign array lower upper mean
for(b in 1:rounds){
  CI_turb_Pot1_99_lower[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[1]
  CI_turb_Pot1_99_upper[b]= getCI(cl99,nsamples,Arr_sample_AVG[[b]])[2]
  CI_turb_Pot1_99_mean[b] = mean(Arr_sample_Mean[b])
}

CI_turb_Pot1_99 = data.frame(S_round,CI_turb_Pot1_99_mean,CI_turb_Pot1_99_lower,CI_turb_Pot1_99_upper)

qplot(
  x =  CI_turb_Pot1_99_mean,
  y = S_round,
  color = S_round,
  data =  CI_turb_Pot1_99,main = "Confidence Interval (CI) of Mean (99) : Average Turbidity of Drinkable Water",
  xlab = "Average Turbidity (NTU)",
  ylab = "Round"
)+geom_errorbar(aes(xmin = CI_turb_Pot1_99_lower, xmax = CI_turb_Pot1_99_upper, width = 1)) + geom_vline(xintercept = pop_pot1_turb_mean)
#################################

#### Linear Regression

data_Pot0_no_out <- data.frame(pH = c(no_outliers_pH_Pot_0), Turbidity = c(no_outliers_Turb_Pot_0[1:1184]))
data_Pot1_no_out <- data.frame(pH = c(no_outliers_pH_Pot_1), Turbidity = c(no_outliers_Turb_Pot_1[1:793]))
data_Pot0_no_out_1 <- data.frame(pH = c(no_outliers_pH_Pot_0), Turbidity = c(no_outliers_Turb_Pot_0[1:1184]), Potability = c(dataRm_Na$Potability[1:1184]))
data_Pot1_no_out_1 <- data.frame(pH = c(no_outliers_pH_Pot_1), Turbidity = c(no_outliers_Turb_Pot_1[1:793]), Potability = c(dataRm_Na$Potability[1:793]))


ggplot(data_Pot0_no_out_1,aes(Turbidity,Potability))+
  geom_point(color = "red")+
  geom_smooth(method = "lm",size = 1, alpha = 1)+
  xlab("Turbidity")+ylab("Potability")+
  theme(axis.title = element_text(size=10))

model <- lm(Potability~pH,data = data_Pot0_no_out_1)
model
tidy(model)
x<-data_Pot0_no_out_1$Turbidity
y<-data_Pot0_no_out_1$Potability
xbar <- mean(x)
ybar <- mean(y)
n <- length(y)
# y = 0.307 

SSxy <- sum(x*y) - n*xbar*ybar
SSxx <- sum(x^2) - n*xbar^2
SSyy <- sum(y^2) - n*ybar^2
r <- SSxy/(sqrt(SSxx)*sqrt(SSyy))

ggplot(data_Pot1_no_out_1,aes(Turbidity,Potability))+
  geom_point(color = "red")+
  geom_smooth(method = "lm",size = 1, alpha = 1)+
  xlab("pH")+ylab("Potability")+
  theme(axis.title = element_text(size=10))

model <- lm(Potability~Turbidity,data = data_Pot1_no_out_1)
model
tidy(model)
x<-data_Pot1_no_out_1$Turbidity
y<-data_Pot1_no_out_1$Potability
xbar <- mean(x)
ybar <- mean(y)
n <- length(y)
# y = 0.307 

SSxy <- sum(x*y) - n*xbar*ybar
SSxx <- sum(x^2) - n*xbar^2
SSyy <- sum(y^2) - n*ybar^2
r <- SSxy/(sqrt(SSxx)*sqrt(SSyy))

