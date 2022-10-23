#install.packages("ggstatsplot")
#install.packages("aplpack")

setwd("E:/2D/Final/Prob_Stat/simpleAnalysis01/probStat_simpleAnalysis")
dat<-read.csv("water_potability.csv",header=T)
dataRm_Na <- na.omit(dat)
data_Potability_0 <- subset(dataRm_Na, Potability == 0)
data_Potability_1 <- subset(dataRm_Na, Potability == 1)

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

