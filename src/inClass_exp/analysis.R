# Survey Analysis Script
# Imanol Arrieta Ibarra, Camelia Simoiu
# February 16 2015

library(data.table)
library(ggplot2)
library(bit64)
library(outliers)
library(fBasics)

# Set working directory 
directory <- "D:/Stanford/crowds/survey"
setwd(directory)


GeometricMean <- function(x){
  # Computes the geometric mean of a vector.
  #
  # Args:
  #   x: a vector of integers / doubles representing the participant guesses
  #
  # Returns:
  #   The geometric mean of the vector
  #
  gm = exp(mean(log(x+1)))
  gm
}

RemoveOutliers <- function(x, na.rm = TRUE, ...) {
  # Removes outliers based on the interquartile range.
  #
  # Args:
  #   x: a vector of integers / doubles representing the participant guesses
  #   na.rm: removes missing values of x
  #
  # Returns:
  #   The vector with outliers removed
  #
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm,...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

TruncatedMean <- function(x, na.rm = TRUE, ...) {
  # Computes the 25% truncated mean of a vector.
  #
  # Args:
  #   x: a vector of integers / doubles representing the participant guesses
  #   na.rm: removes missing values of x
  #
  # Returns:
  #   The 25% truncated mean.
  #
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm,...)
  y <- x
  y[x < qnt[1]] <- NA
  y[x > qnt[2]] <- NA
  y <- na.omit(y)
  mean(y)
}

PctWorse <- function(x, TrueValue, f){
  # Computes the percentage of participants who guessed worse than the average.
  # Measured in terms of the absolute-value distance from the correct value.
  #
  # Args:
  #   x: a vector of integers / doubles representing the participant guesses
  #   TrueValue: the correct value
  #   f: function (mean, geometric mean, etc.)
  #
  # Returns:
  #   The percentage of answers who were worse than the average.
  #
  dist = abs(x-TrueValue)
  m = abs(f(x)-TrueValue)
  1-sum(dist<=m)/length(dist) 
}

summarize<- function(var, TrueValue, name){
  # Outputs summary statistics and plots after the removal of outliers.
  #
  # Args:
  #   var: a vector of integers / doubles representing the participant guesses
  #   TrueValue: the correct value
  #   name: function name (mean, geometric mean, etc.)
  #
  # Returns:
  #   Summary table of descriptive statistics.
  #   Histogram plots of guesses
  #   Red line: Correct answer
  #   Blue line: Average guess of participants
  #
  x <- RemoveOutliers(var)
  x <- na.omit(x)
  df = data.frame(V1=as.numeric(x))
  m <- ggplot(df, aes(x=V1))
  m <- m + geom_histogram(fill="blue",alpha=0.5) + xlab(name)
  m <- m + geom_vline(xintercept=TrueValue, color='red')
  m <- m + geom_vline(xintercept=mean(x), color='blue', linetype = "longdash")
  plot(m)
  data.table(CorrectAnswer = TrueValue, 
             Mean = mean(x), 
             Median= median(x),
             Min = min(x), 
             Max = max(x), 
             SD = stdev(x), 
		 TruncMean = TruncatedMean(x),
	       GeomMean = GeometricMean(x), 
             Skewness = skewness(x),
             PctWorseMean = PctWorse(x, TrueValue, mean), 
             PctWorseGeoMean = PctWorse(x, TrueValue, GeometricMean),
		  PctWorseTruncMean = PctWorse(x, TrueValue, TruncatedMean))   
}


# Main Script
Answers = c(1890,1149,300,224,3287590)
Survey <- fread(input='Questions.csv', header = TRUE)
Questions <- Survey
Questions$Name <- NULL


# Output results
PaintingYr <- summarize(var=Questions$Year, 
		            TrueValue=Answers[1], 
                        name='Year of Painting')

NrDots <- summarize(var=Questions$Dots, 
			   TrueValue=Answers[2], 
			   name='Number of Dots')

RunsIndia  <- summarize(var=Questions$IndiaRuns, 
 			       TrueValue=Answers[3], 
			       name='Number of Runs by India')

RunsPakistan  <- summarize(var=Questions$PakistanRuns, 
				    TrueValue=Answers[4], 
				    name='Number of Runs by Pakistan')

IndiaLandmass <- summarize(var=Questions$India, 
			          TrueValue=Answers[5], 
	 			    name='Landmass of India (km2)')


