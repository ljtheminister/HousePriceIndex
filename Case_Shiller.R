# Modeling Problem: 
#  Using the given data set create a repeat sales index using the Case and Shiller methodology as described in the attached paper.
#  NOTE: This dataset was created in the following way from a database of property transactions.
#  	Every transaction is matched to prior sales subject to two things:
#                        That the PropertyIDs are match
#                        and that the matched transaction occured before the current observation.
#                This causes a problem in that every transaction is matched not just to the transaction immediately prior, 
#                but to transactions prior to that. 
#   Cleaning may be required.
#
#   Comment and structure your code to be easily followed.
#
library(McSpatial)
library(dplyr)
library(lubridate)

# Read data
setwd('/Users/lj/Downloads/')
data <- read.csv("Transactions.csv")

# Data Pre-processing
data$transdate <- as.Date(data$transdate, format="%m/%d/%Y")
data$transdate_previous <- as.Date(data$transdate_previous, format="%m/%d/%Y")
data <- data[!(is.na(data$transdate_previous)),] # drop observations with NA transdate_previous because same observations as NA transvalue_previous

# sequence of dates in date_range to convert dates to ints as time periods
min_date <- as.Date(min(data$transdate_previous, na.rm=TRUE))
max_date <- as.Date(max(data$transdate, na.rm=TRUE))
min_year <- year(min_date)
date_range <- as.character(seq(from=min_date, to=max_date, by=1))

## Fix chain of transvalue, transdates
propertyid_groups <- split(data, data$propertyid) # split/subset data by propertyid

fix_chain <- function(grp) { #grp is the set of observations for each propertyid
  for (i in (1:nrow(grp)-1)) {
    grp$transvalue[i] <- grp$transvalue_previous[i+1]
    grp$transdate[i] <- grp$transdate_previous[i+1]
  }
  return (grp)
}

propertyid_groups <- lapply(propertyid_groups, fix_chain) # apply fix_chain function to each subset of data by propertyid
data <- unsplit(propertyid_groups, data$propertyid) # rbind list back into dataframe
#data$gaptime <- data$transdate - data$transdate_previous # compute gaptime between sales


## Time Intervals/Periods for Repeat Sales Method
date_map = list()
daily_periods <- function(date_map) {
  for (i in 1:length(date_range)) {
    date_map[date_range[i]] <- i-1
  }
  return (date_map)
}

process_daily_periods <- function(data, date_map) {
  for (i in 1:nrow(data)) {
    row <- data[i,]
    data[i, 't1'] <- date_map[as.character(row$transdate)]
    data[i, 't0'] <- date_map[as.character(row$transdate_previous)]
  }
}

# annual time periods
data$t0 <- year(data$transdate_previous) - min_year + 1
data$t1 <- year(data$transdate) - min_year + 1

# Repeat Sales Method using McSpatial Library
fit_CS_McSpatial <- repsale(price0=data$transvalue_previous, price1=data$transvalue, time0=data$t0, time1=data$t1, mergefirst=0, stage3="square")

## Let's build our own Case-Shiller Repeat Sales Model
dy <- data$transvalue - data$transvalue_previous
times <- levels(factor(c(data$t0, data$t1)))

K = length(times)
N = length(dy)

# construct matrix for b_t' - b_t
X <- array(0,dim=c(N,K))
for (j in seq(1, K)) {
  X[,j] <- ifelse(data$t1==times[j], 1, X[,j])
  X[,j] <- ifelse(data$t0==times[j], -1, X[,j])
}

# Step 1: Fit BMN model
fit1 <- lm(dy ~ X) # no intercept
e <- residuals(fit1) # take the residuals
data$saletime <- data$t1 - data$t0 
# Step 2: Fit residuals
fit2 <- lm(e^2 ~ data$saletime) # fit the residuals
W <- fitted(fit2)
W <- ifelse(W>0, 1/sqrt(W), 0) # make non-negative, take reciprocal of sqrt of fitted values
# Step 3: Weighted Least Squares regression
fit_CS <- lm(dy ~ X + 0, weights=W)
