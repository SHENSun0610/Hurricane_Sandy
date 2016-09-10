install.packages("RMySQL")
library(RMySQL)
library(MASS)

sandydb <- dbConnect(MySQL(), user='sandyresearch3', password='W7xx4PTVLuTXYhe5', dbname='sandydata', host='dev1.bmazanec.com')

####### ANALYSIS OF TWITTER DATA ########

################################
# 1. TWEET FREQUENCY HISTOGRAM #
################################

sq <- dbSendQuery(sandydb, "select * from twitter_data")
td <- fetch(sq, n=-1)

evector <- vector()
et <- ifelse((substring(td[,15], 1, 7) == "Eastern"), "yes", "no")
et <- na.omit(et)[1:length(na.omit(et))]

# This function gets a vector of just the Eastern Time time-stamps, retaining
# the original indices and simply giving an NA for the non-ET time-stamps.
# et is a vector which identifies each entry in the time-zone column as being eastern or not. If there is an NA,
# then it fills an NA for that index, otherwise it just fills in the "yes" or "no".
# The evector is then filled based on et; if et's value at an index is "yes", evector takes the time-stamp corresponding
# to that index. If et's value at that index is "no" or NA, then evector takes NA for that time.
for (i in 1:length(td[,15])) {
	if (is.na(et[i]) == FALSE && et[i] == "yes") {
		evector[i] = td[i,19]
	}
}

# This loop takes the time-stamp vector, evector, extracts the hour of the day the Tweet was sent, and turns it into
# a numeric value in preparation for the creation of a histogram.
enum <- vector()
for (i in 1:length(evector)) {
	if (is.na(evector[i]) == FALSE) {
		enum[i] = as.numeric(substring(evector[i], 12, 13))
	}
	else {
		enum[i] = NA
	}
}

# Histogram of the Tweet frequency across all ET data.
setwd("/Users/spencerhall/Desktop/RPlots")
p <- paste("png(\'tweetsbyhourwholestorm\')",sep="")
eval(parse(text=p))
hist(enum, xlim=c(0, 24), main="Frequency By Hour - Oct. 25th - 30th", xlab="Hour of Day", col="skyblue", axes=FALSE)
axis(1, seq(0, 23, 1), pos=0)
axis(2, pos=0)
dev.off()

days <- paste("10-",c(25:31),sep="")

divday <- function(date) {
	tonum <- function(numstring) {
		return(as.numeric(substring(numstring, 4, 5)))
	}
	dates <- tonum(date)
	yesnof <- function(date) {
		yesno <- ifelse(is.na(evector) == FALSE & substring(evector, 9, 10) == date, "yes", "no")
	}	
ynvec <- sapply(dates, yesnof)
return(ynvec)
}

ynmat <- divday(days)

loopf <- function(col) {
	temp <- vector()
	for (i in 1:length(col)) {
		if (col[i] == "yes") {
			temp[i] <- as.numeric(substring(evector[i], 12, 13))
		}
	}
	return(temp)
}

set <- apply(ynmat, 2, loopf)

getlength <- function(arg) {
	new  <- na.omit(arg)[1:length(na.omit(arg))]
	return(length(new))
}

getTweets <- function(arg) {
	new  <- na.omit(arg)[1:length(na.omit(arg))]
	return(new)
}

lengths <- lapply(set, getlength)

## Now we get the multi-histogram plot. ##
setwd("/Users/spencerhall/Desktop/RPlots")
p <- paste("png(\'tweetsbydayhour.png\')",sep="")
eval(parse(text=p))
par(mfrow=c(2, 3))
hist(set[[1]], breaks=24, main="Oct. 25th", xlab="Hours", col="chartreuse3", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
hist(set[[2]], breaks=24, main="Oct. 26th", xlab="Hours", col="chartreuse3 ", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
hist(set[[3]], breaks=24, main="Oct. 27th", xlab="Hours", col="chartreuse3 ", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
hist(set[[4]], breaks=24, main="Oct. 28th", xlab="Hours", col="chartreuse3 ", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
hist(set[[5]], breaks=24, main="Oct. 29th", xlab="Hours", col="chartreuse3 ", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
hist(set[[6]], breaks=24, main="Oct. 30th", xlab="Hours", col="chartreuse3 ", axes=FALSE)
axis(1, at = seq(0, 23, 1), labels = TRUE, pos = 0)
axis(2, pos = 0)
dev.off()

################################
#  2. TWEET TERM BAR GRAPHS    #
################################

freq <- c(counter("power"), counter("frankenstorm"), counter("lights"), counter("real"), counter("school"), counter("why"), counter("touchdown"), counter("beer"))

names <- c("Power", "Frankenstorm", "Lights", "Real", "School", "Destroyed", "Why", "Touchdown", "Beer")
my.data <- data.frame(words=names, frequency=freq)
mytable <- xtabs(frequency~words, data=my.data)

setwd("/Users/spencerhall/Desktop/RPlots")
p <- paste("png(\'termsbarplot\')",sep="")
eval(parse(text=p))
barplot(mytable, main="Frequency of Common Terms", xlab="Terms", ylab="Frequency", col="darkblue")
dev.off()
