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


# Frequency chart for appearance of certain terms.
# "power", "frankenstorm", "lights", "real", "school", "destroyed", "wet", "why", "touchdown", # "beer", "cuddling"

##################################
#  3. SANDY PERSONA OVER TIME    #
##################################

# Sandy terms to be used: Sandy, bitch, whore, nigger, black, woman, need no man, wet,
# blow, don't care, AHuricaneSandy, Katrina, Irene, Obama, twerk, twerk-icane, school (with nigger)
# olive garden, threw a Taco Bell, emergency ration
# "only a category 1", "overrated", "death megatron"

termdate <- function(term, date=25) {
	col <- ifelse(grepl(term, td[,22]) == TRUE, td[,22], NA)
	sub <- cbind(col, td[,19])
	sub <- as.data.frame(sub)
	names(sub) <- c("Body", "Time")
	sub[,1] <- as.character(sub[,1])
	sub[,2] <- as.character(sub[,2])
	complete <- sub[which(is.na(sub$Body) == FALSE & substring(sub$Time, 9, 10) == date), ]
	return(complete)
}

countsByDay <- function(termvec) {
	countsList <- lapply(termvec, termdate)
	getCount <- function(listObj) {
		return(length(listObj[,1]))
	}
	counts <- lapply(countsList, getCount)
	counts <- unlist(counts)
	countsTable <- as.data.frame(cbind(termvec, counts))
	return(countsTable)
}

day1 <- countsByDay(personaVec)
day2 <- countsByDay(personaVec)
day3 <- countsByDay(personaVec)
day4 <- countsByDay(personaVec)
day5 <- countsByDay(personaVec)
day6 <- countsByDay(personaVec)
day7 <- countsByDay(personaVec)

getPlots <- function(clist) {
	clist[,2] <- as.numeric(as.character(clist[,2]))
	return(barplot(xtabs(counts~personaVec, data=clist)))
}

daysVec <- paste("day",1:6,sep="")
par(mfrow=c(3,3))
getPlots(day1)
getPlots(day2)
getPlots(day3)
getPlots(day4)
getPlots(day5)
getPlots(day6)
getPlots(day7)



termsvec <- c("black", "Taco Bell", "nigga")
personaVec <- c("whore", "bitch", "twerk", "need no man", "blow", "don't care")
infoVec <- c("help", "NOAA", "ration", "overrated", "death", "megatron", "category1")

##################################
#  4. GEO-SPATIAL ANALYSIS       #
##################################

library(maps)
library(rworldmap)

plotTerms <- function(sub, color) {
	points(sub[,25], sub[,24], col=color, cex=0.08)
}

gMT <- function(term) {
	subFrame <- function(date) {
		subFrame <- td[grepl(term,td[,22],ignore.case=TRUE) == TRUE & substring(td[,19], 9, 10) == date, ]
		return(subFrame)
	}
	dates <- c(25:30)
	return(subList <- lapply(dates, subFrame))
}

folderPlots <- function(term) {
	plot.new()
	colVec <- c("cyan", 47, 558, 115, "firebrick1", "goldenrod2")
	setwd("/Users/spencerhall/Desktop/RPlots")
	p <- paste("png(\'",term,".png\')",sep="")
	eval(parse(text=p))
	newmap <- getMap(resolution = "low")
	plot(newmap, xlim=c(-80, -67), ylim=c(40.9, 41), asp=1.5)
	for (i in 1:6) {
		plotTerms(gMT(term)[[i]], colVec[i])
	}
	ds <- c("10/25","10/26", "10/27", "10/28", "10/29", "10/30")
	legend(x="bottomright", y=40, ds, col=colVec, pch=16)
	dev.off()
}

personaVec <- c("pregnant", "bread sticks", "pole", "shark")
sapply(personaVec, folderPlots)

######################
#  TERMS TO LOOK UP  #
######################

# Duration: "over yet," "bring it"
# Strength: "cat 1," "aftermath," "apocalypse," "category 1, "worse", "Obama's Katrina"
# Size: "tornado," "touchdown" (with spacing)

# Persona terms: "twerk," "whore," "Taco Bell," "pregnant," "bread sticks," "pole," "shark"

counter <- function(term, date) {
	sub <- td[substring(td[,15], 1, 7) == "Eastern", ]
	sub <- cbind(sub[,19], sub[,22])
	nS <- na.omit(sub[substring(sub[,1], 9, 10) == date, ])
	final <- nS[grepl(term, nS[,2], ignore.case=TRUE, perl=TRUE), ]
	if (length(final) > 2) {
		return(length(final[,1]))
	}
	else if (length(final) == 2) {
		return(1)
	}
	else {
		return(0)
	}
}

byTerm <- function(term) {
	dates <- 25:30
	cV <- vector()
	for (i in 1:length(dates)) {
		cV[i] <- counter(term, dates[i])
	}
	return(cV)
}

#### Term ratios. Persona: nigga, bitch, whore
# "NWS," "mph," "category/cat," and maybe "landfall."

persona <- counter("nigga") + counter("bitch") + counter("whore")
counter("NWS") / persona
counter("mph") / persona
(counter("category") + counter("cat")) / persona
counter("landfall") / persona
counter("windspeed") / persona




