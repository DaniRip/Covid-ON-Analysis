covidONAnalisis <- function(cases, tests, curDate)
{ # Calculates the % positives/day and outputs them along with the daily positives and tests
  # Seeded with Ontario, Canada data, targetting their important dates
  # To customize to another region, different dates will likely be needed for colour coding!
  #
  # Written and updated by: Danielle Ripsman
	# ----------------------------------------------------------#
  
  # Great gov. data source for ON covid-19 info: https://covid-19.ontario.ca/data#testing
  
  # Latest default settings
  if(!hasArg(cases)){
    cases = c(260, 426, 401, 462, 375, 408, 309, 379, 550, 483, 478, 411, 401, 421, 483, 494, 514, 564, 485, 568, 606, 551, 510, 634, 640, 476, 437, 424, 525, 347, 459, 421, 511, 434, 370, 387, 412, 399, 477, 346, 294, 308, 361, 329, 345, 341, 391, 340, 304,427,390,413,441,412,460,404,287,292,383,344,323,326,404,446,338,356,344,455,415, 243,230,251,203,182,266,197,181,184, 190, 173, 178, 206, 175, 161, 216, 163, 189, 111, 160, 178, 257, 157, 149, 153, 165, 121, 138, 154, 112, 118, 170, 116, 130, 129, 116, 111, 102, 111, 111, 166, 164, 135, 203, 165, 103, 195, 138, 137, 119, 111, 76, 89, 134, 124, 116,  88,  91,  86,  95,  88,  70,  79, 115,  33,  95,  78,  92, 106,  81,  99, 125, 102,  76, 131, 108, 115, 105, 100, 88, 118, 122, 148, 112, 114, 112, 133, 132, 148, 169, 158, 190, 185, 149, 170, 213, 232, 204, 313, 251, 315, 293, 401, 407, 365, 425, 478, 335, 409, 409, 435, 491, 700, 554, 625, 538, 732, 653, 566, 615, 548, 583)
  }
  if(!hasArg(tests)){
    tests = c(3168, 6245, 4859, 4020, 4585, 3708, 3750, 2568, 3237, 4097, 3975, 3648, 6844, 5065, 4852, 6010, 9001, 8899, 9462, 9643, 11704, 9330 , 16730, 10214, 12295, 10578, 12020, 12550, 10852, 11554, 12928, 16532, 16305, 17146, 14555, 10654, 12961, 15179, 16295, 19227, 17618, 13970, 11957, 15137, 17429, 18345, 17768, 16217, 9155, 5813,7382,10506,11276,11028,11383,8170,9875,15133,17615,18525,20640, 17014,14379,15244,17537,20822,22730, 23105, 19374, 15357, 13509, 19941, 24341, 28335, 27456, 23278, 21751, 21724, 24205, 25278, 27225, 27387, 23408, 21900, 16189, 23207, 27511, 30780, 33492, 28633, 27127, 23759, 26056, 24322, 24194, 21425, 23792, 17303, 15112, 22832, 26326, 27484, 29522,  25726, 20896, 16744, 23769, 26492, 31163, 28849, 26890, 20913, 22974, 23990, 26001, 28809, 29904, 26144, 24664, 17334, 27308, 27676, 30033, 33282, 30443, 23021, 19169, 17229, 26181, 25136, 26008, 27771, 22275, 21581, 24572, 29626, 30137, 30436, 23813, 25567, 23067, 25642, 25917, 28073, 28656, 23384, 18790, 20013, 21960, 28625, 31823, 32106,24970,25098,23545,24004,26298,28591,28672,28955,23725,20929,17605,24669,32501,35618,31143,29540,27664,28761,35134,35826,38940,40127,31753,34201,35436,30634,41865,43238,42509,41111,38375, 35753, 39646, 40093, 46254, 39661, 38196, 42031, 43277)
  }
  if(!hasArg(curDate)){
    curDate = "Oct 7"
  }
  
  fixedCases = 48
  schoolOpen = 162
  schoolOpenDate = "Sept 8"
  newTotalCases = length(cases)
	x = 1:newTotalCases
	x_ax = x - .5
	casesMA = calcMA(cases, 5)
	testsMA = calcMA(tests, 5)
	percents = cases/tests*100
	percentsMA = calcMA(percents, 5)
	
	labs = c("March 31", rep("",fixedCases-2), "May 17", 
	         rep("",schoolOpen-fixedCases-1), schoolOpenDate, 
	         rep("",newTotalCases-schoolOpen-1), curDate)
	
	colourSchemeEarly = c(rep("#828282",fixedCases),rep("#d0d0d0",schoolOpen-fixedCases))
	par(mfrow=c(3,1))
	barplot(cases, main="Daily Positive Tests", space = 0, 
	        col = c(colourSchemeEarly,rep("lightpink",newTotalCases-schoolOpen)), 
	        border = "white")
	lines(x_ax, casesMA)
	text(cex=1, x=x-.25, y=-50, paste(labs), xpd=TRUE)#, srt=45)
	barplot(tests, main="Total Daily Tests", space = 0, 
	        col = c(colourSchemeEarly,rep("lightblue",newTotalCases-schoolOpen)), 
	        border = "white")
	lines(x_ax, testsMA)
	text(cex=1, x=x-.25, y=-2050, paste(labs), xpd=TRUE)
	barplot(percents, main="Percent Positive Tests/Day", space = 0, 
	        col = c(colourSchemeEarly,rep("lightgreen",newTotalCases-schoolOpen)), 
	        border = "white")
	lines(x_ax, percentsMA)
	# Thanks to Tyler in https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot 
	text(cex=1, x=x-.25, y=-1.25, paste(labs), xpd=TRUE)

}

# Quick and dirty function for calculating moving average
calcMA <- function(x, n){
	symShift = (n-1)/2
	pad = rep(NA,symShift)
	l = length(x)
	cx <- c(0,cumsum(x))
	rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
	#final <- c(x[1:symShift], rsum, x[(l-symShift+1):l])
	final <- c(pad, rsum, pad)
}
