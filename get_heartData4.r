
#Remember to change format of time column in excel to m:d:yyyy h:mm:ss

#Get list of files to process
files <- list.files(pattern ="Wk10_")
#Set up loop to process each file in turn
for (j in seq_along(files)) {
	filenamej <- files[j]
	filenamej_NoExt <- substr(files[j],1,nchar(files[j])-4)
	heartData <- read.table(file=filenamej, head=TRUE, sep="\t")
	time_interval <- read.table(file="time_interval.txt", sep="\t")
	start_time <- time_interval[,1]
	end_time <- time_interval[,2]
	StatsTable <- read.table("Stats.txt", head=TRUE, sep = "\t", stringsAsFactors=F)

	#For all cases
	for (i in 1:24 ) {
		if (grepl("8/21/2012", heartData[1,2])) { 					#if row1 column2 of heartData contains 8/21/2012 do this:
			myStart <- start_time[i]
			s_time <- paste("8/21/2012", myStart)
			f_idx<-which(heartData$RealTime == s_time)
			f <- f_idx[2]
			  
			#second condition

			myEnd <- end_time[i]
			e_time <- paste("8/21/2012", myEnd)
			e_idx<-which(heartData$RealTime == e_time)
			l <- e_idx[length(e_idx)]
		} else if (grepl("8/22/2012", heartData[1,2])) { 			#else if column2 of heartData contains 8/22/2012 do this:
			myStart <- start_time[i]
			s_time <- paste("8/22/2012", myStart)
			f_idx<-which(heartData$RealTime == s_time)
			f <- f_idx[2]
			  
			#second condition

			myEnd <- end_time[i]
			e_time <- paste("8/22/2012", myEnd)
			e_idx<-which(heartData$RealTime == e_time)
			l <- e_idx[length(e_idx)]
		} else  { 													#else do this:
			myStart <- start_time[i]
			s_time <- paste("8/23/2012", myStart)
			f_idx<-which(heartData$RealTime == s_time)
			f <- f_idx[2]
			  
			#second condition

			myEnd <- end_time[i]
			e_time <- paste("8/23/2012", myEnd)
			e_idx<-which(heartData$RealTime == e_time)
			l <- e_idx[length(e_idx)]	
		}
		
		#Save the subset of data to new object
		newData <- heartData[f:l, ]
		filename1 <- paste("Subset_",filenamej_NoExt,"_",i,".csv", sep="")
		write.csv(newData, file= filename1)
		
		#Create new_RR file with outliers removed
		RR <- newData[,5]
		
		# total # of rows
		tot_row <- length(RR)

		# Statistics
		total <- sum(RR)
		ave <- mean(RR)
		mysd <- sd(RR)

		upper <- ave + (3*mysd)
		lower <- ave - (3*mysd)
		new_RR <- subset(RR, RR <= upper & RR >= lower)
		length_RR <- length(RR)
		length_new_RR <- length(new_RR)
		diff <- length_RR - length_new_RR
		per <- (diff/tot_row) *100
		
		# write newData object to a file
		filename2 <- paste("RR_",filenamej_NoExt,"_",i,".txt", sep="")
		write.table(new_RR, file= filename2, col.names = FALSE, row.names = FALSE )
		
		#Append Stats to StatsTable
		NewRow <- list(i, filename2, total, ave, mysd, upper, lower, length_RR, length_new_RR, per)
		StatsTable <- rbind(StatsTable, NewRow)

	 }
	 
	 #write NewStatsTable
	 filename3 <- paste("NewStatsTable_",filenamej_NoExt,".csv", sep = "")
	 write.csv(StatsTable, file = filename3, row.names = FALSE)
 
}
 