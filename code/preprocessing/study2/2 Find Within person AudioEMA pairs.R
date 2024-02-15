#This script takes the EMA data PER target, and matches it with the Audio data. 
#The final object includes all EMA/Audio pairs without the possiblitiy for duplicates. 


#Rationale for the time diff approach:  The filename columns in the audio and ema data contain a time stamp but it is not granular enough to disentangle duplicate entries and in some cases the audio recordings are 'minimally' several hours distant from the closest EMA records.  using the UTC time recorded in the EMA records is also more precise than the EMA filenames. Even with this approach, there are still, roughly, 16.7% of all audio files  that are recorded more than 10 minutes later or earlier than the closest EMA (for the target SAD).  


a <- read.csv(file.choose()) #import the audio features
b <- read.csv(file.choose()) #import the EMA data

rm(list=setdiff(ls(), c("a","b")))
a$names <- stringr::str_remove(a$ID.index,"/Volumes/ROCKET-nano/All audio files//")
a$names <- stringr::str_remove(a$names,"/Volumes/ROCKET-nano/All audio files for missing/")

q <- table(a$names) 
length(q)
i=1
#Conduct the pairing of the EMAs with the Audio files
for(i in 1:length(q)){
  print(i) #For each person
  
  #Grab all the audio files of the person
  a2 <- a[a$names%in%names(q)[i],]
  #Grab all the ema files of the person
  b2 <- b[b$names%in%names(q)[i],]
  if(nrow(b2)>0){
  #Convert audio times from character to time format
  t1 <- substring(a2$ID.index.dates,1,nchar(a2$ID.index.dates)-4)
  t1 <-  format(as.POSIXlt(t1,format = "%Y-%m-%d_%H-%M-%S",tz='UTC'),"%Y-%m-%d %H:%M:%OS3")
  #Convert ema times from character to time format
  t2 <- substring(b2$UTC.time,1,nchar(b2$filename)-4)
  t2 <-  format(as.POSIXlt(t2,format = "%Y-%m-%d %H:%M:%OS",tz="UTC"),"%Y-%m-%d %H:%M:%OS3")
  if(exists('min.index.bin')){rm(min.index.bin,diff.bin) }
 J=1 
  for(J in 1:length(t1)){  #For each audio time
   
    #Compute difference between the audio time and every EMA time
  
  diffs <- difftime(t1[J],t2,units = 'secs')
  #record which EMA time and index is the closest to the audio time
  min.time <- diffs[which(abs(diffs)==min(abs(diffs)))]
  min.index <- which(abs(diffs)==min(abs(diffs)))
  #Each audio file should have 1 index and 1 time diff. But, in principle, the same audio file could be equally minimally distant from two (maybe more) EMA files because of the way data were recorded and ingested for storage. So, if there is an audio file linked to multiple EMA responses, we pick the EMA response that occurred before the audio sample was recorded.(i.e., the positive timediff) because the EMAs were answered before the audio samples were recorded during collection
  if(length(min.time)>1){ 
  min.index <-  min.index[which(min.time==max(min.time))]
  min.time <- min.time[which(min.time==max(min.time))]}
  
  #if(abs(min.time)>4000){stop()}
  #Create storage for indexes and times for each audio file. Each audio file adds 1 index and 1 time difference.
  
  if(!exists('diff.bin')){diff.bin <- c()
  min.index.bin <- c()}
  
  min.index.bin <- c(min.index.bin,min.index)
  diff.bin <- c(diff.bin,min.time)
  }
  # Also, and more likely, two audio files could be minimally distant from the same EMA file. SO, 
  #create a table to see if any EMA indexes are duplicated. 
 tb1 <- table(min.index.bin)

  #Identify repeated indexes.
 repeats <- as.numeric(names(tb1)[which(tb1>1)])
 if(length(repeats)>0){ #if there are repeated indexes, remove the indexes that are more distant in time per the diff.time recorded in diff.bin
   reps=1
 for(reps in 1:length(repeats)){
 indexes.with.repeats <- which(min.index.bin==repeats[reps])
 to.drop <- indexes.with.repeats[which(abs(diff.bin[indexes.with.repeats])>min(abs(diff.bin[indexes.with.repeats])))] #Dropping indexes with greater difference in time. In principle, there could be multiple samples with the same minimal difference in time, in which case there would still be multiples after this step and the code would hang below when diff.time is added to the joined object. Very unlikely to occur though as both audio files would have to be uploaded at the same temporal distance around an EMA.
 if(!exists('to.drop.bin')){to.drop.bin <- c()}
 to.drop.bin <- c(to.drop.bin, to.drop) 
 }
 min.index.bin <- min.index.bin[-to.drop.bin]
 diff.bin <- diff.bin[-to.drop.bin]
  
 #min.index.bin records the row index in b2 to retain. 
 #-to.drop.bin refers to the index of min.index.bin before repeats are removed. i.e., it refers to the indexes in a2 that should be dropped due to mapping on to the same EMA as other audio files but having greater diff.time. 
  b3 <- b2[min.index.bin,]
  a3 <- a2[-to.drop.bin,]
  
  rm(to.drop.bin)
  
 } else {
#to simply code below, Still need to create the b3 a3 objects even if there are no repeats
 b3 <- b2[min.index.bin,] 
 a3 <- a2 }
 
  b3$index.bin <- min.index.bin
  a3$index.bin <- min.index.bin
  joined <- merge(b3,a3,by='index.bin')
  joined$time.diff <- diff.bin
  rm(min.index.bin,diff.bin)
  if(!exists('joined.bin')){joined.bin <- list()}
  joined.bin <- append(joined.bin,list(joined))
  } #closes b2 loop (i.e., match records only if there are EMAs for theZZZ person)
} #closes for loop


#Recompile all data into a final object
joined.bin <- Reduce(rbind,joined.bin)

#label targets
colnames(joined.bin)[15] <- "CONTENT_EMA_Numeric"
colnames(joined.bin)[28] <- "ENERGY_EMA_Numeric"
colnames(joined.bin)[41] <- "LONELY_EMA_Numeric"
colnames(joined.bin)[54] <- "SAD_EMA_Numeric"
colnames(joined.bin)[67] <- "STRESSED_EMA_Numeric"




write.csv(joined.bin,file = 'C:/Users/Zachariah/Desktop/All_EMA_opensmile_features_FA18_Joined.csv',row.names = F)
