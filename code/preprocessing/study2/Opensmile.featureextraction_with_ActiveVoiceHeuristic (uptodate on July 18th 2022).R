reticulate::use_python('/Users/zachariah/opt/anaconda3/bin/python')
i=1
audiofile=1
if(!exists('sample_recognize')){
  reticulate::source_python('~/Desktop/PYAUDIO_opensmile.py',convert = F)}

# files <- list.files(r'(F:/Fa18data/)',full.names = T)
# names <- stringr::str_remove(files,"F:/Fa18data/")
#Chunk1 Extract audio files from other data for faster loading. 
# for(i in 2:length(files)){
#   print(i)
#   tryCatch(   a <- readRDS(files[i]),error=function(e)a <- paste('Failed to load:',names[i]))
#   if('audio_recordings'%in%names(a)){
#     a <- a$audio_recordings
#     saveRDS(a,file = paste0("F:/All audio files/",names[i]))}
#   if(is.character(a)){print(a)}
#   
# }
files <- list.files(dirname(file.choose()),full.names = T)
names <- list.files(dirname(file.choose()))
# #chunk 2, load audio files per person, save to .wav format, process with python script, 
files <- list.files(r'(/Volumes/ROCKET-nano/All audio files/)',full.names = T)
names <- stringr::str_remove(files,r'(/Volumes/ROCKET-nano/All audio files/)')

for(i in 1:length(files)){
  print(i)
  tryCatch(   Participant.s.RDS <- readRDS(files[i]),error=function(e)Participant.s.RDS <- paste('Failed to load:',names[i]))
  
  # a <- Participant.s.RDS$audio_recordings
  #saveRDS(a,file = paste0("F:/r/",names[i]))}}
  
  
  #"F:/All audio files/121f7uao.RDS"
  
  
  Start.T.for.person <- Sys.time()
  
  
  N.audiio.files.for.participant <- length(Participant.s.RDS) #Determine the number of audio recordings
  for(audiofile in 1:N.audiio.files.for.participant){
    print(paste("Person",i,":", audiofile,"of",N.audiio.files.for.participant))
    #Extract the audio data for processing
    a <- Participant.s.RDS[[audiofile]]$audio_wave
    Very.quiet.flag <- any(abs(a$sig)>.15)
    if(!Very.quiet.flag){
      a$sig <- (a$sig-min(a$sig))/(max(a$sig)-min(a$sig))*2 -1 }
    
    Text <- Participant.s.RDS[[audiofile]]$speech.to.text$Complete.transcript
    File.duration.in.seconds <- a$duration
    if(is.null(Text)){ Text <- "No Speech Detected"}
    speech.flag <- ifelse(Participant.s.RDS[[audiofile]]$speech.to.text[[1]]=="No speech was detected in this file",F,T)
    if(speech.flag){
      End.of.last.word <- Participant.s.RDS[[audiofile]]$speech.to.text[[length(Participant.s.RDS[[audiofile]]$speech.to.text)]]$end_time[length(Participant.s.RDS[[audiofile]]$speech.to.text[[length(Participant.s.RDS[[audiofile]]$speech.to.text)]]$end_time)]
      Duration.of.ambient.audio <- Participant.s.RDS[[audiofile]]$audio_wave$duration-End.of.last.word
      Combined.voice <- lapply(Participant.s.RDS[[audiofile]]$speech.to.text,function(i){
        if(!is.character(i)){
          
          a <- i$start_time
          b <- i$end_time
          df <- list(a=a,b=b)
          return(df)}
      })
      Combined.voice[[1]] <-NULL
      Combined.voice <- lapply(Combined.voice,function(i) {diff <- i$b-i$a
      i <- data.frame(i,diff=diff)
      return(i)})
      Combined.voice <-  Reduce(rbind,Combined.voice)
      Total.words <- nrow(Combined.voice)
      
      Flag <- TRUE
      starttime.flag <- FALSE
      startime.nrow <- nrow(Combined.voice)
      while(Flag){
        if(Flag){
          Flag <-ifelse(Combined.voice$diff[1]<0 | is.na(Combined.voice$diff[1]) | Combined.voice$diff[1]>5 | Combined.voice$a[1]>40,T,F) 
          if(Flag){Combined.voice <- Combined.voice[-1,]}
          if(nrow(Combined.voice)==0){Flag <- FALSE}
        }}
      rm(Flag)
      Words.in.processed.segment <- nrow(Combined.voice) 
    } else {Combined.voice <- data.frame()}
    if(nrow(Combined.voice)>0){
      if(startime.nrow!=nrow(Combined.voice)){starttime.flag <- TRUE}
      Within.words.sum.of.duration <- sum(Combined.voice$diff)
      Within.words.mean.of.duration <- mean(Combined.voice$diff)
      Within.words.SD.of.duration <- sd(Combined.voice$diff)
      Between.words.sum.pause.duration <- sum(Combined.voice$a[-1]-Combined.voice$b[-nrow(Combined.voice)])
      Between.words.mean.pause.duration <- mean(Combined.voice$a[-1]-Combined.voice$b[-nrow(Combined.voice)])
      Between.words.SD.of.pause.duration <- sd(Combined.voice$a[-1]-Combined.voice$b[-nrow(Combined.voice)])
      Combined.voice.starttime <- Combined.voice$a[1]*44100
      if(starttime.flag) {
        if( a$t[which(abs(a$sig)>.15)[1]]>.1) { starttime <- a$t[which(abs(a$sig)>.15)[1]]*44100-200} else { starttime <- a$t[which(abs(a$sig)>.15)[1]]*44100} } else {
          if(Combined.voice$a[1]>.1){starttime <- Combined.voice$a[1]*44100 - 200} else {starttime <- Combined.voice$a[1]*44100}
        } 
      if(a$duration- Combined.voice$b[nrow(Combined.voice)]>.1){endtime <- Combined.voice$b[nrow(Combined.voice)]*44100 + 200} else {endtime <- Combined.voice$b[nrow(Combined.voice)]*44100}
      if(endtime>length(a$sig)){endtime <- length(a$sig)
      Endtime.flag <- TRUE} else {Endtime.flag <- FALSE}
      if(starttime>endtime){starttime <- 1
      starttime.flag <- TRUE}
      Percentage.of.sample.with.voice <- (endtime-starttime)/length(a$t)
      a$t <- a$t[starttime:endtime]
      
      a$sig <- a$sig[starttime:endtime]
      Raw.file.endtime <- a$nSamples
      a$nSamples <- length(a$t)
      a$duration <- a$nSamples/44100
      if(!exists("tmpdir")){tmpdir <- tempdir()} #use the temporary directory to write files
      audiotmplocation <- paste0(tmpdir,"/tmpaudio.wav")
      rPraat::snd.write(a,audiotmplocation) #saves the wave as a .wav
      # rPraat::snd.write(a,"~/Desktop/Testaudio.wav")
      res <- tryCatch(res <- sample_recognize(audiotmplocation),error=function(w){e2 <- print("ERROR")})
      #   if(!is.character(res)){
      res <- reticulate::py_to_r(res)
     
      
     
     
      
      features <- cbind(N.audiio.files.for.participant=N.audiio.files.for.participant,Very.quiet.flag=Very.quiet.flag,Starttime=starttime,Combined.voice.starttime=Combined.voice.starttime,Endtime=endtime,Raw.file.endtime=Raw.file.endtime,Starttime.flag=starttime.flag,Endtime.flag=Endtime.flag,Total.words,Words.in.processed.segment=Words.in.processed.segment,File.duration.in.seconds,Voice.only.duration.in.seconds=a$duration,Duration.of.ambient.audio=Duration.of.ambient.audio,Percentage.of.sample.with.voice,Within.words.sum.of.duration,Within.words.mean.of.duration,Within.words.SD.of.duration,Between.words.sum.pause.duration,Between.words.mean.pause.duration,Between.words.SD.of.pause.duration)
      features <- cbind(Text,features)
      features <- cbind(features,res)
    } else {features <- cbind(N.audiio.files.for.participant=N.audiio.files.for.participant,Very.quiet.flag=Very.quiet.flag,Starttime=NA,Combined.voice.starttime=NA,Endtime=NA,Raw.file.endtime=NA,Starttime.flag=TRUE,Endtime.flag=TRUE,Total.words=NA,Words.in.processed.segment=NA,File.duration.in.seconds,Voice.only.duration.in.seconds=NA,Duration.of.ambient.audio=NA,Percentage.of.sample.with.voice=NA,Within.words.sum.of.duration=NA,Within.words.mean.of.duration=NA,Within.words.SD.of.duration=NA,Between.words.sum.pause.duration=NA,Between.words.mean.pause.duration=NA,Between.words.SD.of.pause.duration=NA)
    res <- as.data.frame(t(rep(NA,6373))) 
    features <- cbind(Text,features)
    features <- cbind(features,res)
    }
    
    if(!exists('results.dataframe')){results.dataframe <- c()}
    results.dataframe <- data.table::rbindlist(list(results.dataframe,features),use.names=FALSE)
    
    if(!exists('ID.index')){ID.index <- c()}
    ID.index <- c(ID.index,files[i])
    if(!exists('ID.index.dates')){ID.index.dates <- c()}
    ID.index.dates <- c(ID.index.dates,names(Participant.s.RDS)[audiofile])
  }
  
  
  
}
} 
}
}
res.names <- unlist(as.list(sapply(0:135,function(index){as.character(res[2L][index])})))
res.names <- c(res.names,paste0("SD_",res.names))
new.df <- as.data.frame(results.dataframe)

new.df2 <- cbind(ID.index,ID.index.dates)
new.df2 <- cbind(new.df2,new.df)
opensmile2 <- rbind(opensmile,new.df2)
write.csv(opensmile2,r'(/Volumes/ROCKET-nano/Opensmile.features_active_voice_24692observation_over_980participants.csv)',row.names = F)
missings <- read.csv(file.choose())
full <- read.csv(r'(/Volumes/ROCKET-nano/All_audio_samples_opensmile_FA18.csv)')
full$merge.column <- paste0(full$ID.index,full$ID.index.dates)
new.df2$merge.column <- paste0(new.df2$ID.index,new.df2$ID.index.dates)
final.df <- merge(new.df2,full,by='merge.column',all = T)
plot(a$t,a$sig
)
