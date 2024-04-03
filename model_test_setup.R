# Function to convert text time to seconds
textToSeconds <- function(textTime) {
  if (textTime == "0"){
    x = 0
  }
  else {
    seconds <- as.numeric(strsplit(textTime, split = ":")[[1]]) %*% c(60*60, 60, 1)
    x = as.numeric(seconds)
  }
  return(x)
}


# Function to categorize day into weekday or weekend (binary)
dayConvert <- function(day){
  y = 0
  # 1 if the day is weekday
  if (day == "Monday" | day == "Tuesday" | day == "Wednesday" | day == "Thursday" | day == "Friday"){
    y = 1
  }
  return(y)
}


# Function to categorize run time of a train into peak or off-peak (binary)
runTimeConvert <- function(time){
  z = 0
  # 1 if run time is peak
  if ((time>=10 & time<=11) | (time>=20 & time<=21)){
    z = 1
  }
  return(z) 
}


# Functions to convert the stops - it will check if a train stops at a particular stop.
NormStopConvert <- function(stops){
  x1 = 0
  if ("NORMNTN" %in% stops){
    x1 = 1
  }
  return(x1)
}

WakeStopConvert <- function(stops){
  x2 = 0
  if ("WKFLDKG" %in% stops){
    x2 = 1
  }
  return(x2)
}

BarnStopConvert <- function(stops){
  x3 = 0
  if ("BNSLY" %in% stops){
    x3 = 1
  }
  return(x3)
}

MeadStopConvert <- function(stops){
  x4 = 0
  if ("MEADWHL" %in% stops){
    x4 = 1
  }
  return(x4)
}


# Exploring train codes:
testcodes <- numeric(length=length(testData))
for (i in 1:length(testData)){
  testcodes[i] <- testData[[i]]$timings$train.code[1]
}

unique_test_trains <- unique(testcodes)


# Functions that returns 1 or 0 to train depending on the line it runs on
code_09 <- function(i){
  g1 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y09"){
    g1 <- 1
  }
  return(g1)
}

code_49 <- function(i){
  g2 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y49"){
    g2 <- 1
  }
  return(g2)
}

code_45 <- function(i){
  g3 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y45"){
    g3 <- 1
  }
  return(g3)
}

code_21 <- function(i){
  g4 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y21"){
    g4 <- 1
  }
  return(g4)
}

code_57 <- function(i){
  g5 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y57"){
    g5 <- 1
  }
  return(g5)
}

code_13 <- function(i){
  g6 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y13"){
    g6 <- 1
  }
  return(g6)
}

code_53 <- function(i){
  g7 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y53"){
    g7 <- 1
  }
  return(g7)
}

code_37 <- function(i){
  g8 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y37"){
    g8 <- 1
  }
  return(g8)
}

code_33 <- function(i){
  g9 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y33"){
    g9 <- 1
  }
  return(g9)
}

code_41 <- function(i){
  g10 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y41"){
    g10 <- 1
  }
  return(g10)
}

code_17 <- function(i){
  g11 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y17"){
    g11 <- 1
  }
  return(g11)
}

code_05 <- function(i){
  g12 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y05"){
    g12 <- 1
  }
  return(g12)
}

code_25 <- function(i){
  g13 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y25"){
    g13 <- 1
  }
  return(g13)
}

code_29 <- function(i){
  g14 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1Y29"){
    g14 <- 1
  }
  return(g14)
}

code_01 <- function(i){
  g15 <- 0
  tr_line <- testData[[i]]$timings$train.code[1]
  if (tr_line == "1G01"){
    g15 <- 1
  }
  return(g15)
}


# This is the initialisation of the design matrix
data_test <- data.frame(
  # Day
  day=rep(0, length(testData)),
  
  # Run time
  runTime=rep(0, length(testData)),
  
  # Stops
  stopNorm=rep(0, length(testData)),
  stopWake=rep(0, length(testData)),
  stopBarn=rep(0, length(testData)),
  stopMead=rep(0, length(testData)),
  
  # Congestions
  con_Leed=rep(0, length(testData)),
  con_Shef=rep(0, length(testData)),
  con_Nott=rep(0, length(testData)),
  con_avLeed=rep(0, length(testData)),
  con_avShef=rep(0, length(testData)),
  con_avNott=rep(0, length(testData)),
  
  # Train codes
  line_09 = rep(0, length(testData)),
  line_49 = rep(0, length(testData)),
  line_45 = rep(0, length(testData)),
  line_21 = rep(0, length(testData)),
  line_57 = rep(0, length(testData)),
  line_13 = rep(0, length(testData)),
  line_53 = rep(0, length(testData)),
  line_37 = rep(0, length(testData)),
  line_33 = rep(0, length(testData)),
  line_41 = rep(0, length(testData)),
  line_17 = rep(0, length(testData)),
  line_05 = rep(0, length(testData)),
  line_25 = rep(0, length(testData)),
  line_29 = rep(0, length(testData)),
  line_01 = rep(0, length(testData)),
  
  # Departure delays
  delayLeeds=rep(0, length(testData)),
  dep_delayNorm=rep(0, length(testData)),
  dep_delayWake=rep(0, length(testData)),
  dep_delayBarn=rep(0, length(testData)),
  dep_delayMead=rep(0, length(testData)),
  
  # Arrival delays
  arr_delayNorm=rep(0, length(testData)),
  arr_delayWake=rep(0, length(testData)),
  arr_delayBarn=rep(0, length(testData)),
  arr_delayMead=rep(0, length(testData)),
  delayShef=rep(0, length(testData)),
  
  stringsAsFactors=FALSE
)


# Stop Condition Converter:
# This function returns 0 for the departure/arrival time if the train does not stop at the particular station.
stopCondConv <- function(t){
  if (identical(t, character(0)) == TRUE){
    y = 0
  } else {
    y = t
  }
  return(y)
}

for (i in 1:length(testData)) {
  tp <- testData[[i]]
  
  # DAY
  data_test$day[i] <- dayConvert(tp$timings$day.week[1])
  
  
  # RUN TIME
  data_test$runTime[i] <- runTimeConvert(tp$congestion$hour)
  
  
  # CHECKING THE STOPS
  data_test$stopNorm[i] <- NormStopConvert(tp$timings$departure.from)
  data_test$stopWake[i] <- WakeStopConvert(tp$timings$departure.from)
  data_test$stopBarn[i] <- BarnStopConvert(tp$timings$departure.from)
  data_test$stopMead[i] <- MeadStopConvert(tp$timings$departure.from)
  
  
  # CONGESTIONS
  for (j in 1:length(historicalCongestion$Day)) {
    if ((historicalCongestion$Day[j] == tp$congestion$week.day) & (historicalCongestion$Hour[j] == tp$congestion$hour)) {
      data_test$con_Leed[i] <- -historicalCongestion$Leeds.trains[j] + tp$congestion$Leeds.trains
      data_test$con_Shef[i] <- -historicalCongestion$Sheffield.trains[j] + tp$congestion$Sheffield.trains
      data_test$con_Nott[i] <- -historicalCongestion$Nottingham.trains[j] + tp$congestion$Nottingham.trains
      data_test$con_avLeed[i] <- -historicalCongestion$Leeds.av.delay[j] + tp$congestion$Leeds.av.delay
      data_test$con_avShef[i] <- -historicalCongestion$Sheffield.av.delay[j] + tp$congestion$Sheffield.av.delay
      data_test$con_avNott[i] <- -historicalCongestion$Nottingham.av.delay[j] + tp$congestion$Nottingham.av.delay
      break
    }
  }
  
  
  # TRAIN CODES
  data_test$line_09[i] <- code_09(i)
  data_test$line_49[i] <- code_49(i)
  data_test$line_45[i] <- code_45(i)
  data_test$line_21[i] <- code_21(i)
  data_test$line_57[i] <- code_57(i)
  data_test$line_13[i] <- code_13(i)
  data_test$line_53[i] <- code_53(i)
  data_test$line_37[i] <- code_37(i)
  data_test$line_33[i] <- code_33(i)
  data_test$line_41[i] <- code_41(i)
  data_test$line_17[i] <- code_17(i)
  data_test$line_05[i] <- code_05(i)
  data_test$line_25[i] <- code_25(i)
  data_test$line_29[i] <- code_29(i)
  data_test$line_01[i] <- code_01(i)
  
  
  # Departure delay (for Leeds)
  depLeeds<- textToSeconds(head(tp$timings$departure.time,1))
  schLeeds <- textToSeconds(head(tp$timings$departure.schedule,1))
  data_test$delayLeeds[i] <- depLeeds - schLeeds
  
  # Arrival Delay (for Sheffield)
  arrSheffield <- textToSeconds(tail(tp$timings$arrival.time,1))
  schSheffield <- textToSeconds(tail(tp$timings$arrival.schedule,1))
  data_test$delayShef[i] <- arrSheffield - schSheffield
  
  
  # DEPARTURES
  
  # Actual departure times
  dept_Norm <- tp$timings$departure.time[tp$timings$departure.from == "NORMNTN"]
  dept_Wake <- tp$timings$departure.time[tp$timings$departure.from == "WKFLDKG"]
  dept_Barn <- tp$timings$departure.time[tp$timings$departure.from == "BNSLY"]
  dept_Mead <- tp$timings$departure.time[tp$timings$departure.from == "MEADWHL"]
  
  dept <- c(stopCondConv(dept_Norm), stopCondConv(dept_Wake), stopCondConv(dept_Barn), stopCondConv(dept_Mead))
  dept_seconds <- unlist(lapply(dept, textToSeconds))
  
  # Scheduled departure times
  schNorm <- tp$timings$departure.schedule[tp$timings$departure.from == "NORMNTN"]
  schWake <- tp$timings$departure.schedule[tp$timings$departure.from == "WKFLDKG"]
  schBarn <- tp$timings$departure.schedule[tp$timings$departure.from == "BNSLY"]
  schMead <- tp$timings$departure.schedule[tp$timings$departure.from == "MEADWHL"]
  
  sch <- c(stopCondConv(schNorm), stopCondConv(schWake), stopCondConv(schBarn), stopCondConv(schMead))
  sch_seconds <- unlist(lapply(sch, textToSeconds))
  
  # Departure delays
  data_test$dep_delayNorm[i] <- dept_seconds[1] - sch_seconds[1]
  data_test$dep_delayWake[i] <- dept_seconds[2] - sch_seconds[2]
  data_test$dep_delayBarn[i] <- dept_seconds[3] - sch_seconds[3]
  data_test$dep_delayMead[i] <- dept_seconds[4] - sch_seconds[4]
  
  
  # ARRIVALS
  
  # Actual arrival times
  arrt_Norm <- tp$timings$arrival.time[tp$timings$arrival.to == "NORMNTN"]
  arrt_Wake <- tp$timings$arrival.time[tp$timings$arrival.to == "WKFLDKG"]
  arrt_Barn <- tp$timings$arrival.time[tp$timings$arrival.to == "BNSLY"]
  arrt_Mead <- tp$timings$arrival.time[tp$timings$arrival.to == "MEADWHL"]
  
  arrt <- c(stopCondConv(arrt_Norm), stopCondConv(arrt_Wake), stopCondConv(arrt_Barn), stopCondConv(arrt_Mead))
  arrt_seconds <- unlist(lapply(arrt, textToSeconds))
  
  # Scheduled arrival times
  scha_Norm <- tp$timings$arrival.schedule[tp$timings$arrival.to == "NORMNTN"]
  scha_Wake <- tp$timings$arrival.schedule[tp$timings$arrival.to == "WKFLDKG"]
  scha_Barn <- tp$timings$arrival.schedule[tp$timings$arrival.to == "BNSLY"]
  scha_Mead <- tp$timings$arrival.schedule[tp$timings$arrival.to == "MEADWHL"]
  
  scha <- c(stopCondConv(scha_Norm),  stopCondConv(scha_Wake), stopCondConv(scha_Barn), stopCondConv(scha_Mead))
  scha_seconds <- unlist(lapply(scha, textToSeconds))
  
  # Arrival delays
  data_test$arr_delayNorm[i] <- arrt_seconds[1] - scha_seconds[1]
  data_test$arr_delayWake[i] <- arrt_seconds[2] - scha_seconds[2]
  data_test$arr_delayBarn[i] <- arrt_seconds[3] - scha_seconds[3]
  data_test$arr_delayMead[i] <- arrt_seconds[4] - scha_seconds[4]
}


# Save the design matrix to a file
save(data_test, file="test_Matrix.RData")
