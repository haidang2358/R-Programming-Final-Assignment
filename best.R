best <- function(state, outcome) {
## Read outcome data
mydata<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
mydata<-mydata[,c(2,7,11,17,23)]
#get unique state and outcome
unique_state<-as.vector(unique(mydata$State))
outcome_f<-as.vector(c("heart attack","heart failure","pneumonia"))

## Check that state and outcome are valid
if (state %in% unique_state) {
sub_state<-subset(mydata, mydata$State == state)
#convert rate from char to numeric
sub_state[,3]<-suppressWarnings(as.numeric(sub_state[,3]))
sub_state[,4]<-suppressWarnings(as.numeric(sub_state[,4]))
sub_state[,5]<-suppressWarnings(as.numeric(sub_state[,5]))
}
else {
stop("invalid state")
}

if (outcome %in% outcome_f) {
}
else {
stop("invalid outcome") }

## Return hospital name in that state with lowest 30-day death
if (outcome == "heart attack") {
f_rate<-sub_state[complete.cases(sub_state[,3]),]
hosp_name<-subset(f_rate[,1],f_rate[,3] == min(f_rate[,3]))
#get last hospital by sorting by alphabet
f_hospital<-sort(hosp_name)[1]
}
else if (outcome == "heart failure") {
f_rate<-sub_state[complete.cases(sub_state[,4]),]
hosp_name<-subset(f_rate[,1],f_rate[,4] == min(f_rate[,4]))
#get last hospital by sorting by alphabet
f_hospital<-sort(hosp_name)[1]
}
else {
f_rate<-sub_state[complete.cases(sub_state[,5]),]
hosp_name<-subset(f_rate[,1],f_rate[,5] == min(f_rate[,5]))
#get last hospital by sorting by alphabet
f_hospital<-sort(hosp_name)[1]
}
#Return the last hospital
f_hospital
}

#Test cases
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
