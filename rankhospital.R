rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
mydata<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
mydata<-mydata[,c(2,7,11,17,23)]
#get unique state and outcome
unique_state<-as.vector(unique(mydata$State))
outcome_f<-as.vector(c("heart attack","heart failure","pneumonia"))

## Check that state and outcome are valid
#Checking state
if (state %in% unique_state) {
sub_state<-subset(mydata, mydata$State == state)
#convert rate from char to numeric, surpressWarning to 
#turn off warning NA from R
sub_state[,3]<-suppressWarnings(as.numeric(sub_state[,3]))
sub_state[,4]<-suppressWarnings(as.numeric(sub_state[,4]))
sub_state[,5]<-suppressWarnings(as.numeric(sub_state[,5]))
}
else {
stop("invalid state")
}
#Cheking outcome
if (outcome %in% outcome_f) {
}
else {
stop("invalid outcome") }

## Return hospital name in that state with the given rank
## 30-day death rate
if (outcome == "heart attack") {
f_rate<-sub_state[complete.cases(sub_state[,3]),]
hosp_name<-f_rate[order(f_rate[,3], f_rate[,1], decreasing = F),c(1,3)]
}

else if (outcome == "heart failure") {
f_rate<-sub_state[complete.cases(sub_state[,4]),]
hosp_name<-f_rate[order(f_rate[,4], f_rate[,1], decreasing = F),c(1,4)]
}

else {
f_rate<-sub_state[complete.cases(sub_state[,5]),]
hosp_name<-f_rate[order(f_rate[,5], f_rate[,1], decreasing = F),c(1,5)]
}

#Return the last hospital
if (num == "best") {
hosp_name[1,1]     }
else if (num == "worst") 	  {
hosp_name[length(f_rate[,1]),1] }
else if (num>length(f_rate[,1]) ) {
NA                      }
else 		     {
hosp_name[num,1] }

}

rankhospital("TX","heart failure", num = 4)