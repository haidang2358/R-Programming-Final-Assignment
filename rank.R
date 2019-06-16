rankall <- function(outcome, num = "best") {
## Read outcome data
mydata<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
mydata<-mydata[,c(2,7,11,17,23)]
names(mydata)<-c("Hospital", "State","HA", "HF","PN")
## Check that state and outcome are valid
outcome_f<-as.vector(c("heart attack","heart failure","pneumonia"))
if (outcome %in% outcome_f) {}
else {
stop("invalid outcome") }

#convert rate from char to numeric
mydata$HA<-suppressWarnings(as.numeric(mydata$HA))
mydata$HF<-suppressWarnings(as.numeric(mydata$HF))
mydata$PN<-suppressWarnings(as.numeric(mydata$PN))

##get final data as specific outcome
fdata_ha<-mydata[complete.cases(mydata$HA), c(1,2,3)]
fdata_hf<-mydata[complete.cases(mydata$HF), c(1,2,4)]
fdata_pn<-mydata[complete.cases(mydata$PN), c(1,2,5)]


Hospital<-vector()
State<-vector()
list_hospital <- data.frame(Hospital,State)

############################################################## 
## For each state, find the hospital of the given rank 
if (outcome == "heart attack")
{
state<-fdata_ha$State
#split into a list of dataframes
spl_fdata_ha<-split(fdata_ha, state)
#order rate and name of hospital in list
spl_fdata_ha<-lapply(spl_fdata_ha, function(x) x[with(x,order(x[,3],x[,1])),] )

#return the final list of hospial by rank by combine each hospital in state
for (i in 1:length(spl_fdata_ha)) 	{

if (num == "best" ) {l <- spl_fdata_ha[[i]][1,] }

else if (num == "worst") 	{
num <- length( spl_fdata_ha[[i]]$State )
l <- spl_fdata_ha[[i]][num,]	}

else if (num > length( spl_fdata_ha[[i]]$State ) )        {
l <- c( "NA", spl_fdata_ha[[i]]$State[1], "NA" )   }

else {
l<-spl_fdata_ha[[i]][num, ] }

Hospital<-l[1]
State<-l[2]
data<-cbind(Hospital,State)
list_hospital <- rbind(list_hospital,data)

}
f_hostpital<-list_hospital
}
############################################################


if (outcome == "heart failure")
{
state<-fdata_hf$State
#split into a list of dataframes
spl_fdata_hf<-split(fdata_hf, state)
#order rate and name of hospital in list
spl_fdata_hf<-lapply(spl_fdata_hf, function(x) x[with(x,order(x[,3],x[,1])),] )
#return the final list of hospial by rank by combine each hospital in state
for (i in 1:length(spl_fdata_hf)  )       {
if (num == "best" ) {l <- spl_fdata_hf[[i]][1,] }

else if (num == "worst") 	{
num <- length( spl_fdata_hf[[i]]$State )
l <- spl_fdata_hf[[i]][num,]	}

else if (num > length( spl_fdata_hf[[i]]$State ) )        {
l <- c( "NA", spl_fdata_hf[[i]]$State[1], "NA" )   }

else {
l<-spl_fdata_hf[[i]][num, ] }

Hospital<-l[1]
State<-l[2]
data<-cbind(Hospital,State)
list_hospital <- rbind(list_hospital,data)
}
f_hostpital<-list_hospital
}
 
#########################################################################

if (outcome == "pneumonia")
{
state<-fdata_pn$State
#split into a list of dataframes
spl_fdata_pn<-split(fdata_pn, state)
#order rate and name of hospital in list
spl_fdata_pn<-lapply(spl_fdata_pn, function(x) x[with(x,order(x[,3],x[,1])),])
#return the final list of hospial by rank by combine each hospital in state
for (i in 1:length(spl_fdata_pn)  )       {
if (num == "best" ) {l <- spl_fdata_pn[[i]][1,] }

else if (num == "worst") 	{
#num <- length( spl_fdata_pn[[i]]$State )
l <- tail(spl_fdata_pn[[i]],1)	}

else if (num > length( spl_fdata_pn[[i]]$State ) )        {
l <- c( "NA", spl_fdata_pn[[i]]$State[1], "NA" )         }

else                        {
l<-spl_fdata_pn[[i]][num, ] }

Hospital<-l[1]
State<-l[2]
data<-cbind(Hospital,State)
list_hospital <- rbind(list_hospital,data)
}
f_hostpital<-list_hospital
}
f_hostpital


}

## Return a data frame with the hospital names and the
## (abbreviated) state name

head(rankall("heart attack", 20),10)

tail(rankall("pneumonia", "worst"), 3)
