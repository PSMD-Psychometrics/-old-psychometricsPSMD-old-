
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Jo's Functions ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Converting integers to characters (<10)                          ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Function to convert integers <10 to strings
fnNumToWord<-function(x){
  ifelse(x==0,"no",ifelse(x==1,"one",ifelse(x==2,"two",ifelse(x==3,"three",ifelse(x==4,"four",ifelse(x==5,"five",
  ifelse(x==6,"six",ifelse(x==7,"seven",ifelse(x==8,"eight",ifelse(x==9,"nine",x))))))))))
}

fnNumToWordCap<-function(x){
  ifelse(x==0,"no",ifelse(x==1,"One",ifelse(x==2,"Two",ifelse(x==3,"Three",ifelse(x==4,"Four",ifelse(x==5,"Five",
  ifelse(x==6,"Six",ifelse(x==7,"Seven",ifelse(x==8,"Eight",ifelse(x==9,"Nine",x))))))))))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Plot - boxplots                                                  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fnPlotBoxplot1<-function(dataframe,xGroup="Stage",yGroup="Score",maxScore=100){
  
  xAxis<-dataframe[[xGroup]]
  yAxis<-dataframe[[yGroup]]
  
  dataPlot<-data.frame(xAxis,yAxis)

# code to identify xScheme from the data
  
  #if(class(dataPlot[,1]=="numeric" & max(dataPlot[,1]>5)){xScheme<-"Assessor"}
  #if(class(dataPlot[,1]=="numeric" & max(dataPlot[,1]<6)){xScheme<-"Stage"}
    
  if(class(dataPlot[,1])=="numeric") {xScheme<-ifelse(max(dataPlot[,1]>5),"Assessor","Stage")}
    
  if(class(dataPlot[,1])=="character" | class(dataPlot[,1])=="factor") {
  dataPlot$Code<-substr(dataPlot[,1],1,1)
  variables<-as.character(unique(dataPlot[,1])) # a list of the unique (non-numeric) variables values in this df          variables<-variables[sort.list(variables)]    # sort alphabetically
  variablesCode<-sort(variables)
   
  
  # loop to get grades as a string
  groupCode<-""
  for (i in 1:length(variablesCode)){           # loop to bring it together as a groupCode
         text<-variablesCode[i]
         groupCode<-paste(groupCode,text,sep="") }
  
  groupCode<<-groupCode
  
  if(groupCode=="BESU"){xScheme<-"UBSE"}
  if(groupCode=="BES") {xScheme<-"UBSE"}
  if(groupCode=="ESU") {xScheme<-"USE"}
  if(groupCode=="EFP") {xScheme<-"PassFailEx"}
  if(groupCode=="FP")  {xScheme<-"PassFail"}
  if(groupCode=="P")   {xScheme<-"PassFail"}
  if(groupCode=="ES")  {xScheme<-"UBSE"}  # add a message to check right group

  
# Convert all abbreviated grades in data (e.g. "U","B","S","E") to full strings ("Unsatisfactory","Borderline","Satisfactory","Excellent")
  maxNChar<-max(nchar(as.character(dataPlot[,1]))) # to find the longest character string in Grade column
  if(xScheme=="UBSE" | xScheme=="USE" & maxNChar==1){dataPlot[,3][dataPlot[,3]=="U"]<-"Unsatisfactory"}
  if(xScheme=="UBSE"                  & maxNChar==1){dataPlot[,3][dataPlot[,3]=="B"]<-"Borderline"}
  if(xScheme=="UBSE" | xScheme=="USE" & maxNChar==1){dataPlot[,3][dataPlot[,3]=="S"]<-"Satisfactory"}
  if(xScheme=="UBSE" | xScheme=="USE" & maxNChar==1){dataPlot[,3][dataPlot[,3]=="E"]<-"Excellent"}
  
  #if(xScheme=="UBSE" | xScheme=="USE"){dataPlot[,3][dataPlot[,3]=="U"]<-"Unsatisfactory"}
  #if(xScheme=="UBSE"                 ){dataPlot[,3][dataPlot[,3]=="B"]<-"Borderline"}
  #if(xScheme=="UBSE" | xScheme=="USE"){dataPlot[,3][dataPlot[,3]=="S"]<-"Satisfactory"}
  #if(xScheme=="UBSE" | xScheme=="USE"){dataPlot[,3][dataPlot[,3]=="E"]<-"Excellent"}
  
  #if(xScheme=="PassFailEx" | xScheme=="PassFail"){dataPlot[,3][dataPlot[,3]=="P"]<-"Pass"}
  #if(xScheme=="PassFailEx" | xScheme=="PassFail"){dataPlot[,3][dataPlot[,3]=="F"]<-"Fail"}
  #if(xScheme=="PassFailEx"                      ){dataPlot[,3][dataPlot[,3]=="E"]<-"Excellent"}
  
  if(xScheme=="PassFailEx" | xScheme=="PassFail" & maxNChar==1){dataPlot[,3][dataPlot[,3]=="P"]<-"Pass"}
  if(xScheme=="PassFailEx" | xScheme=="PassFail" & maxNChar==1){dataPlot[,3][dataPlot[,3]=="F"]<-"Fail"}
  if(xScheme=="PassFailEx"                       & maxNChar==1){dataPlot[,3][dataPlot[,3]=="E"]<-"Excellent"}
  
  dataPlot$xAxis<-dataPlot[,3]
  
  } 
  
  xScheme<<-xScheme
  
# need to build in error/warning messages with query if function is not sure (e.g. allocate to UBSE but ask of correct and if not to add #something 'Force.Scheme="USE") 
    
  if(xScheme=="UBSE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Borderline","Satisfactory","Excellent"),ordered=TRUE )}
  if(xScheme=="USE"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Unsatisfactory","Satisfactory","Excellent"),ordered=TRUE)}
  if(xScheme=="PassFailEx"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Fail","Pass","Excellent"),ordered=TRUE)}
  if(xScheme=="PassFail"){dataPlot$xAxis<-factor(dataPlot$xAxis, levels=c("Fail","Pass"),ordered=TRUE)}
  if(xScheme=="Stage"){dataPlot$xAxis<-factor(dataPlot$xAxis)}  # levels will be numerical ascending for whichever Stage groups are in the data
  if(xScheme=="Assessor"){dataPlot$xAxis<-factor(dataPlot$xAxis)}

### 
##### need to calculate the median values within the function to enable ordering by these  
### 
  if(xScheme=="Assessor") {
  oind<-order(as.numeric(by(dataPlot$yAxis,dataPlot$xAxis,median)))
  dataPlot$xAxis<-ordered(dataPlot$xAxis,levels=levels(dataPlot$xAxis)[oind])
  }


  #  dataPlot$xAxis<-Data.Example$Assessor.ID
   # factor the numeric ID
  #if(xScheme=="Assessor"){orderAssessors<- order(as.numeric(by(dataPlot$yAxis,dataPlot$xAxis,median)))} # define the order of assessors by median ascending 
  #if(xScheme=="Assessor"){dataPlot$xAxis<-dataPlot$xAxis,levels=c(orderAssessors),ordered=TRUE)}
  #dataAssessors$Assessor <- ordered(dataAssessors$Assessor, levels=levels(dataAssessors$Assessor)[oind])
  
  
  
  
# Colours for each group
  if(xScheme=="UBSE"){colFill<-c("red","orange","#117733","#332288")}
  if(xScheme=="USE"){colFill<-c("red","#117733","#332288")}
  if(xScheme=="PassFailEx"){colFill<-c("red","#117733","#332288")}
  if(xScheme=="PassFail"){colFill<-c("red","#117733")}
  if(xScheme=="Stage"){colFill<-"maroon"} 
  if(xScheme=="Assessor"){colFill<-"darkgrey"}
 
  dataPlot<<-dataPlot
  
  ggplot(dataPlot, aes(x=xAxis, y=yAxis, group=xAxis)) +
    geom_boxplot(fill=colFill) +
    expand_limits(y=c(0,maxScore)) +
    ylab(ifelse(maxScore==100,"Score (%)","Score")) +
    xlab(ifelse(xScheme=="Assessor",xScheme,ifelse(xScheme=="Stage",xScheme,"Grade"))) +
	stat_summary(fun.y="mean", geom="point", shape=8, size=3.5, position=position_dodge(width=0.75), color="white") +
    theme_psmd()
  
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
##
##
##
##
##
####
###
##
#
