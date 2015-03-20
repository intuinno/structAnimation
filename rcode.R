#------------------ LIBRARIES --------------------------#
# install.packages("ggplot2")
library(ggplot2)

# install.packages("boot")
library(boot)

# install.packages("gridExtra")
library(gridExtra)
#-------------------------------------------------------#


#--------------- USEFUL FUNCTIONS ----------------------#


aggregateParticipant <- function(x,measure,f) {
  if (f=="mean")
    return (aggregateMean(x,measure))
  else
    return (aggregateMedian(x,measure))
}


###### get mean per participant
aggregateMean <- function(x, measure) {
  aggdata <- aggregate(x[,measure] ~ participant, data = x, mean)
  return (aggdata)
}


###### get median per participant
aggregateMedian <- function(x, measure) {
  aggdata <- aggregate(x[,measure] ~ participant, data = x, median)
  return (aggdata)
}



### get confidence interval
getCI <- function(x,f) {
  if (f == "mean")
    return(getCIMean(x))
  else 	# median
    return(getCIMedian(x))
}

### get Confidence interval (mean)
getCIMean <- function(x) {
  number.measures <- length(x)
  number.samples <- 5000
  
  sample.mean <- function(x, index) {
    return(mean(x[index]))
  }
  boot.object <- boot(x, sample.mean, R = number.samples)
  confidence.intervals <- quantile(boot.object$t, c(0.025, 0.975))  #the samples are in boot.object$t
  boot.object$t0  # the mean
  return (confidence.intervals)
}

### get Confidence interval (median)
getCIMedian <- function(x) {
  number.measures <- length(x)
  number.samples <- 5000
  
  sample.median <- function(x, index) {
    return(median(x[index]))
  }
  boot.object <- boot(x, sample.median, R = number.samples)
  confidence.intervals <- quantile(boot.object$t, c(0.025, 0.975))  #the samples are in boot.object$t
  boot.object$t0  # the median
  return (confidence.intervals)
} 	

## get Lower value of a CI, 
# Param: column=<normalized_error, accuracy>, f=<"mean", "median">
getLowerCI <- function(x, f) {
  ci <- getCI(x,f)
  ci <- as.data.frame(ci)
  return(ci[1,1])
}


getUpperCI <- function(x, f) {
  ci <- getCI(x,f)
  ci <- as.data.frame(ci)
  return(ci[2,1])
}



#--------------- USEFUL FUNCTIONS ----------------------#



logs <- "/Users/intuinno/codegit/structAnimation/logs.csv"
data = read.csv(logs, sep=";", head=TRUE)

task_SSSP <- data[data$task == 'SSSP',]
task_SSSL <- data[data$task == 'SLSS',]
task_SSDP <- data[data$task == 'SSDP',]
task_SSDL <- data[data$task == 'SSDL',]
task_SPSL <- data[data$task == 'SPSL',]
task_SPDS <- data[data$task == 'SPDS',]
task_SPDL <- data[data$task == 'SPDL',]
task_SLDS <- data[data$task == 'SLDS',]
task_SLDP <- data[data$task == 'SLDP',]
task_DSDP <- data[data$task == 'DPDS',]
task_DSDL <- data[data$task == 'DLDS',]
task_DPDL <- data[data$task == 'DPDL',]

### SANITY CHECK ###
NB_PARTICIPANTS = 100
NB_REPS = 3
NB_CONDITIONS = 12

if (length(task_SSSP[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SSSP")
if (length(task_SSSL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SSSL")
if (length(task_SSDP[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SSDP")
if (length(task_SSDL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SSDL")
if (length(task_SPSL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SPSL")
if (length(task_SPDS[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SPDS")
if (length(task_SPDL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SPDL")
if (length(task_SLDS[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SLDS")
if (length(task_SLDP[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task SLDP")
if (length(task_DSDP[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task DSDP")
if (length(task_DSDL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task DSDL")
if (length(task_DPDL[,1]) != NB_PARTICIPANTS*NB_REPS)
  cat("Warning: wrong number or records for task DPDL")

### Aggregate accuracies per participant for each task and variable


################### FOCUS ON SS #############################


### SS vs. SP

SSSP_SS_accu <- aggregateParticipant(task_SSSP,'SS','mean')
SSSP_SP_accu <- aggregateParticipant(task_SSSP,'SP','mean')
SSSP_diff <- SSSP_SP_accu - SSSP_SS_accu

SSSP_SS_point <- c(mean(SSSP_SS_accu[,2]))
SSSP_SS_lower_CI <- c(getLowerCI(SSSP_SS_accu[,2],'mean'))
SSSP_SS_upper_CI <- c(getUpperCI(SSSP_SS_accu[,2],'mean'))


SSSP_SP_point <- c(mean(SSSP_SP_accu[,2]))
SSSP_SP_lower_CI <- c(getLowerCI(SSSP_SP_accu[,2],'mean'))
SSSP_SP_upper_CI <- c(getUpperCI(SSSP_SP_accu[,2],'mean'))

SSSP_diff_point <- c(mean(SSSP_diff[,2]))
SSSP_diff_lower_CI <- c(getLowerCI(SSSP_diff[,2],'mean'))
SSSP_diff_upper_CI <- c(getUpperCI(SSSP_diff[,2],'mean'))



## CHECK GRAPH
points <- c(SSSP_SS_point, SSSP_SP_point)
upper_CIs <- c(SSSP_SS_upper_CI, SSSP_SP_upper_CI)
lower_CIs <- c(SSSP_SS_lower_CI, SSSP_SP_lower_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = points,
  y = c("SS","SP"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2)

dfr



p <- ggplot(dfr, aes(x, y_numeric)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_y_discrete(breaks=c(1,2), labels=c("SS", "SP")) +	
  xlab("accuracies") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Accuracies for SS vs. something")




### SS vs. SL

SSSL_SS_accu <- aggregateParticipant(task_SSSL,'SS','mean')
SSSL_SL_accu <- aggregateParticipant(task_SSSL,'SL','mean')
SSSL_diff <- SSSL_SL_accu - SSSL_SS_accu

SSSL_SS_point <- c(mean(SSSL_SS_accu[,2]))
SSSL_SS_lower_CI <- c(getLowerCI(SSSL_SS_accu[,2],'mean'))
SSSL_SS_upper_CI <- c(getUpperCI(SSSL_SS_accu[,2],'mean'))


SSSL_SL_point <- c(mean(SSSL_SL_accu[,2]))
SSSL_SL_lower_CI <- c(getLowerCI(SSSL_SL_accu[,2],'mean'))
SSSL_SL_upper_CI <- c(getUpperCI(SSSL_SL_accu[,2],'mean'))

SSSL_diff_point <- c(mean(SSSL_diff[,2]))
SSSL_diff_lower_CI <- c(getLowerCI(SSSL_diff[,2],'mean'))
SSSL_diff_upper_CI <- c(getUpperCI(SSSL_diff[,2],'mean'))


#### CHECK 1 GRAPH
points <- c(SSSL_SS_point, SSSL_SL_point)
upper_CIs <- c(SSSL_SS_upper_CI, SSSL_SL_upper_CI)
lower_CIs <- c(SSSL_SS_lower_CI, SSSL_SL_lower_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = points,
  y = c("SS","SL"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2)

dfr



p <- ggplot(dfr, aes(x, y_numeric)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_y_discrete(breaks=c(1,2), labels=c("SS", "SL")) +	
  xlab("accuracies") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Accuracies for SS vs. something")




### SS vs. DP

SSDP_SS_accu <- aggregateParticipant(task_SSDP,'SS','mean')
SSDP_DP_accu <- aggregateParticipant(task_SSDP,'DP','mean')
SSDP_diff <- SSDP_DP_accu - SSDP_SS_accu

SSDP_SS_point <- c(mean(SSDP_SS_accu[,2]))
SSDP_SS_lower_CI <- c(getLowerCI(SSDP_SS_accu[,2],'mean'))
SSDP_SS_upper_CI <- c(getUpperCI(SSDP_SS_accu[,2],'mean'))


SSDP_DP_point <- c(mean(SSDP_DP_accu[,2]))
SSDP_DP_lower_CI <- c(getLowerCI(SSDP_DP_accu[,2],'mean'))
SSDP_DP_upper_CI <- c(getUpperCI(SSDP_DP_accu[,2],'mean'))

SSDP_diff_point <- c(mean(SSDP_diff[,2]))
SSDP_diff_lower_CI <- c(getLowerCI(SSDP_diff[,2],'mean'))
SSDP_diff_upper_CI <- c(getUpperCI(SSDP_diff[,2],'mean'))

### SS vs. DL

SSDL_SS_accu <- aggregateParticipant(task_SSDL,'SS','mean')
SSDL_DL_accu <- aggregateParticipant(task_SSDL,'DL','mean')
SSDL_diff <- SSDL_DL_accu - SSDL_SS_accu

SSDL_SS_point <- c(mean(SSDL_SS_accu[,2]))
SSDL_SS_lower_CI <- c(getLowerCI(SSDL_SS_accu[,2],'mean'))
SSDL_SS_upper_CI <- c(getUpperCI(SSDL_SS_accu[,2],'mean'))


SSDL_DL_point <- c(mean(SSDL_DL_accu[,2]))
SSDL_DL_lower_CI <- c(getLowerCI(SSDL_DL_accu[,2],'mean'))
SSDL_DL_upper_CI <- c(getUpperCI(SSDL_DL_accu[,2],'mean'))


SSDL_diff_point <- c(mean(SSDL_diff[,2]))
SSDL_diff_lower_CI <- c(getLowerCI(SSDL_diff[,2],'mean'))
SSDL_diff_upper_CI <- c(getUpperCI(SSDL_diff[,2],'mean'))


##### Plot Difference
points <- c(SSSP_diff_point, SSSL_diff_point, SSDP_diff_point, SSDL_diff_point)
lower_CIs <- c(SSSP_diff_lower_CI, SSSL_diff_lower_CI, SSDP_diff_lower_CI, SSDL_diff_lower_CI)
upper_CIs <- c(SSSP_diff_upper_CI, SSSL_diff_upper_CI, SSDP_diff_upper_CI, SSDL_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("SS vs. SP","SS vs. SL","SS vs. DP", "SS vs. DL"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pSSdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SS / SP","SS / SL","SS / DP", "SS / DL")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over SS")




##### Plot Versus

SS_points <- c(SSSP_SS_point, SSSL_SS_point, SSDP_SS_point, SSDL_SS_point)
SS_lower_CIs <- c(SSSP_SS_lower_CI, SSSL_SS_lower_CI, SSDP_SS_lower_CI, SSDL_SS_lower_CI)
SS_upper_CIs <- c(SSSP_SS_upper_CI, SSSL_SS_upper_CI, SSDP_SS_upper_CI, SSDL_SS_upper_CI)

vs_points <- c(SSSP_SP_point, SSSL_SL_point, SSDP_DP_point, SSDL_DL_point)
vs_lower_CIs <- c(SSSP_SP_lower_CI, SSSL_SL_lower_CI, SSDP_DP_lower_CI, SSDL_DL_lower_CI)
vs_upper_CIs <- c(SSSP_SP_upper_CI, SSSL_SL_upper_CI, SSDP_DP_upper_CI, SSDL_DL_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(SS_points,vs_points),
  y = c("SS", "SS", "SS", "SS", "SP", "SL", "DP", "DL"),
  upper = c(SS_upper_CIs, vs_upper_CIs),
  lower = c(SS_lower_CIs, vs_lower_CIs),
  variable = rep(c("SS","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pSSvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=1) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SS vs. SP","SS vs. SL","SS vs. DP", "SS vs. DL")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")







################### FOCUS ON SP #############################


### SP vs. SS

SPSS_SP_accu <- aggregateParticipant(task_SSSP,'SP','mean')
SPSS_SS_accu <- aggregateParticipant(task_SSSP,'SS','mean')
SPSS_diff <- SPSS_SS_accu - SPSS_SP_accu

SPSS_SP_point <- c(mean(SPSS_SP_accu[,2]))
SPSS_SP_lower_CI <- c(getLowerCI(SPSS_SP_accu[,2],'mean'))
SPSS_SP_upper_CI <- c(getUpperCI(SPSS_SP_accu[,2],'mean'))


SPSS_SS_point <- c(mean(SPSS_SS_accu[,2]))
SPSS_SS_lower_CI <- c(getLowerCI(SPSS_SS_accu[,2],'mean'))
SPSS_SS_upper_CI <- c(getUpperCI(SPSS_SS_accu[,2],'mean'))

SPSS_diff_point <- c(mean(SPSS_diff[,2]))
SPSS_diff_lower_CI <- c(getLowerCI(SPSS_diff[,2],'mean'))
SPSS_diff_upper_CI <- c(getUpperCI(SPSS_diff[,2],'mean'))



### SP vs. SL

SPSL_SP_accu <- aggregateParticipant(task_SPSL,'SP','mean')
SPSL_SL_accu <- aggregateParticipant(task_SPSL,'SL','mean')
SPSL_diff <- SPSL_SL_accu - SPSL_SP_accu

SPSL_SP_point <- c(mean(SPSL_SP_accu[,2]))
SPSL_SP_lower_CI <- c(getLowerCI(SPSL_SP_accu[,2],'mean'))
SPSL_SP_upper_CI <- c(getUpperCI(SPSL_SP_accu[,2],'mean'))


SPSL_SL_point <- c(mean(SPSL_SL_accu[,2]))
SPSL_SL_lower_CI <- c(getLowerCI(SPSL_SL_accu[,2],'mean'))
SPSL_SL_upper_CI <- c(getUpperCI(SPSL_SL_accu[,2],'mean'))

SPSL_diff_point <- c(mean(SPSL_diff[,2]))
SPSL_diff_lower_CI <- c(getLowerCI(SPSL_diff[,2],'mean'))
SPSL_diff_upper_CI <- c(getUpperCI(SPSL_diff[,2],'mean'))


### SP vs. DS

SPDS_SP_accu <- aggregateParticipant(task_SPDS,'SP','mean')
SPDS_DS_accu <- aggregateParticipant(task_SPDS,'DS','mean')
SPDS_diff <- SPDS_DS_accu - SPDS_SP_accu

SPDS_SP_point <- c(mean(SPDS_SP_accu[,2]))
SPDS_SP_lower_CI <- c(getLowerCI(SPDS_SP_accu[,2],'mean'))
SPDS_SP_upper_CI <- c(getUpperCI(SPDS_SP_accu[,2],'mean'))


SPDS_DS_point <- c(mean(SPDS_DS_accu[,2]))
SPDS_DS_lower_CI <- c(getLowerCI(SPDS_DS_accu[,2],'mean'))
SPDS_DS_upper_CI <- c(getUpperCI(SPDS_DS_accu[,2],'mean'))

SPDS_diff_point <- c(mean(SPDS_diff[,2]))
SPDS_diff_lower_CI <- c(getLowerCI(SPDS_diff[,2],'mean'))
SPDS_diff_upper_CI <- c(getUpperCI(SPDS_diff[,2],'mean'))

### SP vs. DL

SPDL_SP_accu <- aggregateParticipant(task_SPDL,'SP','mean')
SPDL_DL_accu <- aggregateParticipant(task_SPDL,'DL','mean')
SPDL_diff <- SPDL_DL_accu - SPDL_SP_accu

SPDL_SP_point <- c(mean(SPDL_SP_accu[,2]))
SPDL_SP_lower_CI <- c(getLowerCI(SPDL_SP_accu[,2],'mean'))
SPDL_SP_upper_CI <- c(getUpperCI(SPDL_SP_accu[,2],'mean'))


SPDL_DL_point <- c(mean(SPDL_DL_accu[,2]))
SPDL_DL_lower_CI <- c(getLowerCI(SPDL_DL_accu[,2],'mean'))
SPDL_DL_upper_CI <- c(getUpperCI(SPDL_DL_accu[,2],'mean'))


SPDL_diff_point <- c(mean(SPDL_diff[,2]))
SPDL_diff_lower_CI <- c(getLowerCI(SPDL_diff[,2],'mean'))
SPDL_diff_upper_CI <- c(getUpperCI(SPDL_diff[,2],'mean'))


##### Plot Difference
points <- c(SPSS_diff_point, SPSL_diff_point, SPDS_diff_point, SPDL_diff_point)
lower_CIs <- c(SPSS_diff_lower_CI, SPSL_diff_lower_CI, SPDS_diff_lower_CI, SPDL_diff_lower_CI)
upper_CIs <- c(SPSS_diff_upper_CI, SPSL_diff_upper_CI, SPDS_diff_upper_CI, SPDL_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("SP vs. SP","SP vs. SL","SP vs. DS", "SP vs. DL"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pSPdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SP / SS","SP / SL","SP / DS", "SP / DL")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over SP")




##### Plot Versus

SP_points <- c(SPSS_SP_point, SPSL_SP_point, SPDS_SP_point, SPDL_SP_point)
SP_lower_CIs <- c(SPSS_SP_lower_CI, SPSL_SP_lower_CI, SPDS_SP_lower_CI, SPDL_SP_lower_CI)
SP_upper_CIs <- c(SPSS_SP_upper_CI, SPSL_SP_upper_CI, SPDS_SP_upper_CI, SPDL_SP_upper_CI)

vs_points <- c(SPSS_SP_point, SPSL_SL_point, SPDS_DS_point, SPDL_DL_point)
vs_lower_CIs <- c(SPSS_SP_lower_CI, SPSL_SL_lower_CI, SPDS_DS_lower_CI, SPDL_DL_lower_CI)
vs_upper_CIs <- c(SPSS_SP_upper_CI, SPSL_SL_upper_CI, SPDS_DS_upper_CI, SPDL_DL_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(SP_points,vs_points),
  y = c("SP", "SP", "SP", "SP", "SS", "SL", "DS", "DL"),
  upper = c(SP_upper_CIs, vs_upper_CIs),
  lower = c(SP_lower_CIs, vs_lower_CIs),
  variable = rep(c("SP","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pSPvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SP vs. SS","SP vs. SL","SP vs. DS", "SP vs. DL")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")










################### FOCUS ON SL #############################



### SL vs. SP

SLSP_SL_accu <- aggregateParticipant(task_SPSL,'SL','mean')
SLSP_SP_accu <- aggregateParticipant(task_SPSL,'SP','mean')
SLSP_diff <- SLSP_SP_accu - SLSP_SL_accu

SLSP_SL_point <- c(mean(SLSP_SL_accu[,2]))
SLSP_SL_lower_CI <- c(getLowerCI(SLSP_SL_accu[,2],'mean'))
SLSP_SL_upper_CI <- c(getUpperCI(SLSP_SL_accu[,2],'mean'))

SLSP_SP_point <- c(mean(SLSP_SP_accu[,2]))
SLSP_SP_lower_CI <- c(getLowerCI(SLSP_SP_accu[,2],'mean'))
SLSP_SP_upper_CI <- c(getUpperCI(SLSP_SP_accu[,2],'mean'))


SLSP_diff_point <- c(mean(SLSP_diff[,2]))
SLSP_diff_lower_CI <- c(getLowerCI(SLSP_diff[,2],'mean'))
SLSP_diff_upper_CI <- c(getUpperCI(SLSP_diff[,2],'mean'))

### SL vs. SS

SLSS_SL_accu <- aggregateParticipant(task_SSSL,'SL','mean')
SLSS_SS_accu <- aggregateParticipant(task_SSSL,'SS','mean')
SLSS_diff <- SLSS_SS_accu - SLSS_SL_accu

SLSS_SL_point <- c(mean(SLSS_SL_accu[,2]))
SLSS_SL_lower_CI <- c(getLowerCI(SLSS_SL_accu[,2],'mean'))
SLSS_SL_upper_CI <- c(getUpperCI(SLSS_SL_accu[,2],'mean'))


SLSS_SS_point <- c(mean(SLSS_SS_accu[,2]))
SLSS_SS_lower_CI <- c(getLowerCI(SLSS_SS_accu[,2],'mean'))
SLSS_SS_upper_CI <- c(getUpperCI(SLSS_SS_accu[,2],'mean'))

SLSS_diff_point <- c(mean(SLSS_diff[,2]))
SLSS_diff_lower_CI <- c(getLowerCI(SLSS_diff[,2],'mean'))
SLSS_diff_upper_CI <- c(getUpperCI(SLSS_diff[,2],'mean'))




### SL vs. DS

SLDS_SL_accu <- aggregateParticipant(task_SLDS,'SL','mean')
SLDS_DS_accu <- aggregateParticipant(task_SLDS,'DS','mean')
SLDS_diff <- SLDS_DS_accu - SLDS_SL_accu

SLDS_SL_point <- c(mean(SLDS_SL_accu[,2]))
SLDS_SL_lower_CI <- c(getLowerCI(SLDS_SL_accu[,2],'mean'))
SLDS_SL_upper_CI <- c(getUpperCI(SLDS_SL_accu[,2],'mean'))


SLDS_DS_point <- c(mean(SLDS_DS_accu[,2]))
SLDS_DS_lower_CI <- c(getLowerCI(SLDS_DS_accu[,2],'mean'))
SLDS_DS_upper_CI <- c(getUpperCI(SLDS_DS_accu[,2],'mean'))

SLDS_diff_point <- c(mean(SLDS_diff[,2]))
SLDS_diff_lower_CI <- c(getLowerCI(SLDS_diff[,2],'mean'))
SLDS_diff_upper_CI <- c(getUpperCI(SLDS_diff[,2],'mean'))

### SL vs. DP

SLDP_SL_accu <- aggregateParticipant(task_SLDP,'SL','mean')
SLDP_DP_accu <- aggregateParticipant(task_SLDP,'DP','mean')
SLDP_diff <- SLDP_DP_accu - SLDP_SL_accu

SLDP_SL_point <- c(mean(SLDP_SL_accu[,2]))
SLDP_SL_lower_CI <- c(getLowerCI(SLDP_SL_accu[,2],'mean'))
SLDP_SL_upper_CI <- c(getUpperCI(SLDP_SL_accu[,2],'mean'))


SLDP_DP_point <- c(mean(SLDP_DP_accu[,2]))
SLDP_DP_lower_CI <- c(getLowerCI(SLDP_DP_accu[,2],'mean'))
SLDP_DP_upper_CI <- c(getUpperCI(SLDP_DP_accu[,2],'mean'))


SLDP_diff_point <- c(mean(SLDP_diff[,2]))
SLDP_diff_lower_CI <- c(getLowerCI(SLDP_diff[,2],'mean'))
SLDP_diff_upper_CI <- c(getUpperCI(SLDP_diff[,2],'mean'))


##### Plot Difference
points <- c(SLSS_diff_point, SLSP_diff_point, SLDS_diff_point, SLDP_diff_point)
lower_CIs <- c(SLSS_diff_lower_CI, SLSP_diff_lower_CI, SLDS_diff_lower_CI, SLDP_diff_lower_CI)
upper_CIs <- c(SLSS_diff_upper_CI, SLSP_diff_upper_CI, SLDS_diff_upper_CI, SLDP_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("SL vs. SS","SL vs. SP","SL vs. DS", "SL vs. DP"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pSLdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SL / SS","SL / SP","SL / DS", "SL / DP")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over SL")




##### Plot Versus

SL_points <- c(SLSS_SL_point, SLSP_SL_point, SLDS_SL_point, SLDP_SL_point)
SL_lower_CIs <- c(SLSS_SL_lower_CI, SLSP_SL_lower_CI, SLDS_SL_lower_CI, SLDP_SL_lower_CI)
SL_upper_CIs <- c(SLSS_SL_upper_CI, SLSP_SL_upper_CI, SLDS_SL_upper_CI, SLDP_SL_upper_CI)

vs_points <- c(SLSS_SS_point, SLSP_SP_point, SLDS_DS_point, SLDP_DP_point)
vs_lower_CIs <- c(SLSS_SS_lower_CI, SLSP_SP_lower_CI, SLDS_DS_lower_CI, SLDP_DP_lower_CI)
vs_upper_CIs <- c(SLSS_SS_upper_CI, SLSP_SP_upper_CI, SLDS_DS_upper_CI, SLDP_DP_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(SL_points,vs_points),
  y = c("SL", "SL", "SL", "SL", "SS", "SP", "DS", "DL"),
  upper = c(SL_upper_CIs, vs_upper_CIs),
  lower = c(SL_lower_CIs, vs_lower_CIs),
  variable = rep(c("SL","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pSLvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("SL vs. SS","SL vs. SL","SL vs. DS", "SL vs. DL")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")












################### FOCUS ON DS #############################



### DS vs. SP

DSSP_DS_accu <- aggregateParticipant(task_SPDS,'DS','mean')
DSSP_SP_accu <- aggregateParticipant(task_SPDS,'SP','mean')
DSSP_diff <- DSSP_SP_accu - DSSP_DS_accu

DSSP_DS_point <- c(mean(DSSP_DS_accu[,2]))
DSSP_DS_lower_CI <- c(getLowerCI(DSSP_DS_accu[,2],'mean'))
DSSP_DS_upper_CI <- c(getUpperCI(DSSP_DS_accu[,2],'mean'))

DSSP_SP_point <- c(mean(DSSP_SP_accu[,2]))
DSSP_SP_lower_CI <- c(getLowerCI(DSSP_SP_accu[,2],'mean'))
DSSP_SP_upper_CI <- c(getUpperCI(DSSP_SP_accu[,2],'mean'))


DSSP_diff_point <- c(mean(DSSP_diff[,2]))
DSSP_diff_lower_CI <- c(getLowerCI(DSSP_diff[,2],'mean'))
DSSP_diff_upper_CI <- c(getUpperCI(DSSP_diff[,2],'mean'))

### DS vs. SL

DSSL_DS_accu <- aggregateParticipant(task_SLDS,'DS','mean')
DSSL_SL_accu <- aggregateParticipant(task_SLDS,'SL','mean')
DSSL_diff <- DSSL_SL_accu - DSSL_DS_accu

DSSL_DS_point <- c(mean(DSSL_DS_accu[,2]))
DSSL_DS_lower_CI <- c(getLowerCI(DSSL_DS_accu[,2],'mean'))
DSSL_DS_upper_CI <- c(getUpperCI(DSSL_DS_accu[,2],'mean'))


DSSL_SL_point <- c(mean(DSSL_SL_accu[,2]))
DSSL_SL_lower_CI <- c(getLowerCI(DSSL_SL_accu[,2],'mean'))
DSSL_SL_upper_CI <- c(getUpperCI(DSSL_SL_accu[,2],'mean'))

DSSL_diff_point <- c(mean(DSSL_diff[,2]))
DSSL_diff_lower_CI <- c(getLowerCI(DSSL_diff[,2],'mean'))
DSSL_diff_upper_CI <- c(getUpperCI(DSSL_diff[,2],'mean'))




### DS vs. DL

DSDL_DL_accu <- aggregateParticipant(task_DSDL,'DL','mean')
DSDL_DS_accu <- aggregateParticipant(task_DSDL,'DS','mean')
DSDL_diff <- DSDL_DL_accu - DSDL_DS_accu

DSDL_DL_point <- c(mean(DSDL_DL_accu[,2]))
DSDL_DL_lower_CI <- c(getLowerCI(DSDL_DL_accu[,2],'mean'))
DSDL_DL_upper_CI <- c(getUpperCI(DSDL_DL_accu[,2],'mean'))


DSDL_DS_point <- c(mean(DSDL_DS_accu[,2]))
DSDL_DS_lower_CI <- c(getLowerCI(DSDL_DS_accu[,2],'mean'))
DSDL_DS_upper_CI <- c(getUpperCI(DSDL_DS_accu[,2],'mean'))

DSDL_diff_point <- c(mean(DSDL_diff[,2]))
DSDL_diff_lower_CI <- c(getLowerCI(DSDL_diff[,2],'mean'))
DSDL_diff_upper_CI <- c(getUpperCI(DSDL_diff[,2],'mean'))

### DS vs. DP

DSDP_DS_accu <- aggregateParticipant(task_DSDP,'DS','mean')
DSDP_DP_accu <- aggregateParticipant(task_DSDP,'DP','mean')
DSDP_diff <- DSDP_DP_accu - DSDP_DS_accu

DSDP_DS_point <- c(mean(DSDP_DS_accu[,2]))
DSDP_DS_lower_CI <- c(getLowerCI(DSDP_DS_accu[,2],'mean'))
DSDP_DS_upper_CI <- c(getUpperCI(DSDP_DS_accu[,2],'mean'))


DSDP_DP_point <- c(mean(DSDP_DP_accu[,2]))
DSDP_DP_lower_CI <- c(getLowerCI(DSDP_DP_accu[,2],'mean'))
DSDP_DP_upper_CI <- c(getUpperCI(DSDP_DP_accu[,2],'mean'))


DSDP_diff_point <- c(mean(DSDP_diff[,2]))
DSDP_diff_lower_CI <- c(getLowerCI(DSDP_diff[,2],'mean'))
DSDP_diff_upper_CI <- c(getUpperCI(DSDP_diff[,2],'mean'))


##### Plot Difference
points <- c(DSSL_diff_point, DSSP_diff_point, DSDL_diff_point, DSDP_diff_point)
lower_CIs <- c(DSSL_diff_lower_CI, DSSP_diff_lower_CI, DSDL_diff_lower_CI, DSDP_diff_lower_CI)
upper_CIs <- c(DSSL_diff_upper_CI, DSSP_diff_upper_CI, DSDL_diff_upper_CI, DSDP_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("DS vs. SL","DS vs. SP","DS vs. DL", "DS vs. DP"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pDSdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DS / SL","DS / SP","DS / DL", "DS / DP")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over DS")




##### Plot Versus

DS_points <- c(DSSL_DS_point, DSSP_DS_point, DSDL_DS_point, DSDP_DS_point)
DS_lower_CIs <- c(DSSL_DS_lower_CI, DSSP_DS_lower_CI, DSDL_DS_lower_CI, DSDP_DS_lower_CI)
DS_upper_CIs <- c(DSSL_DS_upper_CI, DSSP_DS_upper_CI, DSDL_DS_upper_CI, DSDP_DS_upper_CI)

vs_points <- c(DSSL_SL_point, DSSP_SP_point, DSDL_DL_point, DSDP_DP_point)
vs_lower_CIs <- c(DSSL_SL_lower_CI, DSSP_SP_lower_CI, DSDL_DL_lower_CI, DSDP_DP_lower_CI)
vs_upper_CIs <- c(DSSL_SL_upper_CI, DSSP_SP_upper_CI, DSDL_DL_upper_CI, DSDP_DP_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(DS_points,vs_points),
  y = c("DS", "DS", "DS", "DS", "SL", "SP", "DL", "DP"),
  upper = c(DS_upper_CIs, vs_upper_CIs),
  lower = c(DS_lower_CIs, vs_lower_CIs),
  variable = rep(c("DS","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pDSvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DS vs. SL","DS vs. SP","DS vs. DL", "DS vs. DP")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")






################### FOCUS ON DP #############################



### DP vs. SS

DPSS_DP_accu <- aggregateParticipant(task_SSDP,'DP','mean')
DPSS_SS_accu <- aggregateParticipant(task_SSDP,'SS','mean')
DPSS_diff <- DPSS_SS_accu - DPSS_DP_accu

DPSS_DP_point <- c(mean(DPSS_DP_accu[,2]))
DPSS_DP_lower_CI <- c(getLowerCI(DPSS_DP_accu[,2],'mean'))
DPSS_DP_upper_CI <- c(getUpperCI(DPSS_DP_accu[,2],'mean'))

DPSS_SS_point <- c(mean(DPSS_SS_accu[,2]))
DPSS_SS_lower_CI <- c(getLowerCI(DPSS_SS_accu[,2],'mean'))
DPSS_SS_upper_CI <- c(getUpperCI(DPSS_SS_accu[,2],'mean'))


DPSS_diff_point <- c(mean(DPSS_diff[,2]))
DPSS_diff_lower_CI <- c(getLowerCI(DPSS_diff[,2],'mean'))
DPSS_diff_upper_CI <- c(getUpperCI(DPSS_diff[,2],'mean'))

### DP vs. SL

DPSL_DP_accu <- aggregateParticipant(task_SLDP,'DP','mean')
DPSL_SL_accu <- aggregateParticipant(task_SLDP,'SL','mean')
DPSL_diff <- DPSL_SL_accu - DPSL_DP_accu

DPSL_DP_point <- c(mean(DPSL_DP_accu[,2]))
DPSL_DP_lower_CI <- c(getLowerCI(DPSL_DP_accu[,2],'mean'))
DPSL_DP_upper_CI <- c(getUpperCI(DPSL_DP_accu[,2],'mean'))


DPSL_SL_point <- c(mean(DPSL_SL_accu[,2]))
DPSL_SL_lower_CI <- c(getLowerCI(DPSL_SL_accu[,2],'mean'))
DPSL_SL_upper_CI <- c(getUpperCI(DPSL_SL_accu[,2],'mean'))

DPSL_diff_point <- c(mean(DPSL_diff[,2]))
DPSL_diff_lower_CI <- c(getLowerCI(DPSL_diff[,2],'mean'))
DPSL_diff_upper_CI <- c(getUpperCI(DPSL_diff[,2],'mean'))




### DP vs. DL

DPDL_DL_accu <- aggregateParticipant(task_DPDL,'DL','mean')
DPDL_DP_accu <- aggregateParticipant(task_DPDL,'DP','mean')
DPDL_diff <- DPDL_DL_accu - DPDL_DP_accu

DPDL_DL_point <- c(mean(DPDL_DL_accu[,2]))
DPDL_DL_lower_CI <- c(getLowerCI(DPDL_DL_accu[,2],'mean'))
DPDL_DL_upper_CI <- c(getUpperCI(DPDL_DL_accu[,2],'mean'))


DPDL_DP_point <- c(mean(DPDL_DP_accu[,2]))
DPDL_DP_lower_CI <- c(getLowerCI(DPDL_DP_accu[,2],'mean'))
DPDL_DP_upper_CI <- c(getUpperCI(DPDL_DP_accu[,2],'mean'))

DPDL_diff_point <- c(mean(DPDL_diff[,2]))
DPDL_diff_lower_CI <- c(getLowerCI(DPDL_diff[,2],'mean'))
DPDL_diff_upper_CI <- c(getUpperCI(DPDL_diff[,2],'mean'))

### DP vs. DS

DPDS_DP_accu <- aggregateParticipant(task_DSDP,'DP','mean')
DPDS_DS_accu <- aggregateParticipant(task_DSDP,'DS','mean')
DPDS_diff <- DPDS_DS_accu - DPDS_DP_accu

DPDS_DS_point <- c(mean(DPDS_DS_accu[,2]))
DPDS_DS_lower_CI <- c(getLowerCI(DPDS_DS_accu[,2],'mean'))
DPDS_DS_upper_CI <- c(getUpperCI(DPDS_DS_accu[,2],'mean'))


DPDS_DP_point <- c(mean(DPDS_DP_accu[,2]))
DPDS_DP_lower_CI <- c(getLowerCI(DPDS_DP_accu[,2],'mean'))
DPDS_DP_upper_CI <- c(getUpperCI(DPDS_DP_accu[,2],'mean'))


DPDS_diff_point <- c(mean(DPDS_diff[,2]))
DPDS_diff_lower_CI <- c(getLowerCI(DPDS_diff[,2],'mean'))
DPDS_diff_upper_CI <- c(getUpperCI(DPDS_diff[,2],'mean'))


##### Plot Difference
points <- c(DPSL_diff_point, DPSS_diff_point, DPDL_diff_point, DPDS_diff_point)
lower_CIs <- c(DPSL_diff_lower_CI, DPSS_diff_lower_CI, DPDL_diff_lower_CI, DPDS_diff_lower_CI)
upper_CIs <- c(DPSL_diff_upper_CI, DPSS_diff_upper_CI, DPDL_diff_upper_CI, DPDS_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("DP vs. SL","DP vs. SS","DP vs. DL", "DP vs. DS"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pDPdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DP / SL","DP / SS","DP / DL", "DP / DS")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over DP")




##### Plot Versus

DP_points <- c(DPSL_DP_point, DPSS_DP_point, DPDL_DP_point, DPDS_DP_point)
DP_lower_CIs <- c(DPSL_DP_lower_CI, DPSS_DP_lower_CI, DPDL_DP_lower_CI, DPDS_DP_lower_CI)
DP_upper_CIs <- c(DPSL_DP_upper_CI, DPSS_DP_upper_CI, DPDL_DP_upper_CI, DPDS_DP_upper_CI)

vs_points <- c(DPSL_SL_point, DPSS_SS_point, DPDL_DL_point, DPDS_DS_point)
vs_lower_CIs <- c(DPSL_SL_lower_CI, DPSS_SS_lower_CI, DPDL_DL_lower_CI, DPDS_DS_lower_CI)
vs_upper_CIs <- c(DPSL_SL_upper_CI, DPSS_SS_upper_CI, DPDL_DL_upper_CI, DPDS_DS_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(DP_points,vs_points),
  y = c("DP", "DP", "DP", "DP", "SL", "SS", "DL", "DS"),
  upper = c(DP_upper_CIs, vs_upper_CIs),
  lower = c(DP_lower_CIs, vs_lower_CIs),
  variable = rep(c("DP","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pDPvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DP vs. SL","DP vs. SS","DP vs. DL", "DP vs. DS")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")










################### FOCUS ON DL #############################



### DL vs. SS

DLSS_DL_accu <- aggregateParticipant(task_SSDL,'DL','mean')
DLSS_SS_accu <- aggregateParticipant(task_SSDL,'SS','mean')
DLSS_diff <- DLSS_SS_accu - DLSS_DL_accu

DLSS_DL_point <- c(mean(DLSS_DL_accu[,2]))
DLSS_DL_lower_CI <- c(getLowerCI(DLSS_DL_accu[,2],'mean'))
DLSS_DL_upper_CI <- c(getUpperCI(DLSS_DL_accu[,2],'mean'))

DLSS_SS_point <- c(mean(DLSS_SS_accu[,2]))
DLSS_SS_lower_CI <- c(getLowerCI(DLSS_SS_accu[,2],'mean'))
DLSS_SS_upper_CI <- c(getUpperCI(DLSS_SS_accu[,2],'mean'))


DLSS_diff_point <- c(mean(DLSS_diff[,2]))
DLSS_diff_lower_CI <- c(getLowerCI(DLSS_diff[,2],'mean'))
DLSS_diff_upper_CI <- c(getUpperCI(DLSS_diff[,2],'mean'))

### DL vs. SP

DLSP_DL_accu <- aggregateParticipant(task_SPDL,'DL','mean')
DLSP_SP_accu <- aggregateParticipant(task_SPDL,'SP','mean')
DLSP_diff <- DLSP_SP_accu - DLSP_DL_accu

DLSP_DL_point <- c(mean(DLSP_DL_accu[,2]))
DLSP_DL_lower_CI <- c(getLowerCI(DLSP_DL_accu[,2],'mean'))
DLSP_DL_upper_CI <- c(getUpperCI(DLSP_DL_accu[,2],'mean'))


DLSP_SP_point <- c(mean(DLSP_SP_accu[,2]))
DLSP_SP_lower_CI <- c(getLowerCI(DLSP_SP_accu[,2],'mean'))
DLSP_SP_upper_CI <- c(getUpperCI(DLSP_SP_accu[,2],'mean'))

DLSP_diff_point <- c(mean(DLSP_diff[,2]))
DLSP_diff_lower_CI <- c(getLowerCI(DLSP_diff[,2],'mean'))
DLSP_diff_upper_CI <- c(getUpperCI(DLSP_diff[,2],'mean'))




### DL vs. DP

DLDP_DL_accu <- aggregateParticipant(task_DPDL,'DL','mean')
DLDP_DP_accu <- aggregateParticipant(task_DPDL,'DP','mean')
DLDP_diff <- DLDP_DP_accu - DLDP_DL_accu

DLDP_DL_point <- c(mean(DLDP_DL_accu[,2]))
DLDP_DL_lower_CI <- c(getLowerCI(DLDP_DL_accu[,2],'mean'))
DLDP_DL_upper_CI <- c(getUpperCI(DLDP_DL_accu[,2],'mean'))


DLDP_DP_point <- c(mean(DLDP_DP_accu[,2]))
DLDP_DP_lower_CI <- c(getLowerCI(DLDP_DP_accu[,2],'mean'))
DLDP_DP_upper_CI <- c(getUpperCI(DLDP_DP_accu[,2],'mean'))

DLDP_diff_point <- c(mean(DLDP_diff[,2]))
DLDP_diff_lower_CI <- c(getLowerCI(DLDP_diff[,2],'mean'))
DLDP_diff_upper_CI <- c(getUpperCI(DLDP_diff[,2],'mean'))

### DL vs. DS

DLDS_DL_accu <- aggregateParticipant(task_DSDL,'DL','mean')
DLDS_DS_accu <- aggregateParticipant(task_DSDL,'DS','mean')
DLDS_diff <- DLDS_DS_accu - DLDS_DL_accu

DLDS_DS_point <- c(mean(DLDS_DS_accu[,2]))
DLDS_DS_lower_CI <- c(getLowerCI(DLDS_DS_accu[,2],'mean'))
DLDS_DS_upper_CI <- c(getUpperCI(DLDS_DS_accu[,2],'mean'))


DLDS_DL_point <- c(mean(DLDS_DL_accu[,2]))
DLDS_DL_lower_CI <- c(getLowerCI(DLDS_DL_accu[,2],'mean'))
DLDS_DL_upper_CI <- c(getUpperCI(DLDS_DL_accu[,2],'mean'))


DLDS_diff_point <- c(mean(DLDS_diff[,2]))
DLDS_diff_lower_CI <- c(getLowerCI(DLDS_diff[,2],'mean'))
DLDS_diff_upper_CI <- c(getUpperCI(DLDS_diff[,2],'mean'))


##### Plot Difference
points <- c(DLSP_diff_point, DLSS_diff_point, DLDP_diff_point, DLDS_diff_point)
lower_CIs <- c(DLSP_diff_lower_CI, DLSS_diff_lower_CI, DLDP_diff_lower_CI, DLDS_diff_lower_CI)
upper_CIs <- c(DLSP_diff_upper_CI, DLSS_diff_upper_CI, DLDP_diff_upper_CI, DLDS_diff_upper_CI)


dfr <- data.frame(        									 # data frame with differences and CIs
  x = points,
  y = c("DL vs. SP","DL vs. SS","DL vs. DP", "DL vs. DS"),
  upper = upper_CIs,
  lower = lower_CIs
)

dfr$y_numeric <- c(1,2,3,4)

dfr

pDLdiff <- ggplot(dfr, aes(x, y_numeric)) +
  geom_vline(xintercept = 0) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DL / SP","DL / SS","DL / DP", "DL / DS")) +	
  xlab("Improvement in Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Gain in grouping strength over DL")




##### Plot Versus

DL_points <- c(DLSP_DL_point, DLSS_DL_point, DLDP_DL_point, DLDS_DL_point)
DL_lower_CIs <- c(DLSP_DL_lower_CI, DLSS_DL_lower_CI, DLDP_DL_lower_CI, DLDS_DL_lower_CI)
DL_upper_CIs <- c(DLSP_DL_upper_CI, DLSS_DL_upper_CI, DLDP_DL_upper_CI, DLDS_DL_upper_CI)

vs_points <- c(DLSP_SP_point, DLSS_SS_point, DLDP_DP_point, DLDS_DS_point)
vs_lower_CIs <- c(DLSP_SP_lower_CI, DLSS_SS_lower_CI, DLDP_DP_lower_CI, DLDS_DS_lower_CI)
vs_upper_CIs <- c(DLSP_SP_upper_CI, DLSS_SS_upper_CI, DLDP_DP_upper_CI, DLDS_DS_upper_CI)

dfr <- data.frame(        									 # data frame with f and CIs
  x = c(DL_points,vs_points),
  y = c("DL", "DL", "DL", "DL", "SP", "SS", "DP", "DS"),
  upper = c(DL_upper_CIs, vs_upper_CIs),
  lower = c(DL_lower_CIs, vs_lower_CIs),
  variable = rep(c("DL","vs"), each = 4)
)

dfr$y_numeric <- c(0.9,1.9,2.9,3.9,1.1,2.1,3.1,4.1)

dfr


pDLvs <- ggplot(dfr, aes(x, y_numeric, colour = variable)) +
  geom_point(size = 6) +
  geom_errorbarh(aes(xmin=lower, xmax=upper), height = .0, size = 1) +
  scale_colour_brewer(palette=3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(0.7,4.3)) +
  scale_y_discrete(breaks=c(1,2,3,4), labels=c("DL vs. SP","DL vs. SS","DL vs. DP", "DL vs. DS")) +	
  xlab("Grouping Strength") +		
  theme_bw() + 
  theme(axis.title.y = element_blank(), axis.line.y = element_blank()) +
  ggtitle("Mean Grouping Strength by Task and by Visual Variable")




pdf(file = "gestalt-static.pdf", width=9, height=15)
grid.arrange(pSPvs,pSPdiff, pSSvs,pSSdiff,pSLvs,pSLdiff, ncol=2)
dev.off()	

pdf(file = "gestalt-dynamic.pdf", width=9, height=15)
grid.arrange(pDPvs,pDPdiff, pDSvs,pDSdiff,pDLvs,pDLdiff, ncol=2)
dev.off()
