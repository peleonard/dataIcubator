### Lending Club loan stats Q4 ### 
getOption("device")
options(device = "RStudioGD")

### data.table and tidyverse
library(data.table)
library(tidyverse)
library(gridExtra)

### Import the dataset and set the order of cols for fast retrieval.

loanStat2017 <- lapply(Sys.glob("data/LoanStats_2017Q*.csv"), fread, na.strings = "", skip = 1)
names(loanStat2017) <- paste0("Q", 1:4)

## A careful check tells us they have the same variables 
lapply(loanStat2017, dim)
all.equal(names(loanStat2017[[1]]), names(loanStat2017[[2]]))
all.equal(names(loanStat2017[[2]]), names(loanStat2017[[3]]))
all.equal(names(loanStat2017[[3]]), names(loanStat2017[[4]]))
# They can be combined.

## Set the columns order for all four datasets for quick retrieval
invisible(lapply(loanStat2017, setcolorder, neworder = order(names(loanStat2017$Q1))))


ls2017 <- rbindlist(loanStat2017)

### 1. Numeric, Categorical and Date Columns-------------------------------------------------

# ## Re-check numerical columns determined by "data.table"
# (numCols <- names(ls2017)[!(ls2017[, lapply(.SD, class)] == "character")])
# # Unfortunately, all cols are of type character, we have to assign class manually.
# 
# ## There is hardly any data upon "hardship" features
# ls2017[, .SD, .SDcols = grep("hardship", names(ls2017))] -> hs
# # hs[, lapply(.SD, table)]
# # lapply(hs, table)
# 
# ls2017[, .SD, .SDcols = grep("mths", names(ls2017))] -> mon
# # lapply(mon, table)
# 
# ls2017[, .SD, .SDcols = grep("^sec_", names(ls2017))] -> sec
# # lapply(sec, table)

## potential features: "dti_joint"
## unknown: policy_code, pub_rec, recoveries, 
numCols <- c("acc_now_delinq", "acc_open_past_24mths", "all_util", "annual_inc", "annual_inc_joint",
             "avg_cur_bal", "bc_open_to_buy", "bc_util", "chargeoff_within_12_mths", 
             "collection_recovery_fee", "collections_12_mths_ex_med", "delinq_2yrs", "delinq_amnt",
             "dti", "dti_joint", "funded_amnt", "funded_amnt_inv", 
             "il_util", "inq_fi", "inq_last_12m", "inq_last_6mths",
             "last_pymnt_amnt", "loan_amnt", "max_bal_bc", "mo_sin_old_il_acct",
             "mo_sin_old_rev_tl_op", "mo_sin_rcnt_rev_tl_op", "mo_sin_rcnt_tl", "mort_acc", 
             "mths_since_last_delinq", "mths_since_last_major_derog", "mths_since_last_record",
             "mths_since_rcnt_il", "mths_since_recent_bc", "mths_since_recent_bc_dlq",
             "mths_since_recent_inq", "mths_since_recent_revol_delinq", "num_accts_ever_120_pd", 
             "num_actv_bc_tl", "num_actv_rev_tl", "num_bc_sats", "num_bc_tl", 
             "num_il_tl", "num_op_rev_tl", "num_rev_accts", "num_rev_tl_bal_gt_0", 
             "num_sats", "num_tl_120dpd_2m", "num_tl_30dpd", "num_tl_90g_dpd_24m",
             "num_tl_op_past_12m", "open_acc", "open_acc_6m", "open_act_il", 
             "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m", "orig_projected_additional_accrued_interest",
             "out_prncp", "out_prncp_inv", "pct_tl_nvr_dlq", "percent_bc_gt_75", 
             "pub_rec", "pub_rec_bankruptcies", "revol_bal", "revol_bal_joint", 
             "sec_app_chargeoff_within_12_mths", "sec_app_collections_12_mths_ex_med",                     
             "sec_app_inq_last_6mths", "sec_app_mort_acc",                          
             "sec_app_mths_since_last_major_derog", "sec_app_num_rev_accts",                     
             "sec_app_open_acc", "sec_app_open_act_il", "sec_app_revol_util",
             "tax_liens", "tot_coll_amt", "tot_cur_bal", "tot_hi_cred_lim",
             "total_bal_ex_mort", "total_bal_il", "total_bc_limit", "total_cu_tl", "total_il_high_credit_limit",
             "total_pymnt", "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", 
             "total_rec_prncp", "total_rev_hi_lim")
ls2017[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
## Warning "In lapply(.SD, as.numeric) : NAs introduced by coercion": owe to  as.numeric("NA")

# Be espeicially carefully on int_rate, in the form of "4.7%"
ls2017[, ":="(int_rate = as.numeric(gsub("%", "", int_rate)), 
              revol_util = as.numeric(gsub("%", "", revol_util)))]

# str(ls2017[, 90:ncol(ls2017)])
dateCols <- c("debt_settlement_flag_date", "earliest_cr_line", 
              "hardship_end_date", "hardship_start_date", 
              "issue_d", "last_credit_pull_d", "last_pymnt_d", "next_pymnt_d", 
              "payment_plan_start_date", "sec_app_earliest_cr_line",
              "settlement_date")
# ls2017[, dateCols, with = F]
ls2017[, (dateCols) := lapply(.SD, parse_date, format = "%b-%Y"), .SDcols = dateCols]
str(ls2017)


### 2. smartEDA ----------------------------------------------------------------
library(SmartEDA)
ExpData(ls2017, type = 1, DV = NULL)
ExpData(ls2017, type = 2, DV = NULL)

ExpNumStat(ls2017, by = "A", Qnt = seq(0, 1, .25), Outlier = TRUE, round = 2)
ExpNumViz(ls2017, gp = NULL, nlim = 10, Page= c(2, 2), sample = 8)

ExpCTable(ls2017, margin = 2, clim = 10, nlim = NULL, round = 2, per = T)
ExpCatViz(ls2017, gp = NULL, fname = NULL, clim = 10, margin = 2, Page = c(2,1), sample = 4)


### 3. Missing Values -----------------------------------------------------------------------

N <- nrow(ls2017)
percNAs <- ls2017[, lapply(.SD, function(x) sum(is.na(x))/N)]
qplot(x = as.numeric(percNAs), geom = "histogram", 
      main = "Missing Values Percentage of Variables", xlab = "Percentages of Missing Values")
# Take off the feature with more than a half NAs. 
## One might go back to this step INCASEOF further interest in the features removeds####
smNACols <- names(ls2017)[percNAs <= .5]
ls2017.v1 <- ls2017[, smNACols, with = FALSE]

# N <- ncol(ls2017.v1)
# ls2017.v1I <- ls2017.v1[, 1:floor(N/3)]
# ls2017.v1II <- ls2017.v1[, ceiling(round(N/3)):floor(2*N/3)]
# ls2017.v1III <- ls2017.v1[, ceiling(round(2*N/3)):N]
# 
# table(ls4$bc_open_to_buy)
# hist(ls4$bc_open_to_buy)

ls2017[, lapply(.SD, function(x) sum(is.na(x))/nrow(ls2017)), .SDcols = grep("joint", names(ls2017), value = T)]



### 4. Secondary Applicant (Co-borrower) ---------------------------------------

## .1
jointPos <- grep("_joint", names(ls2017))
ls2017.joint <- ls2017[, names(ls2017)[c(jointPos, jointPos - 1)], with = F]
ls2017.joint[, lapply(.SD, function(x) sum(is.na(x))/nrow(ls2017.joint))]
setcolorder(ls2017.joint, order(names(ls2017.joint)))

str(ls2017.joint)

# violin default already scaled: 
## Income and Revolving Balance has a long tail, which is believe to be outliers (false data)
## One possible method is to use log(x+1) transform
ls2017.joint[, paste0(names(ls2017.joint)[1:6], "_log") := log(.SD + 1), .SD = names(ls2017.joint)[1:6]]


p11 <- ggplot(melt(ls2017.joint[,9:10], measure.vars = names(ls2017.joint[,9:10])), 
       aes(x = variable, y = value)) + geom_violin()
t.test(ls2017.joint$annual_inc_log, ls2017.joint$annual_inc_joint_log, paired = T)

ggplot(ls2017.joint, aes(x = annual_inc_log, y = annual_inc_joint_log)) + geom_hex()


# geom_freqpoly scaling with "y = ..density.."
ggplot(melt(ls2017.joint[,11:12], measure.vars = names(ls2017.joint[,11:12])), 
       aes(col = variable, x = value, y = ..density..)) + geom_freqpoly() + ggtitle("Frequecy Polygon of dti")
t.test(ls2017.joint$dti_log, ls2017.joint$dti_joint_log, paired = T)

p12 <- ggplot(ls2017.joint, aes(x = dti_log, y = dti_joint_log)) + geom_bin2d() + ggtitle("Binary Plot")

ggplot(melt(ls2017.joint[,13:14], measure.vars = names(ls2017.joint[,13:14])), 
       aes(x = variable, y = value)) + geom_violin()
t.test(ls2017.joint$revol_bal_log, ls2017.joint$revol_bal_joint_log, paired = T)


p13 <- ggplot(melt(na.omit(ls2017.joint[,7:8]), measure.vars = names(ls2017.joint[,7:8])), 
       aes(x = variable, y = value)) + geom_count() + ggtitle("Verification Status Count")
table(ls2017.joint$verification_status_joint, exclude = NULL)
chisq.test(ls2017.joint$verification_status, ls2017.joint$verification_status_joint)


## .2
secAppColnames <- grep("sec_app_", names(ls2017), value = T)
ls2017.secApp <- ls2017[, c(gsub("sec_app_", "", secAppColnames), secAppColnames), with = F]
ls2017.secApp[, lapply(.SD, function(x) sum(is.na(x))/nrow(ls2017.secApp))]

table(ls2017.secApp$chargeoff_within_12_mths)
table(ls2017.secApp$sec_app_chargeoff_within_12_mths)

str(ls2017.secApp)
# lapply(ls2017.secApp, table)
ggplot(melt(ls2017.secApp[,c(1,11)], measure.vars = names(ls2017.secApp[,c(1,11)])), 
       aes(x = variable, y = value)) + geom_count()
ggplot(melt(ls2017.secApp[,c(2,12)], measure.vars = names(ls2017.secApp[,c(2,12)])), 
       aes(x = variable, y = value)) + geom_count()


### Deal with date 
# Create a date Class
# d <- parse_date(ls2017.secApp$earliest_cr_line[[1]], format = "%b-%Y")

p14 <- ggplot(melt(na.omit(ls2017.secApp[, c(3,13)]), measure.vars = names(ls2017.secApp)[c(3,13)]), 
       aes(x = value, y = ..density.., col = variable)) + geom_freqpoly() + 
  ggtitle("Frequency Polygon of Earlies Credit Line Date")

date.secApp <- na.omit(ls2017.secApp[, c(3,13)])
date.secApp[, ahead := earliest_cr_line - sec_app_earliest_cr_line]

# qqnorm(as.numeric(date.secApp$ahead))
# qqline(as.numeric(date.secApp$ahead))
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
  
}
qqplot.data(as.numeric(date.secApp$ahead))

sum(as.numeric(date.secApp$ahead) < 0)/ nrow(date.secApp)


# 1. stat similarity between the borrower and coborrower
# 2. how do coborrowers affect the prediction. e.g. int_rate, loan status

### 5. Impact on certain response: int_rate ---------------------------------------


ExpNumViz(ls2017.joint[, -(1:6)], gp = "int_rate", nlim = 4, fname = NULL, Page= c(2, 2))

ls2017.lin <- ls2017[, c("int_rate",
                         "revol_util", "total_rec_int", "inq_last_6mths", "loan_amnt",
                         "issue_d", "application_type", "grade", "purpose", "verification_status"), 
                     with = F]

ls2017.joint.log <- ls2017.joint[, -(1:6)]
library(mice)
md.pattern(ls2017.joint.log)

library(VIM)
nhanes_aggr = aggr(ls2017.joint.log, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                   labels=names(ls2017.joint.log), cex.axis=.4, gap=3, 
                   ylab=c("Proportion of missingness","Missingness Pattern"))

# Create coborrower's class
ls2017.joint.log[, cbClass := is.na(verification_status_joint) + 
                   is.na(annual_inc_joint_log) + 
                   is.na(dti_joint_log) + 
                   is.na(revol_bal_joint_log)]
ls2017.joint.log[, cbType := ifelse(cbClass == 0, "none", ifelse(cbClass == 4, "all", "med"))]
ls2017.joint.log[, ":="(int_rate = ls2017$int_rate, loan_status = ls2017$loan_status, 
                        loan_amnt = ls2017$loan_amnt, revol_util = ls2017$revol_util)]

## let the class be balanced to do better visualization 
## cbType: none missing(10%) in "_joint", 1~3 missing(1%), all missing(89%)
(a <- table(ls2017.joint.log$cbType))

set.seed(101)
allInd_inv <- sample(which(ls2017.joint.log$cbType == "all"), a[1] - a[3])


p21 <- ggplot(ls2017.joint.log[-allInd_inv], aes(x = annual_inc_log, y = int_rate, col = cbType)) + 
  geom_point() + geom_jitter(height = 2, width = 0, cex = .1) + 
  ggtitle("Interest Rate VS Annual Income (with or without Coborrowers")

ggplot(ls2017.joint.log[-allInd_inv], aes(x = dti_log, y = int_rate, col = cbType)) + 
  geom_point() + geom_jitter(height = 2, width = 0, cex = .1)

p22 <- ggplot(ls2017.joint.log[-allInd_inv], aes(x = verification_status, y = cbType)) + 
  geom_raster(aes(fill = int_rate), hjust = .5, vjust = .5, interpolate = F) + 
  ggtitle("Interest Rate VS Verification Status (with or without Coborrowers")

grid.arrange(p21, p22, nrow = 1)


# ggplot(ls2017.joint.log[-allInd_inv], aes(x = loan_amnt, y = int_rate, col = cbType)) + 
#   geom_point() + geom_jitter(height = 2, width = 0, cex = .1)

ggplot(ls2017.joint.log[-allInd_inv], aes(x = loan_status, y = cbType)) + 
  geom_raster(aes(fill = int_rate), hjust = .5, vjust = .5, interpolate = F)


ls2017.joint.log[, issue_d := ls2017$issue_d]
ggplot(ls2017.joint.log[-allInd_inv], aes(x = issue_d, y = int_rate, col = cbType)) + 
  geom_line()




### 6. linear model ---------------------------------------------------------


# Based on the EDA last week, we focus ourselves on the following dataset: ten covariates with 
# one response
ls2017.lin <- ls2017[, c("int_rate",
                         "revol_util", "total_rec_int", "funded_amnt", "funded_amnt_inv", "inq_last_6mths", "loan_amnt",
                          "application_type", "grade", "purpose", "verification_status"), 
                     with = F]

# There are only missing values in revol_util and inq_last_6mths, imputed by median
ls2017.lin[, lapply(.SD, function(x) sum(is.na(x)) / nrow(ls2017.lin))]
ls2017.lin[, ":="(revol_util = ifelse(is.na(revol_util), median(revol_util, na.rm = T), revol_util), 
                   inq_last_6mths = ifelse(is.na(inq_last_6mths), median(inq_last_6mths, na.rm = T), inq_last_6mths))]


### Work on continuous data


### Work on discrete data

gradeNum <- data.table(grade = toupper(letters[1:7]), grade_Num = 1:7)
ls2017.lin <- ls2017.lin[gradeNum, on = "grade"]


intPurpose <- ls2017.lin[, .(int_rate_purpose = mean(int_rate, na.rm = T)), by = purpose]
thPurpose <- quantile(intPurpose$int_rate_purpose, c(1/3, 2/3))
intPurpose[, purpose_Class := ifelse(int_rate_purpose <= thPurpose[1], "low", 
                                     ifelse(int_rate_purpose <= thPurpose[2], "medium", "high"))]
ls2017.lin <- ls2017.lin[intPurpose, on = "purpose"]


ls2017.linFinal <- ls2017.lin[, c("int_rate", 
                                  "revol_util", "total_rec_int", "inq_last_6mths", 
                                  "loan_amnt", "application_type", "grade_Num", 
                                  "purpose_Class", "verification_status"), with = F]


ls2017.linFinal[, cbType := ls2017.joint.log$cbType]

set.seed(102)
train.ind <- sample(1:nrow(ls2017.linFinal), 0.7 * nrow(ls2017.linFinal))

str(ls2017.linFinal)
# ls2017.linFinal[, -"int_rate", with = F]
lm1 <- lm(int_rate ~ ., data = ls2017.linFinal[, -9, with = F][train.ind])
summary(lm1)

lm2 <- lm(int_rate ~ ., data = ls2017.linFinal[train.ind])
summary(lm2)

int_rate_pred <- predict(lm1, ls2017.linFinal[, -9, with = F][-train.ind])
int_rate_true <- ls2017.linFinal[, -9, with = F][-train.ind]$int_rate
sum((int_rate_pred - int_rate_true)^2)/sum((int_rate_true)^2)


int_rate_pred2 <- predict(lm2, ls2017.linFinal[-train.ind])
int_rate_true2 <- ls2017.linFinal[-train.ind]$int_rate
sum((int_rate_pred2 - int_rate_true)^2)/sum((int_rate_true)^2)

ls2017.linFinalTest <- ls2017.linFinal[-train.ind][, ":="(int_rate_pred = int_rate_pred)][, ":="(errorSQ = (int_rate - int_rate_pred)^2,
                                                                                                 errorSQ2 = (int_rate - int_rate_pred2)^2)]

ls2017.linFinalTest[, .(meanESQbyCB = sum(errorSQ)/sum(int_rate^2), 
                        meanESQbyCB2 = sum(errorSQ2)/sum(int_rate^2)), by = cbType]
table(ls2017$title)



