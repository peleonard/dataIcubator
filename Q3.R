### Lending Club loan stats Q4 ### 

### data.table and tidyverse
library(data.table)
library(tidyverse)

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

### First interest on loanStats 2017 Quarter 4
lsQ4 <- loanStat2017$Q4

### 1. Numeric and Categorical -------------------------------------------------

## Re-check numerical columns determined by "data.table"
(numCols <- names(lsQ4)[!(lsQ4[, lapply(.SD, class)] == "character")])
# Unfortunately, all cols are of type character, we have to assign class manually.

## There is hardly any data upon "hardship" features
lsQ4[, .SD, .SDcols = grep("hardship", names(lsQ4))] -> hs
# hs[, lapply(.SD, table)]
lapply(hs, table)

lsQ4[, .SD, .SDcols = grep("mths", names(lsQ4))] -> mon
lapply(mon, table)

lsQ4[, .SD, .SDcols = grep("^sec_", names(lsQ4))] -> sec
lapply(sec, table)

## potential features: "dti_joint"
## unknown: policy_code, pub_rec, recoveries, 
numCols <- c("acc_now_delinq", "acc_open_past_24mths", "all_util", "annual_inc", "annual_inc_joint",
             "avg_cur_bal", "bc_open_to_buy", "bc_util", "chargeoff_within_12_mths", 
             "collection_recovery_fee", "collections_12_mths_ex_med", "delinq_2yrs", "delinq_amnt",
             "dti", "dti_joint", "funded_amnt", "funded_amnt_inv", 
             "il_util", "inq_fi", "inq_last_12m", "inq_last_6mths", "int_rate",
             "last_pymnt_amnt", "loan_amnt", "max_bal_bc", "mo_sin_old_il_acct",
             "mo_sin_old_rev_tl_op", "mo_sin_rcnt_rev_tl_op", "mo_sin_rcnt_tl", "mort_acc", 
             "mths_since_last_delinq", "mths_since_last_major_derog", "mths_since_last_record",
             "mths_since_rcnt_il", "mths_since_recent_bc", "mths_since_recent_bc_dlq",
             "mths_since_recent_inq", "mths_since_recent_revol_delinq", "num_accts_ever_120_pd", 
             "num_actv_bc_tl", "num_actv_rev_tl", "num_bc_sats", "num_bc_tl", 
             "num_il_tl", "num_op_rev_tl", "num_rev_accts", "num_rev_tl_bal_gt_0", 
             "num_sats", "num_tl_120dpd_2m", "num_tl_30dpd", "num_tl_90g_dpd_24m",
             "num_tl_op_past_12m", "open_acc", "open_acc_6m", "open_act_il", 
             "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m",
             "out_prncp", "out_prncp_inv", "pct_tl_nvr_dlq", "percent_bc_gt_75", 
             "pub_rec", "pub_rec_bankruptcies", "revol_bal", "revol_bal_joint", "revol_util", 
             "sec_app_chargeoff_within_12_mths", "sec_app_collections_12_mths_ex_med",                     
             "sec_app_inq_last_6mths", "sec_app_mort_acc",                          
             "sec_app_mths_since_last_major_derog", "sec_app_num_rev_accts",                     
             "sec_app_open_acc", "sec_app_open_act_il", "sec_app_revol_util",
             "tax_liens", "tot_coll_amt", "tot_cur_bal", "tot_hi_cred_lim",
             "total_bal_ex_mort", "total_bal_il", "total_bc_limit", "total_cu_tl", "total_il_high_credit_limit",
             "total_pymnt", "total_pymnt_inv", "total_rec_int", "total_rec_late_fee", 
             "total_rec_prncp", "total_rev_hi_lim")
lsQ4[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
# Warning "In lapply(.SD, as.numeric) : NAs introduced by coercion": owe to  as.numeric("NA")

### 2. Missing Values -----------------------------------------------------------------------

N <- nrow(lsQ4)
percNAs <- lsQ4[, lapply(.SD, function(x) sum(is.na(x))/N)]
qplot(x = as.numeric(percNAs), geom = "histogram", 
      main = "Missing Values Percentage of Variables", xlab = "Percentages of Missing Values")
# Take off the feature with more than a half NAs. 
## One might go back to this step INCASEOF further interest in the features removeds####
smNACols <- names(lsQ4)[percNAs <= .5]
lsQ4.v1 <- lsQ4[, smNACols, with = FALSE]

# N <- ncol(lsQ4.v1)
# lsQ4.v1I <- lsQ4.v1[, 1:floor(N/3)]
# lsQ4.v1II <- lsQ4.v1[, ceiling(round(N/3)):floor(2*N/3)]
# lsQ4.v1III <- lsQ4.v1[, ceiling(round(2*N/3)):N]
# 
# table(ls4$bc_open_to_buy)
# hist(ls4$bc_open_to_buy)


lsQ4[, lapply(.SD, function(x) sum(is.na(x))/nrow(lsQ4)), .SDcols = grep("joint", names(lsQ4), value = T)]




### 3. Secondary Applicant (Co-borrower) ---------------------------------------

## .1
jointPos <- grep("_joint", names(lsQ4))
lsQ4.joint <- lsQ4[, names(lsQ4)[c(jointPos, jointPos - 1)], with = F]
lsQ4.joint[, lapply(.SD, function(x) sum(is.na(x))/nrow(lsQ4.joint))]
setcolorder(lsQ4.joint, order(names(lsQ4.joint)))

str(lsQ4.joint)

# violin default already scaled: 
## Income and Revolving Balance has a long tail, which is believe to be outliers (false data)
## One possible method is to use log(x+1) transform
lsQ4.joint[, paste0(names(lsQ4.joint)[1:6], "_log") := log(.SD + 1), .SD = names(lsQ4.joint)[1:6]]


ggplot(melt(lsQ4.joint[,9:10], measure.vars = names(lsQ4.joint[,9:10])), 
       aes(x = variable, y = value)) + geom_violin()
t.test(lsQ4.joint$annual_inc_log, lsQ4.joint$annual_inc_joint_log, paired = T)

# geom_freqpoly scaling with "y = ..density.."

ggplot(melt(lsQ4.joint[,11:12], measure.vars = names(lsQ4.joint[,11:12])), 
       aes(col = variable, x = value, y = ..density..)) + geom_freqpoly()
t.test(lsQ4.joint$dti_log, lsQ4.joint$dti_joint_log, paired = T)

ggplot(melt(lsQ4.joint[,13:14], measure.vars = names(lsQ4.joint[,13:14])), 
       aes(x = variable, y = value)) + geom_violin()
t.test(lsQ4.joint$revol_bal_log, lsQ4.joint$revol_bal_joint_log, paired = T)

ggplot(melt(na.omit(lsQ4.joint[,7:8]), measure.vars = names(lsQ4.joint[,7:8])), 
       aes(x = variable, y = value)) + geom_count()
table(lsQ4.joint$verification_status_joint, exclude = NULL)
chisq.test(lsQ4.joint$verification_status, lsQ4.joint$verification_status_joint)




## .2
secAppColnames <- grep("sec_app_", names(lsQ4), value = T)
lsQ4.secApp <- lsQ4[, c(gsub("sec_app_", "", secAppColnames), secAppColnames), with = F]
lsQ4.secApp[, lapply(.SD, function(x) sum(is.na(x))/nrow(lsQ4.secApp))]
lsQ4.secApp <- lsQ4.secApp

table(lsQ4.secApp$chargeoff_within_12_mths)
table(lsQ4.secApp$sec_app_chargeoff_within_12_mths)

str(lsQ4.secApp)
# lapply(lsQ4.secApp, table)
ggplot(melt(lsQ4.secApp[,c(1,11)], measure.vars = names(lsQ4.secApp[,c(1,11)])), 
       aes(x = variable, y = value)) + geom_count()
ggplot(melt(lsQ4.secApp[,c(2,12)], measure.vars = names(lsQ4.secApp[,c(2,12)])), 
       aes(x = variable, y = value)) + geom_count()

### Deal with date 

# Create a date Class
datePos <- grep("earliest_cr_line", names(lsQ4.secApp))
lsQ4.secApp[, (datePos) := lapply(.SD, parse_date, format = "%b-%Y"), .SDcols = datePos]
str(lsQ4.secApp)

ggplot(melt(lsQ4.secApp[, c(3,13)], measure.vars = names(lsQ4.secApp)[c(3,13)]), 
            aes(x = value, col = variable)) + geom_freqpoly()
d <- parse_date(lsQ4.secApp$earliest_cr_line[[1]], format = "%b-%Y")
d1 <- lubridate::ymd(d)








