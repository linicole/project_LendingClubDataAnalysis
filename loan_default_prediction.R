#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


library(klaR)
library(woe)
library(gbm)
library(caret)
library(MASS)
library(ROCR)
library(pROC)
library(plyr)
library(sqldf)
library(glmnet)
library(ggplot2)
library(rpart)
library(party)
library(partykit)
library(Hmisc)
library(boot)



data=read.csv('mydata.csv')
scored=read.csv('scored_def.csv')
data=join(data,scored[,2:3],by='approval_id')

# Remove variables that are useless and contain large character strings, slowing down processing
remove_vars=c()
saved_vars = c(setdiff(colnames(data), remove_vars))
data=data[,saved_vars]




data.raw=data

lists=c()
lists=c(lists,'data.raw','lists')
save(file='my_loan_default_storage.Rdata',list=unique(lists))


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


###########################
# Fix Broken Variables    #
###########################


# Fix error codes and obviously messed up values, unless you think someone is making 20,000 pounds per month and trying to borrow 400
data[which(data[,'paycheck_amount'] > 20000),'paycheck_amount']=NA
data[which(data[,'income_monthly_net'] > 20000),'income_monthly_net']=NA
data[which(data[,'last_utilization_ratio'] > 1),'last_utilization_ratio'] = 1
data[which(data[,'previous_utilization_ratio_average'] > 1),'previous_utilization_ratio_average'] = 1




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


####################
# Create Variables #
####################


email_length=matrix(,dim(data)[1],)
for (i in 1:dim(data)[1]){
if (is.na(data[i,'email'])==F){email_length[i,1]=length(unlist(strsplit(unlist(strsplit(as.character(data[i,'email']),'@'))[1],'')))}
else {email_length[i,1]=NA}
}

application_month=matrix(,dim(data)[1],)
for (i in 1:dim(data)[1])
application_month[i,1]=(unlist(strsplit(as.character(data[i,'funding_date_actual']),'-'))[2])


data=cbind(data
,email_length
,application_month
)


lists=c(lists,'data')
save(file='my_loan_default_storage.Rdata',list=unique(lists))


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


##########################################
# Pre-Processing and Variable Definition #
##########################################


# Remove columns with null entry percentage greater than 95%
data=remove_null_columns(data, 0.975, return_cols=F)


# Ignore columns with zero variation
nzv = nearZeroVar(data[,],freqCut = 100,uniqueCut = .5,saveMetrics = TRUE)
#potential_vars = c(setdiff(colnames(data), row.names(nzv[nzv$zeroVar, ])))
potential_vars = c(setdiff(colnames(data), row.names(nzv[nzv$nzv, ])))


# Ignore variables that are not allowed in model or provide useless information
bad_vars=c(
'loan_id'
,'funding_date_actual'
,'loan_amount'
,'loan_status'
,'approval_id'
,'approval_processed'
,'approval_amount'
,'application_id'
,'customer_id'
,'customer_created'
,'risk_view_report_id'
,'risk_view_report_age'
,'subprime_id_fraud_report_id'
,'subprime_id_fraud_report_age'
,'idscore_report_id'
,'idscore_report_age'
,'clearinquiry_report_id'
,'clearinquiry_report_age'
)

potential_vars = c(setdiff(potential_vars, bad_vars))

# Separate dependent variables
dep_vars=c(
'utilization_ratio'
,'base_loan_ini_def_flg'
,'base_loan_fin_def_flg'
,'loan_chain_ini_def_flg'
,'loan_chain_fin_def_flg'
,'loan_chain_net_fees'
,'extension_count'
)

potential_vars = c(setdiff(potential_vars, dep_vars))



# Ignore variables output by old model
old_model_vars = c(
'app_default_rate'
,'app_credit_score'
,'app_profitability_rate'
)
potential_vars = c(setdiff(potential_vars, old_model_vars))



# Turn variables with less than x levels into factors (if you dare)
# data = auto_factor(data=data,max.levels=3)


# List of variable to be forced to factors
factor_variables = c(
)

# Update relevant variable type in data set to factor
for (i in 1:length(factor_variables)) {
  data[, factor_variables[i]] = as.factor(data[, factor_variables[i]])
}



# Define dependent variable
dep_var = 'loan_chain_fin_def_flg'

# List of independant variables to consider for model
ind_var_list = potential_vars

# View data structure
data_structure = lapply(data[,potential_vars], class)

# Filter for those columns that are not integer, numeric, or factor
data_structure_change = Filter(function(x) x != 'integer' && x != 'numeric' && x != 'factor', data_structure)
data_structure_change = names(data_structure_change)
print(data_structure_change)

# Change the columns to be factors
data[,potential_vars] = factor_data(data_structure_change, data)

# Set the response variable
summary(data[, dep_var])
data$response_var = data[, dep_var]
data$response_var_factor = as.factor(data[, dep_var])

# Remove data with no outcome variable
data = data[!is.na(data$response_var), ]



# Run some summary statistics
summary(data$response_var)


# Use make.names non-sense here
for (i in 1:dim(data)[2]){
    if (is.factor(data[,i]) & length(which(data[,i]=='')) > 0)
    data[,i]=as.factor(make.names(data[,i]))
}

# Check to make sure there are no id's or crazy factors left
too_many_levels = factor_killer(data[,potential_vars],target=30000)
too_many_levels
potential_vars = setdiff(potential_vars,too_many_levels)




# Look for structure and concentration in factor variables
# Print ouliers
lev_men = level_mean(data,potential_vars,response='response_var',outliers=TRUE,outlier.sd.interval=.25,outlier.requiered.frequency=.01)
lev_men
vars_to_consider = lev_men[[3]]


# Change factor levels to other if their frequency is below a certain cutoff
data = small_bin_cleaner(data,
                         potential_vars,
                         min.frequency=.01,
                         max.bins=30,
                         keep.na=TRUE,
                         missing.na=FALSE,
                         missing.blank=FALSE)

# View changes made by small bin cleaner
lev_men = level_mean(data,potential_vars,response='response_var',outliers=TRUE,outlier.sd.interval=.25,outlier.requiered.frequency=.01)
lev_men
vars_to_consider = lev_men[[3]]


# Remove variables with high pairwise correlations and keep columns with highest correlation to dependent variable
highCorr = highCorrelations(data,potential_vars,'response_var',cutoff=0.95, verbose=F)

# Print variables being dropped
highCorr$varsToDrop

# Update variable list if you want...
#potential_vars = setdiff(potential_vars, highCorr$varsToDrop)


# Save cleaned data
lists=c(lists,'potential_vars')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


######################################
# Partition the data into test/train #
######################################


# Split Data
samp=sample(1:nrow(data),nrow(data)*.7)
data.train=data[samp,]
data.test=data[-samp,]



# Save split data
lists=c(lists,'data.train','data.test')
save(file='my_loan_default_storage.Rdata',list=unique(lists))




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#######################################
# Define weight of evidence variables #
#######################################


# Generate WOE variables
woe.variables = potential_vars
woe_run = iv.mult(data.train, 'response_var', vars=woe.variables,sql=TRUE,rcontrol=rpart.control(cp=0.001, minbucket=100))

# Select only WOE variables with information value above .02
woe_run_summary = iv.summary(woe_run, ivcutoff=0.02)
iv.plot.summary(woe_run_summary)
woe_run.top = woe_run[sapply(woe_run, function(x) all(x$variable %in% woe_run_summary$Variable))]




# Secondary WOE run for top bins
tb_run = iv.mult(data.train, 'response_var', vars=woe.variables,sql=TRUE,rcontrol=rpart.control(cp=0.001, minbucket=200),topbin=T,tbcutoff=0.05)


###############################################
###############################################

woe.frame=NULL
for (i in 1:length(woe_run.top)){
    woe.frame=rbind(woe.frame,woe_run.top[[i]])
}

woe.frame[which(woe.frame[,6]==0),]
woe.frame[which(woe.frame[,1]=='pred_fin_def'),]



tb.frame=NULL
for (i in 1:length(tb_run)){
    tb.frame=rbind(tb.frame,tb_run[[i]])
}
tb.frame[which(tb.frame[,1]=='cc_sic'),]


###############################################
###############################################



# Replace data in original data set with the WOE values
woe_data = iv.replace.woe(data.train, woe_run.top)
woe_data = woe_data[, grep('.*_woe$', colnames(woe_data), value = TRUE)]
tb_data = iv.replace.woe(data.train, tb_run)
tb_data = tb_data[, grep('.*_b[0-9]*$', colnames(tb_data), value = TRUE)]

# Group all strong WOE variables
woe_data.train = cbind(woe_data, tb_data)

# Print strong WOE variables
colnames(woe_data.train)

# Remove near-zero variation fields from the binned data
nzv.woe = nearZeroVar(woe_data.train,freqCut = 100,uniqueCut = .5,saveMetrics = TRUE)

# Create WOE variable list
woe_vars = row.names(nzv.woe[!nzv.woe$nzv, ])



# Save woe rules
lists=c(lists,'woe_run.top','tb_run')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


###########################
# Define binned variables #
###########################

# save(file='fin_def_potential_vars.Rdata', list=('potential_vars'))

# Set up binning grid
bin_fml = myformula(potential_vars, 'response_var')
#binGrid = expand.grid(.interaction.depth=seq(1, 3, 1),n.trees=c(1, 5, 10),.shrinkage=0.1)
binGrid = expand.grid(.interaction.depth=seq(1, 5, 1),.n.trees=1,.shrinkage=0.1)

# Find optimal binning in repeated cv fold validation
all_vars_ranked = univariate_prune(bin_fml,data.train,5, 5, binGrid,type="classification")

print(all_vars_ranked)



# Create a data frame containing the binned variables
binning_list = univariate_binner(all_vars_ranked,'response_var',traindata=data.train,newdata=data.train,print.tree=F,create.mapping=T)

summary(binning_list)

binned_data.train = binning_list$bins.matrix

# This shows the dimensions of the new data frame
dim(binned_data.train)

# Save the mappings between the bins and rules
binning_mapping = binning_list$bins.rules
binning_sql = binning_mapping$sql_string_cleaned



# Extract the column names of the binned data
binned_vars = grep('.*_binned_.*$', colnames(binned_data.train), value = TRUE)

# Remove near-zero variation fields from the binned data
nzv.bin = nearZeroVar(binned_data.train[, binned_vars],freqCut = 100,uniqueCut = .5,saveMetrics = TRUE)

# Create binned variable list
binned_vars = row.names(nzv.bin[!nzv.bin$nzv, ])



# Save binning rules
lists=c(lists,'all_vars_ranked','binning_mapping', 'binning_sql')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


###############################
# Define continuous variables #
###############################

# Get only the numerical variables
cont_data.train = get_numeric_columns(data.train[, potential_vars])

# Use median impute
preProcValues = preProcess(cont_data.train, method = c('medianImpute'))
cont_data.train.imp = predict(preProcValues, cont_data.train)

# Median values -- Turn them into SQL
preProcvalues.sql_list = lapply(seq_along(preProcValues$median),
                                  function(ind, list, names) {
                                    paste('set', names[ind], "=", paste(list[[ind]], collapse=', '),
                                          'where', names[ind], 'is null')
                                  },
                                  list=preProcValues$median,
                                  names=names(preProcValues$median)
)

preProcvalues.sql_data = as.matrix(preProcvalues.sql_list)

# Save median impute rules
lists=c(lists,'preProcValues', 'preProcvalues.sql_data')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


##################################
# Combine together all datasets  #
##################################

# Bin variables
binned_vars.remove = c()
binned_vars.final = setdiff(binned_vars, binned_vars.remove)
x.binned = binned_data.train[, binned_vars.final]

# WOE variables
woe_vars.remove = c()
woe_vars.final = setdiff(woe_vars, woe_vars.remove)
x.woe = woe_data.train[, woe_vars.final]

# Continuous variables
cont_vars.remove = c()
cont_vars.final = setdiff(colnames(cont_data.train), cont_vars.remove)
x.cont = cont_data.train.imp[, cont_vars.final]

# Combine and add response variable.
dep_var_list = c('response_var', 'response_var_factor','approval_id')
train.comb = cbind(x.cont, x.woe, x.binned, data.train[, dep_var_list])


# Use high correlation function to remove variables measuring similar things.
highCorr.all = highCorrelations(as.data.frame(train.comb),setdiff(colnames(train.comb), dep_var_list),'response_var',cutoff=0.7, verbose=F)

highCorr.all$varsToDrop
final_vars = setdiff(setdiff(colnames(train.comb), highCorr.all$varsToDrop), dep_var_list)

# Print final list of independent/dependent variables
final_vars


# Save combined data and final variable list
lists=c(lists,'train.comb', 'final_vars')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


######################################
# Run GBM to get important variables #
######################################

# Define GBM Formula
gbm_fml = myformula(final_vars, 'response_var_factor')
summary(train.comb$response_var_factor)

# Set train.controls for model
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5,classProbs = TRUE,summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2),n.trees = c(25, 50, 100),shrinkage = 0.1,n.minobsinnode = c( 10, 25, 50, 100))

#higher precision version
#gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 5),n.trees = c(25, 50, 100, 200),shrinkage = 0.1,n.minobsinnode = c(1, 10, 25, 50, 100))


# Fix formatting for train()
train.comb[,'response_var_factor']=as.factor(make.names(train.comb[,'response_var_factor']))


#save(file='gbm_transfer.Rdata',list=c('fitControl','gbmGrid','gbm_fml','train.comb'))


# Initial GBM model
set.seed(1000)
gbmFit <- train(gbm_fml,data = train.comb,method = "gbm",trControl = fitControl, verbose = FALSE,tuneGrid = gbmGrid,metric = "ROC")



summary(gbmFit)
gbmFit.summary = as.data.frame(summary(gbmFit))


# Extract optimal number of trees
best.iter = gbm.perf(gbmFit, method="OOB")
auc(data.train[, 'response_var'], predict(gbmFit, train.comb, n.trees=best.iter))

# Get non-zero influence variable list from GBM
imp_vars.gbm = as.vector(gbmFit.summary[gbmFit.summary$rel.inf > 0, 'var'])


# Save gbm summary and important variables
lists=c(lists,'gbmFit','gbmFit.summary','imp_vars.gbm')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#######################
# Variable Clustering #
#######################

# Run varclus function
varc = varclusCorrelation(train.comb, final_vars, 'response_var', k=length(imp_vars.gbm))
varc$df

# Save varclus results
lists=c(lists,'varc')
save(file='my_loan_default_storage.Rdata',list=unique(lists))




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


########################################
# Run bootstrap for variable reduction #
########################################

# Set response variable
y_var = train.comb$response_var_factor

# Set independent variables from GBM/Varclus
x_vars = imp_vars.gbm

# Define formula
train_formula = myformula( x_vars,'response_var')


# Define bootstrap function for LASSO
en.bootstrap = function(x_vars, y_var, data, indices) {
  d = data[indices,]
  fit = cv.glmnet(as.matrix(d[, x_vars]), d[, y_var],nfolds=5, type.measure='auc', family='binomial')
  return(as.matrix(coef(fit, s='lambda.1se')))
}


# Run LASSO bootstrap
set.seed(1000)
repeats.i = 500
model.boot = boot(train.comb, statistic=en.bootstrap, R=repeats.i,
                  x_vars=x_vars, y_var='response_var')
boot.results = as.data.frame(model.boot$t)
colnames(boot.results) = c('Intercept', x_vars)

# Frequency of variable showing significance
boot.results.perc = colSums(boot.results != 0) / as.double(repeats.i)
boot.results.perc[order(boot.results.perc)]

# Only use variables appearing more than 70% of the time
x_vars.boot = intersect(x_vars, names(boot.results.perc[boot.results.perc > 0.7]))

# Save bootstrap results
lists=c(lists,'model.boot','x_vars.boot')
save(file='my_loan_default_storage.Rdata',list=unique(lists))




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


################################
# Run Initial GLM For P-Values #
################################

# Add/remove variables for Almost Final variable List
x_vars.remove = c(

)
x_vars.almost.final = c(setdiff(x_vars.boot, x_vars.remove))
x_vars.add = c(
)
x_vars.almost.final = c(x_vars.almost.final,x_vars.add)

# Define almost final formula
almost_final_formula = myformula(x_vars.almost.final, 'response_var')

# Build GLM
first_fit = glm(almost_final_formula, data=train.comb, family='binomial')
summary(first_fit)

# Some sort of backwards stepwise I guess
while (max(summary(first_fit)$coefficients[2:length(x_vars.almost.final)+1,4]) > .0001){
    k = as.data.frame(which(summary(first_fit)$coefficients[2:length(x_vars.almost.final)+1,4] == max(summary(first_fit)$coefficients[2:length(x_vars.almost.final)+1,4])))[,1]
    x_vars.almost.final = x_vars.almost.final[-(k+1)]
    almost_final_formula = myformula(x_vars.almost.final, 'response_var')
    first_fit = glm(almost_final_formula, data=train.comb, family='binomial')
}


while (length(x_vars.almost.final) > 15){
    m=matrix(,length(x_vars.almost.final),1)
    rownames(m)=x_vars.almost.final
    for (l in 1:length(x_vars.almost.final)){
        almost_final_formula = myformula(x_vars.almost.final[-l], 'response_var')
        first_fit = glm(almost_final_formula, data=train.comb, family='binomial')
        train.comb$pred=NA
        train.comb$pred = predict(first_fit,newdata=train.comb,type='response')
        m[l,1]= max(gainsTable(train.comb,score='pred',bins=20,outcome='response_var',dec=T)$KS)
    }
    x_vars.almost.final = x_vars.almost.final[-which(m[,1] == max(m[,1]))]
}

summary(first_fit)



# Add/remove variables for another Almost Final variable List
x_vars.remove = c(
    )
x_vars.almost.final = c(setdiff(x_vars.almost.final, x_vars.remove))
x_vars.add = c()
x_vars.almost.final = c(x_vars.almost.final,x_vars.add)
x_vars.almost.final


# Define almost final formula again
almost_final_formula = myformula(x_vars.almost.final, 'response_var')

# Build GLM again
first_fit = glm(almost_final_formula, data=train.comb, family='binomial')
summary(first_fit)

# Get KS score for initial fit
train.comb$pred_fin_def=(predict(first_fit,newdata=train.comb,type='response'))
ks_table.first_fit = gainsTable(train.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
first_fit_ks=max(ks_table.first_fit$KS)
first_fit_ks



# Save initial GLM results
lists=c(lists,'x_vars.almost.final','first_fit')
save(file='my_loan_default_storage.Rdata',list=unique(lists))




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#############################
# Artificial Rel.Inf Cutoff #
#############################


# Run GBM again (if you want) on final variables (forced/changed WOE will not be in the original GBM)
# To reduce the number of final variables arbitraily, just order by decreasing rel.inf and take the top X


# Define GBM Formula
gbm_fml.2 = myformula(x_vars.almost.final, 'response_var_factor')

# Set train.controls for model
fitControl <- trainControl(method = "repeatedcv",number = 3,repeats = 5,classProbs = TRUE,summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2),n.trees = c(25, 50, 100),shrinkage = 0.1,n.minobsinnode = c( 10, 25, 50, 100))

# Second GBM model
set.seed(1000)
gbmFit.2 <- train(gbm_fml.2,data = train.comb,method = "gbm",trControl = fitControl, verbose = FALSE,tuneGrid = gbmGrid,metric = "ROC")

summary(gbmFit.2)
gbmFit.summary.2 = as.data.frame(summary(gbmFit.2))

trim_gbm_summary = gbmFit.summary.2[x_vars.almost.final,]
trim_gbm_summary = trim_gbm_summary[which(is.na(trim_gbm_summary[,1])==F),]
trim_gbm_summary = trim_gbm_summary[order(trim_gbm_summary[,2],decreasing=T),]

var_ks_plot=sweet_plot(train.comb,trim_gbm_summary,'response_var')
var_ks_plot

#Set the top XX variables you want to keep
trimmed_vars.gbm = c(as.character(trim_gbm_summary[1:12,1]))
trimmed_vars.gbm

trimmed_formula = myformula(trimmed_vars.gbm, 'response_var')

# Build GLM again
trimmed_fit = glm(trimmed_formula, data=train.comb, family='binomial')
summary(trimmed_fit)


# Find KS score with this trimmed set of variables
train.comb$pred_fin_def=(predict(trimmed_fit,newdata=train.comb,type='response'))
ks_table.trimmed_fit = gainsTable(train.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
trimmed_fit_ks=max(ks_table.trimmed_fit$KS)
trimmed_fit_ks
auc.trimmed_fit=auc(train.comb[,'response_var'],train.comb[,'pred_fin_def'])
auc.trimmed_fit


# Save stuff
lists=c(lists,'var_ks_plot','gbmFit.2','gbmFit.summary.2')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


####################
# Varclus Analysis #
####################


# Looks at replacing variables with the top variable in their varclus cluster
# Not sure if this works as is, or if it was pieced together
va = matrix(,length(setdiff(x_vars.almost.final,setdiff(x_vars.almost.final,rownames(varc$df)))),3)
colnames(va) = c('variable','varclus group','group leader')
va[,1] = setdiff(x_vars.almost.final,setdiff(x_vars.almost.final,rownames(varc$df)))
for (i in 1:length(setdiff(x_vars.almost.final,setdiff(x_vars.almost.final,rownames(varc$df))))){
    va[i,2] = varc$df[which(rownames(varc$df)==setdiff(x_vars.almost.final,setdiff(x_vars.almost.final,rownames(varc$df)))[i]),1]
}
for (i in 1:length(setdiff(x_vars.almost.final,setdiff(x_vars.almost.final,rownames(varc$df))))){
    va[i,3] = rownames(varc$df[which(varc$df[,1]==va[i,2]),][1,])
}
va=as.data.frame(va)

summary(glm(myformula(va[,1], 'response_var'), data=train.comb, family='binomial'))
summary(glm(myformula(va[,3], 'response_var'), data=train.comb, family='binomial'))




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


####################
# Variable Forcing #
####################


# Manually combine factor varaiables in to 'other' category
td = as.matrix(data[,'cc_fbc'])
td[which(td[,1] != 'X' & td[,1] != 'U' & td[,1] != 'X0' & td[,1] != 'X1' & td[,1] != 'X2'),1] = 'other'
data[,'cc_fbc'] = as.factor(td[,1])
td = as.matrix(data.train[,'cc_fbc'])
td[which(td[,1] != 'X' & td[,1] != 'U' & td[,1] != 'X0' & td[,1] != 'X1' & td[,1] != 'X2'),1] = 'other'
data.train[,'cc_fbc'] = as.factor(td[,1])
td = as.matrix(data.test[,'cc_fbc'])
td[which(td[,1] != 'X' & td[,1] != 'U' & td[,1] != 'X0' & td[,1] != 'X1' & td[,1] != 'X2'),1] = 'other'
data.test[,'cc_fbc'] = as.factor(td[,1])





# Generate WOE levels for new variables or fix WOE for old variables
# Manually adjust the cp to get the proper number of bins (smaller number means more bins)
woe_add_vars = c('most_recent_base_loan_amount')

woe_run.force = iv.mult(data.train, 'response_var', vars=woe_add_vars,sql=TRUE,rcontrol=rpart.control(cp=0.0001, minbucket=50))
woe_run.force



# Turn WOE rule set into searchable table
woe.frame=NULL
for (i in 1:length(woe_run.top)){
    woe.frame=rbind(woe.frame,woe_run.top[[i]])
}

woe.frame[which(woe.frame[,1]=='paycheck_amount'),]


tb.frame=NULL
for (i in 1:length(tb_run)){
    tb.frame=rbind(tb.frame,tb_run[[i]])
}
tb.frame[which(tb.frame[,1]=='previous_initial_default_count'),]



# Generate replacement values
# Make sure the woe_data.add is just the column you want
woe_data.add = iv.replace.woe(data.train, woe_run.force)
woe_data.add = woe_data.add[, grep('.*_woe$', colnames(woe_data.add), value = TRUE)]
# Sometimes you need this hack job. I can't figure out why
# woe_data.add = woe_data.add[,which(colnames(woe_data.add)=='cc_gauge_score_woe')]
train.comb[,'paycheck_amount_woe'] = woe_data.add

# Use this if the woe variable wasn't created in initial run
#train.comb$days_before_next_payday_woe = woe_data.add


# Replace old WOE rules in master list
woe_run.top[5]=woe_run.force

# Use this if woe variable wasn't created in initial run
# woe_run.top=c(woe_run.top,woe_run.force)



# Add/remove whatever variables you want to force
x_vars.remove = c(

)
x_vars.final = c(setdiff(x_vars.almost.final, x_vars.remove))
x_vars.add = c(

)
x_vars.final = c(x_vars.final,x_vars.add)
x_vars.final


# Or enter final variables in manual mode
# Just keep the final list here so it easy easy to reference in the code and quickly re-run a regression
x_vars.final = c(
    #'pred_ini_def'
    'first_loan_default_score'
    #'first_loan_credit_score'
    #,'rv_score_woe'
    ,'days_since_last_due_date'
    ,'previous_initial_default_count_b1'
    ,'last_utilization_ratio_woe'
    #,'days_since_last_base_loan_woe'
    ,'returned_through_leads_flg'
    #,'days_before_next_payday'
    #,'email_domain_b3'
    #,'income_freq_type_cd_b3'
    #,'income_freq_type_cd_b8'
    ,'paycheck_amount_woe'
    ,'approvals_scored_dist_day_binned_2'
    #,'num_returns_30_b2'
    ,'previous_max_amount'
    ,'bad_rate_individual'
    )


#compare 2014 default scores and 2016 default
#talk to Failor about using past scores

# Define final formula
final_formula = myformula(x_vars.final, 'response_var')

# Build Final GLM
fit = glm(final_formula, data=train.comb, family='binomial')
summary(fit)


# Get KS score for final fit predictions
train.comb$pred_fin_def=(predict(fit,newdata=train.comb,type='response'))
ks_table.fit = gainsTable(train.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
fit_ks=max(ks_table.fit$KS)
fit_ks
auc.fit=auc(train.comb[,'response_var'],train.comb[,'pred_fin_def'])
auc.fit



# Save GLM results (resaves woe rules with forced set)
lists=c(lists,'x_vars.final','fit','final_formula')
save(file='my_loan_default_storage.Rdata',list=unique(lists))





#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#############################
# COEFFICIENT BOOTSTRAPPING #
#############################


# Define bootstrap function for coefficient averages
glm.bootstrap = function(formula, data, indices) {
  d = data[indices, ]
  fit = glm(formula, data=data[indices,], family='binomial')

  # Coefficients
  return(as.matrix(coef(fit)))
}

# Run coefficient bootstrap
set.seed(1000)
model.glm = boot(data=train.comb, statistic=glm.bootstrap, R=100,
                   formula=final_formula)

# Print bootstrapped coefficient summaries
glm.results = as.data.frame(model.glm$t)
colnames(glm.results) = c('Intercept', x_vars.final)
summary(glm.results)

# Print coefficient averages
glm.coefs = as.data.frame(colMeans(glm.results))
colnames(glm.coefs) = c('coef')
glm.coefs$eq = paste(glm.coefs$coef, "*", rownames(glm.coefs))
glm.coefs


mfit=makeglm(
    response_var ~ csidf_csidf_score_woe + rv_score + home_time_36 + home_time_b3 + ids_idscore_woe + ai_income_freq_type_cd_woe + c_income_payment_cd_woe + csidf_reason_codes_b1 + csidf_reason_codes_b11 + csidf_reason_codes_b17 + csidf_idfraud_indicators_b10 + csidf_idfraud_indicators_b13 + email_length + c_income_type_cd_woe + bad_rate_individual_30_woe + days_before_next_payday_binned_1 + csidf_ssn_num_bank_accts_b4 + bad_month + bad_rate
    ,family=binomial
    ,data=train.comb
    ,2.119421244
    ,csidf_csidf_score_woe = 0.673712229
    ,rv_score = -0.005362188
    ,home_time_36 = 0.266625574
    ,home_time_b3 = -1.469074507
    ,ids_idscore_woe = 0.622132635
    ,ai_income_freq_type_cd_woe = 0.932238421
    ,c_income_payment_cd_woe = 1.304030094
    ,csidf_reason_codes_b1 = 0.187664967
    ,csidf_reason_codes_b11 = 0.445285018
    ,csidf_reason_codes_b17 = -0.398037971
    ,csidf_idfraud_indicators_b10 = -0.213245899
    ,csidf_idfraud_indicators_b13 = -0.162455700
    ,email_length = 0.021819097
    ,c_income_type_cd_woe = 0.601556089
    ,bad_rate_individual_30_woe = 0.531293236
    ,days_before_next_payday_binned_1 = -0.126765921
    ,csidf_ssn_num_bank_accts_b4 = 0.327387024
    ,bad_month = -0.305614994
    ,bad_rate = 1.381772906
    )


train.comb$pred_fin_def.boot=(predict(mfit,newdata=train.comb,type='response'))
ks_table.fit = gainsTable(train.comb,score='pred_fin_def.boot',bins=20,outcome='response_var',dec=T)
fit_ks=max(ks_table.fit$KS)
fit_ks
auc.fit=auc(train.comb[,'response_var'],train.comb[,'pred_fin_def.boot'])
auc.fit


# Save bootstrap coefficients
lists=c(lists,'glm.coefs')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#################################
# Check model on validation set #
#################################


# WOE variables
woe_data.test = iv.replace.woe(iv.replace.woe(data.test, woe_run.top), tb_run)
woe_data.test = woe_data.test[,woe_vars]


# Bin the validation set
binning_list.test = univariate_binner(all_vars_ranked,'response_var',traindata=data.train,newdata=data.test,print.tree=F,create.mapping=F)

# Binned variables
binned_data.test = binning_list.test$bins.matrix


# Continuous variables
cont_data.test.imp = predict(preProcValues, data.test[, colnames(cont_data.train)])


# Combine all variables
test.comb = cbind(cont_data.test.imp, woe_data.test, binned_data.test)
test.comb=cbind(test.comb[,x_vars.final],data.test[,c('response_var','approval_id')])


# Find KS on test set
test.comb$pred_fin_def=(predict(fit,newdata=test.comb,type='response'))
ks_table.test = gainsTable(test.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
test_ks=max(ks_table.test$KS)
test_ks
auc.test_fit=auc(test.comb[,'response_var'],test.comb[,'pred_fin_def'])
auc.test_fit



# Save test.comb data
lists=c(lists,'test.comb')
save(file='my_loan_default_storage.Rdata',list=unique(lists))


save(file='transform.Rdata',list=c('woe_run.top','tb_run','all_vars_ranked','data.train','preProcValues','x_vars.final','final_formula','cont_data.train','fit'))


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


##################################
# Check model on out of time set #
##################################


# WOE variables
woe_data.oot = iv.replace.woe(iv.replace.woe(data.oot, woe_run.top), tb_run)
woe_data.oot = woe_data.oot[,woe_vars]


# Bin the validation set
binning_list.oot = univariate_binner(all_vars_ranked,'response_var',traindata=data.train,newdata=data.oot ,print.tree=F,create.mapping=F)

# Binned variables
binned_data.oot = binning_list.oot$bins.matrix


# Continuous variables
cont_data.oot.imp = predict(preProcValues, data.oot[, colnames(cont_data.train)])


# Combine all variables
oot.comb = cbind(cont_data.oot.imp, woe_data.oot, binned_data.oot)
oot.comb=cbind(oot.comb[,x_vars.final],data.oot[,c('response_var','approval_id')])


# Find KS on oot set
oot.comb$pred_fin_def=(predict(fit,newdata=oot.comb,type='response'))
ks_table.oot = gainsTable(oot.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
oot_ks=max(ks_table.oot$KS)
oot_ks
auc.oot_fit=auc(oot.comb[,'response_var'],oot.comb[,'pred_fin_def'])
auc.oot_fit



# Save oot.comb data
lists=c(lists,'oot.comb')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#################
# Model Scoring #
#################


# Find KS of old model
ks_table.old = gainsTable(data,score='app_default_rate',bins=20,outcome='response_var',dec=T)
old_ks=max(ks_table.old$KS)
old_ks
## old_ks = .1535
old_auc=auc(data[,'response_var'],data[,'app_default_rate'])
old_auc

# Find KS on train set
#train.comb$pred_fin_def=(predict(fit,newdata=train.comb,type='response'))
ks_table.train = gainsTable(train.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
train_ks=max(ks_table.train$KS)
train_ks
train_auc=auc(train.comb[,'response_var'],train.comb[,'pred_fin_def'])
train_auc


# Find KS on test set
#test.comb$pred_fin_def=(predict(fit,newdata=test.comb,type='response'))
ks_table.test = gainsTable(test.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
test_ks=max(ks_table.test$KS)
test_ks
test_auc=auc(test.comb[,'response_var'],test.comb[,'pred_fin_def'])
test_auc




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


#############################
# Summary and plotting Info #
#############################





## Plot KS and AUC scores while number of variables increases
sweet_plot(train.comb,trim_gbm_summary,'response_var')



## Plot default rate by score bins
dd=cbind(train.comb[,'response_var'],train.comb[,'pred_fin_def'])
dd=dd[order(dd[,2],decreasing=T),]
colnames(dd)=c('actual','score')
dd=as.data.frame(dd)
dd=bin(dd,'score',20)
ddav=avgtable(dd)
plot(x=ddav[,'bin'],y=ddav[,'actual'],col='red',main='Score vs. Average Default Rate',xlab='Final Default Score Bin',ylab='Final Default Rate',cex=.4)
lines(x=ddav[,'bin'],y=ddav[,'actual'],col='blue')






## Plot KS of Old, Test, and Train data individually
ks_table.oldplot = gainsTable(data[which(data[,'app_default_rate'] != 0),],score='app_default_rate',bins=100,outcome='four_month_final_default_flg',dec=T)
ks_table.trainplot = gainsTable(train.comb,score='pred_fin_def',bins=100,outcome='response_var',dec=T)
ks_table.testplot = gainsTable(test.comb,score='pred_fin_def',bins=100,outcome='response_var',dec=T)

plotKSGraph(ks_table.oldplot, 'Previous Model', 'defaulted')
par(new=F)
plotKSGraph(ks_table.trainplot, 'Initial Default Model: Train', 'defaulted')
par(new=F)
plotKSGraph(ks_table.testplot, 'Initial Default Model: Validation', 'defaulted')



t.comb=rbind(train.comb[,c('approval_id',x_vars.final,'pred_fin_def','response_var')],test.comb[,c('approval_id',x_vars.final,'pred_fin_def','response_var')])
t.comb=join(t.comb,data[,c('approval_id','app_default_rate')],by='approval_id')

# Plot the spread between bins
data.b=bin(t.comb,'pred_fin_def',20)
predav=avgtable(data.b,'bin')
data.b=bin(data,'app_default_rate',20)
retroav=avgtable(data.b,'bin')
plot(x= predav[,'bin'],y= predav[,'response_var'],xlab='Retro/Predicted Default Bin',ylab='Final Default Rate',main='Loan Chain Final Default Rate Bin Spread (1 Loan)',cex=0,ylim=c(0,.35))
lines(x= predav[,'bin'],y= predav[,'response_var'],col='blue')
lines(x= retroav[,'bin'],y= retroav[,'response_var'],col='red')
legend(15,.1,c('Retro','New Model'),lwd=c(2.5,2.5),col=c('red','blue'),bty='n')




data.b=bin(t.comb,'pred_fin_def',20)
predav=avgtable(data.b,'bin')
data.b=bin(data,'default_rate',20)
retroav=avgtable(data.b,'bin')
plot(x= predav[,'bin'],y= predav[,'base_loan_net_fees'],xlab='Retro/Predicted Final Default Bin',ylab='Base Loan Net Fees',main='Retro vs Predicted Net Fees',cex=0,ylim=c(-250,600))
lines(x= predav[,'bin'],y= predav[,'base_loan_net_fees'],col='blue')
lines(x= retroav[,'bin'],y= retroav[,'base_loan_net_fees'],col='red')
legend(15,400,c('Retro','New Model'),lwd=c(2.5,2.5),col=c('red','blue'),bty='n')
lines(x=c(0,20),y=c(0,0),col='black')




t.comb=join(t.comb,data[,c('approval_id','loan_closed','base_loan_net_fees','default_rate')],by='approval_id')
data.b = t.comb[which(t.comb[,'loan_closed']==1 & is.na(t.comb[,'base_loan_net_fees'])==F ),]
data.b=data.b[order(data.b[,'pred_fin_def'],decreasing=T),]
cfees=matrix(,nrow(data.b),5)
colnames(cfees)=c('approval_id','pred_fin_def','default_rate','base_loan_net_fees','cumulative_sum')
cfees[,1] = data.b$approval_id
cfees[,2] = data.b$pred_fin_def
cfees[,3] = data.b$default_rate
cfees[,4] = data.b$base_loan_net_fees
cf=0
for (i in 1:nrow(cfees)){
    cf=cf+cfees[i,'base_loan_net_fees']
    cfees[i,'cumulative_sum'] = cf
}
plot(x=seq(1,nrow(cfees),1),y=cfees[,'cumulative_sum'],type='l',col='blue',xlab='Default Score',ylab='Cumulative Net Fees',main='Cumulative Net Fees',xaxt='n')
data.b=data.b[order(data.b[,'default_rate'],decreasing=T),]
cfees=matrix(,nrow(data.b),5)
colnames(cfees)=c('approval_id','pred_fin_def','default_rate','base_loan_net_fees','cumulative_sum')
cfees[,1] = data.b$approval_id
cfees[,2] = data.b$pred_fin_def
cfees[,3] = data.b$default_rate
cfees[,4] = data.b$base_loan_net_fees
cf=0
for (i in 1:nrow(cfees)){
    cf=cf+cfees[i,'base_loan_net_fees']
    cfees[i,'cumulative_sum'] = cf
}
lines(x=seq(1,nrow(cfees),1),y=cfees[,'cumulative_sum'],type='l',col='red')
lines(x=c(0,nrow(cfees)),y=c(0,0),col='black')
legend(0,3000000,c('Old Model','New Model'),lwd=c(2.5,2.5),col=c('red','blue'),bty='n')









plot.roc(test.comb[,'response_var'], test.comb[,'pred_fin_def']
                              , main='Loan Chain Final Default AUC (1 Loan)'
                              , percent=T
                              , ci=T
                              , xlim=c(100, 0)
                              , ylim=c(0, 100)
                              , col = "forest green")

plot.roc(train.comb[,'response_var'], train.comb[,'pred_fin_def']
                              , main='Final Default AUC'
                              , percent=T
                              , ci=T
                              , xlim=c(100, 0)
                              , ylim=c(0, 100)
                              , col = "blue"
                              , add=T)

plot.roc(data[,'response_var'], data[,'app_default_rate']
                              , main='Final Default AUC'
                              , percent=T
                              , ci=T
                              , xlim=c(100, 0)
                              , ylim=c(0, 100)
                              , col = "red"
                              , add=T)

#plot.roc(oot.comb[,'response_var'], oot.comb[,'pred_fin_def']
#                              , main='Final Default AUC'
#                              , percent=T
#                              , ci=T
#                              , xlim=c(100, 0)
#                              , ylim=c(0, 100)
#                              , col = "pink"
#                              , add=T)


legend(45,40,c('Old Model (KS = 14.91)','Training (KS = 25.98)','Validation (KS = 25.43)'),lwd=c(2.5,2.5,2.5),col=c('red','blue','forest green'),bty='n')

#legend(45,40,c('Old Model (KS = 10.43)','Training (KS = 27.26)','Validation (KS = 26.45)','OOT (KS=22.41)'),lwd=c(2.5,2.5,2.5,2.5),col=c('red','blue','forest green','pink'),bty='n')







#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################


####################################
# Super Sweet Repeated Over-Sample #
####################################




x_vars.remove = c(

)
imp_vars = c(setdiff(x_vars.final, x_vars.remove))
x_vars.add = c(

)
imp_vars = c(imp_vars,x_vars.add)
imp_vars = unique(imp_vars)


# apply WOE and binning rule to the full dataset
t.data=data
t.woe = iv.replace.woe(iv.replace.woe(t.data, woe_run.top), tb_run)
t.woe = t.woe[,woe_vars]
t.binning_list = univariate_binner(all_vars_ranked,'response_var',traindata=data.train,newdata=t.data,print.tree=F,create.mapping=F)
t.binned_data = t.binning_list$bins.matrix
t.cont_data = predict(preProcValues, t.data[, colnames(cont_data.train)])
t.comb = cbind(t.cont_data, t.woe, t.binned_data)

t.comb = cbind(t.comb[,x_vars.final],t.data[,c('response_var','approval_id')])


# randomly select 1500 null cases and ALL of the positive cases on each interaction
# run backwards stepwise, running until all variables have p-value no greater than .0001
# record the remaining significant variable from each iteration into the list
keepers=list()
v.repeats = 100
for (i in 1:v.repeats){
    t.vars=imp_vars
    t.samp=sample(1:nrow(data[which(data[,'response_var']==0),]),1500)
    t.data=rbind(t.comb[which(t.comb[,'response_var']==1),],t.comb[which(t.comb[,'response_var']==0),][t.samp,])
    t.formula = myformula(t.vars, 'response_var')
    t.fit = glm(t.formula, data=t.data, family='binomial')
    while (max(summary(t.fit)$coefficients[2:length(t.vars)+1,4]) > .0001){
        k = as.data.frame(which(summary(t.fit)$coefficients[2:length(t.vars)+1,4] == max(summary(t.fit)$coefficients[2:length(t.vars)+1,4])))[,1]
        t.vars = t.vars[-(k+1)]
        t.formula = myformula(t.vars, 'response_var')
        t.fit = glm(t.formula, data=t.data, family='binomial')
    }
    keepers[i] = list(t.vars)
}






# reverse KS stepwise
keepers=list()
v.repeats = 10
for (i in 1:v.repeats){
    t.vars=imp_vars
    t.samp=sample(1:nrow(t.comb[which(t.comb[,'response_var']==0),]),1500)
    t.data=rbind(t.comb[which(t.comb[,'response_var']==1),],t.comb[which(t.comb[,'response_var']==0),][t.samp,])
    while (length(t.vars) > 10){
        m=matrix(,length(t.vars),1)
        rownames(m)=t.vars
        for (l in 1:length(t.vars)){
            l.formula = myformula(t.vars[-l], 'response_var')
            l.fit = glm(l.formula, data=t.data, family='binomial')
            t.data$pred=NA
            t.data$pred = predict(l.fit,newdata=t.data,type='response')
            m[l,1]= max(gainsTable(t.data,score='pred',bins=20,outcome='response_var',dec=T)$KS)
        }
        t.vars = t.vars[-which(m[,1] == max(m[,1]))]

    }
    keepers[i] = list(t.vars)
}





#order variables by the frequency in which they appeared significant in stepwise iterations
output=matrix(,length(unique(unlist(keepers))),1)
rownames(output) = unique(unlist(keepers))
colnames(output) = 'frequency'
for (i in 1:length(unique(unlist(keepers)))){
    output[i,1] = length(which(unlist(keepers) == rownames(output)[i])) / v.repeats
}
output = as.matrix(output[order(output[,1],decreasing=T),])

output

#select the cutoff for minimum frequency required
t.final_vars = rownames(output)[which(output[,1] >= .5)]

#p-value stepwise
t.final_vars=c(

    )


#ks stepwise
t.final_vars=c(

    )





#run 10,000 glm model iterations to find averages for coefficients
#over sample and use ALL positive cases and randomly selected 1500 null cases
c.repeats = 1000
coefs = matrix(,c.repeats,length(t.final_vars)+1)
colnames(coefs)=c('intercept',t.final_vars)
for (i in 1:c.repeats){
    t.samp=sample(1:nrow(t.comb[which(t.comb[,'response_var']==0),]),1500)
    t.data=rbind(t.comb[which(t.comb[,'response_var']==1),],t.comb[which(t.comb[,'response_var']==0),][t.samp,])
    t.formula = myformula(t.final_vars, 'response_var')
    t.fit = glm(t.formula, data=t.data, family='binomial')
    coefs[i,]=t.fit$coefficients
}
coef_means = matrix(,2,ncol(coefs))
rownames(coef_means)=c('mean','standard_dev')
colnames(coef_means) = colnames(coefs)
for (i in 1:ncol(coefs)){
    coef_means[1,i] = mean(coefs[,i])
    coef_means[2,i] = sd(coefs[,i])
}

coef_means




#Score the original test/trian sets to get KS
#Important to score original sets since WOE was built on the training set
test.comb = cbind(cont_data.test.imp, woe_data.test, binned_data.test)
test.comb=cbind(test.comb[,t.final_vars],data.test[,c('response_var','loan_application_id')])

train.comb$pred_fin_def = NA
test.comb$pred_fin_def = NA

for (i in 1:nrow(train.comb)){
     train.comb[i,'pred_fin_def'] = exp(-4.15 + -0.0050746427*train.comb[i,'cc_gg'] + 1.0754583*train.comb[i,'cc_sic_b2'] + 0.29440413*train.comb[i,'cc_ul'] + 0.38342657*train.comb[i,'cc_eb_woe'] + 0.48026661*train.comb[i,'cc_ol_woe'])/(1+exp(-4.15 + -0.0050746427*train.comb[i,'cc_gg'] + 1.0754583*train.comb[i,'cc_sic_b2'] + 0.29440413*train.comb[i,'cc_ul'] + 0.38342657*train.comb[i,'cc_eb_woe'] + 0.48026661*train.comb[i,'cc_ol_woe']))
}


for (i in 1:nrow(test.comb)){
     test.comb[i,'pred_fin_def'] = exp(-4.15 + -0.0050746427*test.comb[i,'cc_gg'] + 1.0754583*test.comb[i,'cc_sic_b2'] + 0.29440413*test.comb[i,'cc_ul'] + 0.38342657*test.comb[i,'cc_eb_woe'] + 0.48026661*test.comb[i,'cc_ol_woe'])/(1+exp(-4.15 + -0.0050746427*test.comb[i,'cc_gg'] + 1.0754583*test.comb[i,'cc_sic_b2'] + 0.29440413*test.comb[i,'cc_ul'] + 0.38342657*test.comb[i,'cc_eb_woe'] + 0.48026661*test.comb[i,'cc_ol_woe']))
}


mean(train.comb[,'pred_fin_def'],na.rm=T)
mean(test.comb[,'pred_fin_def'],na.rm=T)

ks_table.train = gainsTable(train.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
train_ks=max(ks_table.train$KS)
train_ks

ks_table.test = gainsTable(test.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
test_ks=max(ks_table.test$KS)
test_ks



#Score full dataset and find KS
t.comb$pred_fin_def = predict(fit,newdata=t.comb,type='response')
t.comb$pred_fin_def.boot = predict(mfit,newdata=t.comb,type='response')


mean(t.comb[,'pred_fin_def'])

ks_table.all = gainsTable(t.comb,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
all_ks=max(ks_table.all$KS)
all_ks
#ks_table.all = gainsTable(t.comb[which(is.na(t.comb[,'eigth_installment_fin_def'])==F),],score='pred_fin_def',bins=20,outcome='eigth_installment_fin_def',dec=T)
#all_ks=max(ks_table.all$KS)
#all_ks


#Store the model formula
model_text = "-4.15 + -0.0050746427*t.comb[i,'cc_gg'] + 1.0754583*t.comb[i,'cc_sic_b2'] + 0.29440413*t.comb[i,'cc_ul'] + 0.38342657*t.comb[i,'cc_eb_woe'] + 0.48026661*t.comb[i,'cc_ol_woe']"



#Take 1500 random observations from the full dataset (without any oversampling) to test robustness of the KS and AUC
its = 100
ks_mat = matrix(,its+2,2)
colnames(ks_mat)=c('KS','AUC')
rownames(ks_mat)=c(seq(from=1,to=its,by=1),'Mean','STD')
for (i in 1:its){
    temp.data = t.comb[sample(1:nrow(t.comb),10000),]
    temp.ks_table = gainsTable(temp.data,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
    ks_mat[i,1]=max(temp.ks_table$KS)
    ks_mat[i,2]=auc(temp.data[,'response_var'],temp.data[,'pred_fin_def'])
}
ks_mat[its+1,1] = mean(ks_mat[1:its,1])
ks_mat[its+1,2] = mean(ks_mat[1:its,2])
ks_mat[its+2,1] = sd(ks_mat[1:its,1])
ks_mat[its+2,2] = sd(ks_mat[1:its,2])

#Print average KS/AUC with Standard Deviation
tail(ks_mat,2)




lists=c(lists,'t.comb','ks_mat')
save(file='my_loan_default_storage.Rdata',list=unique(lists))



## choose 500 instead of 1500
## get fourth installment Models
## run out of time sample to check models
## check ks against time periods (quarters) to show insensitivity
## high gauge score cases




t.comb=t.comb[order(t.comb[,'approval_id']),]


t.its = 10

t.coefs = matrix(,t.its,length(x_vars.final)+2)
colnames(t.coefs)=c('intercept',x_vars.final,'def_avg')
for (i in 1:t.its){
    t.data=t.comb[(1+(i-1)*(nrow(t.comb)%/%t.its)):((nrow(t.comb)%/%t.its)+(i-1)*(nrow(t.comb)%/%t.its)),]
    t.formula = myformula(x_vars.final, 'response_var')
    t.fit = glm(t.formula, data=t.data, family='binomial')
    t.coefs[i,]=c(t.fit$coefficients,mean(t.data[,'response_var']))
}
t.coef_means = matrix(,2,ncol(t.coefs))
rownames(t.coef_means)=c('mean','standard_dev')
colnames(t.coef_means) = colnames(t.coefs)
for (i in 1:ncol(t.coefs)){
    t.coef_means[1,i] = mean(t.coefs[,i])
    t.coef_means[2,i] = sd(t.coefs[,i])
}

t.coefs
t.coef_means


t.ks_mat = matrix(,t.its+2,3)
colnames(t.ks_mat)=c('KS','AUC','def_avg')
rownames(t.ks_mat)=c(seq(from=1,to=t.its,by=1),'Mean','STD')
for (i in 1:t.its){
    temp.data = t.comb[(1+(i-1)*(nrow(t.comb)%/%t.its)):((nrow(t.comb)%/%t.its)+(i-1)*(nrow(t.comb)%/%t.its)),]
    temp.ks_table = gainsTable(temp.data,score='pred_fin_def',bins=20,outcome='response_var',dec=T)
    t.ks_mat[i,1]=max(temp.ks_table$KS)
    t.ks_mat[i,2]=auc(temp.data[,'response_var'],temp.data[,'pred_fin_def'])
    t.ks_mat[i,3]=mean(temp.data[,'response_var'])
}
t.ks_mat[t.its+1,1] = mean(t.ks_mat[1:t.its,1],na.rm=T)
t.ks_mat[t.its+1,2] = mean(t.ks_mat[1:t.its,2],na.rm=T)
t.ks_mat[t.its+2,1] = sd(t.ks_mat[1:t.its,1],na.rm=T)
t.ks_mat[t.its+2,2] = sd(t.ks_mat[1:t.its,2],na.rm=T)



#Print average KS/AUC with Standard Deviation
t.ks_mat
#tail(t.ks_mat,2)
