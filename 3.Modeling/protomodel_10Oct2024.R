# Code to run main models
# Now amended to do lots of investigation and comparison stuff.
# Aug 28, 2024

#rm(list = ls())
library(purrr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(pROC)

setwd("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling")
savenameaffix = "11Oct2024"

# Function to use at estimation time ====
elogit.fit <- function(ytrain, Xtrain, Xtest, alpha = .5, usepredictors=NULL){
    
  #Don't deal with missingness -- assume it has already been dealt with
  #to force user to recognize it on the data side.
  xtrain = as.matrix(subset(Xtrain, select = usepredictors))
  xtest = as.matrix(subset(Xtest, select = usepredictors))
  set.seed(90035)
  
  # CJH: adding this to just fit the glmnet so we can see regularization path
  elastic.model <- glmnet(y = ytrain, x = xtrain, alpha = alpha, family = "binomial")
  
  #CJH: then back to this to produce cross-validated fit we need
  elastic.cv <- cv.glmnet(y=ytrain, x=xtrain, alpha=alpha, family="binomial")
  coeffs <- coef(elastic.cv, s = "lambda.min")
  elastic.predictions = predict(elastic.cv,newx=xtest, s="lambda.min", type="response")
  
  risk <- as.numeric(elastic.predictions)
  #out <- list(risk, coeffs, elastic.cv)
  out = list()
  out$risk = risk
  out$coeffs = coeffs
  out$elastic.cv = elastic.cv
  out$elastic.model = elastic.model #CJH: outputting this now as well
  return(out)
}

# Read in data ====
alldata = read.csv("prepared2023predictors-2024-10-10.csv")

# Define models ====

outcomenames = c("mk_onset","mk_onset_1","mk_onset_2", "mk_onset_1or2")

# Model "1"

predictornames.fullest <- c("widetargeting", "narrowtargeting", "v2csgender_binary", 
                            "v2mecenefm_binary", "partyban.new.0", "partyban.new.1", 
                            "partyban.new.2", "minorityrule", "judicialreform", 
                            "religiousfreedom", "pol_killing_approved", "freediscussion", 
                            "social_inequality", "even_civilrights", "repress_civilsoc", 
                            "social_power_dist", "ses_power_dist", "popsize.log2", 
                            "gdppcgrowth","un_imr_sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                            "reg.sca", "reg.mna", "reg.eur", "ios.iccpr1",  "battledeaths.log2", 
                            "coup.try.5yr", "mk_ongoing_count_log2", "mean_mk_onset_interaction", 
                            "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", "year",
                            "countryage.new.log2", "includesnonstate")



setdiff(predictornames.fullest, names(alldata))

# Non-prediction variables worth keeping around
extravariables = c("ewp_name","ccode","year")

#-----------------------------------------------------#
# Run models ==== 
# Do it in a tedious way, repeating code for each model
# to keep separate records 
#-----------------------------------------------------#

### Model 1: new, full ====

# Construct training and test data, pausing to look at missingness.
traindata.full = alldata %>% filter(year<=2021) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full)
table(traindata.full$year)


#now remove missing to compare
traindata.full = traindata.full %>% filter(complete.cases(.))
nrow(traindata.full)
table(traindata.full$year)


# Repeat for 2022's forecasts
# Construct training and test data, pausing to look at missingness.
traindata.full.2022 = alldata %>% filter(year<=2020) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full.2022)
table(traindata.full.2022$year)

#now remove missing to compare
traindata.full.2022 = traindata.full.2022 %>% filter(complete.cases(.))
nrow(traindata.full.2022)
table(traindata.full.2022$year)

# Test data:
testdata.full = alldata %>% filter(year==2023) %>% 
                      select(all_of(c(predictornames.fullest, extravariables)))

testdata.full.2022 = alldata %>% filter(year==2022) %>% 
  select(all_of(c(predictornames.fullest, extravariables)))

nrow(testdata.full)

testdata.full = testdata.full %>% filter(complete.cases(.))
nrow(testdata.full)

testdata.full.2022 = testdata.full.2022 %>% filter(complete.cases(.))
nrow(testdata.full.2022)


model.full.out = elogit.fit(ytrain=traindata.full$mk_onset_1or2, 
                  Xtrain = traindata.full,
                  Xtest = testdata.full,
                  alpha = .5,
                  usepredictors = predictornames.fullest)

# Check the regularization info 
elastic.model = model.full.out$elastic.model
colnames(elastic.model$beta) <- colnames(predictornames.fullest)


# Save the cv.glmnet object to a file
saveRDS(model.full.out$elastic.cv, file = "main_full_cv_glmnet_object_11Oct2024.rds")


model.full.out$coeffs

full.coefs.df = data.frame(
  variable = rownames(model.full.out$coeffs),
  coef.full = as.vector(model.full.out$coeffs))

#Add odds ratio
full.coefs.df = full.coefs.df %>% mutate(or.full = exp(coef.full))

results.full = data.frame(ewp_name = testdata.full$ewp_name, 
                     risk_1or2.from2023 = model.full.out$risk) %>%
  mutate(risk_rank.from2023 = rank(desc(risk_1or2.from2023), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2023))

# For last year (2022-based) predictions
model.full.2022.out = elogit.fit(ytrain=traindata.full.2022$mk_onset_1or2, 
                            Xtrain = traindata.full.2022,
                            Xtest = testdata.full.2022,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

model.full.2022.out$coeffs

full.2022.coefs.df = data.frame(
  variable = rownames(model.full.2022.out$coeffs),
  coef.full.2022 = as.vector(model.full.2022.out$coeffs))

#Add odds ratio
full.2022.coefs.df = full.2022.coefs.df %>% 
  mutate(or.full.2022 = exp(coef.full.2022))

results.full.2022 = data.frame(ewp_name = testdata.full.2022$ewp_name, 
                          risk_1or2.from2022 = model.full.2022.out$risk) %>%
  mutate(risk_rank.from2022 = rank(desc(risk_1or2.from2022), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2022))

#View(results.full.2022)

### Put together outputs in desired format ====

# first get one-year ahead forecast
model.1yr.out = elogit.fit(ytrain=traindata.full$mk_onset_1, 
                            Xtrain = traindata.full,
                            Xtest = testdata.full,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

results.1yr = data.frame(ewp_name = testdata.full$ewp_name, 
                         risk.2024only.from2023 = model.1yr.out$risk) %>%
  mutate(riskrank.2024only.from2023 = rank(desc(risk.2024only.from2023), na.last = "keep")) 

# Now merge everything up
bothforecasts <- results.full %>%
  left_join(results.1yr, by = "ewp_name")

# Merge in the prior year forecasts

bothforecastsandprior = bothforecasts %>% left_join(results.full.2022, by="ewp_name")

# Now merge that with dataset3
finalresults <- bothforecastsandprior %>%
  left_join(testdata.full, by = "ewp_name")

finalresults = finalresults %>% 
  rename(risk.2024and2025.from2023 = risk_1or2.from2023,
         rank.2024and2025.from2023 = risk_rank.from2023,
         risk.2023and2024.from2022 = risk_1or2.from2022, 
         rank.2023and2024.from2022 = risk_rank.from2022)

View(finalresults)

# Round as per Vincent's request
finalresults <- finalresults %>%
  mutate_if(is.numeric, ~signif(.x, 4))

# Now add 2022-based forecasts for comparison in prior year
write.csv(finalresults, file=paste0("final_forecasts_data_",savenameaffix,".csv"))


### Compare Regularization Paths

#regular model graph
coeffs <- as.matrix(model.full.out$elastic.cv$glmnet.fit$beta)
lambda_values <- model.full.out$elastic.cv$lambda

# Identify and remove the intercept
coeffs_no_intercept <- coeffs[rownames(coeffs) != "(Intercept)", ]
variable_names_no_intercept <- rownames(coeffs_no_intercept)

# Plot the regularization path without the intercept
matplot(log(lambda_values), t(coeffs_no_intercept), type = "l", lty = 1, col = 1:nrow(coeffs_no_intercept),
        xlab = "Log(Lambda)", ylab = "Coefficients", main = "Regularization Path - Regular Model")
abline(h = 0, lty = 2)


#Table of drop out times - regular model

# Initialize vector to store drop-out times
drop_out_times <- rep(NA, nrow(coeffs_no_intercept))
names(drop_out_times) <- rownames(coeffs_no_intercept)

# Identify drop-out times for each variable
for (i in 1:nrow(coeffs_no_intercept)) {
  # Find indices where coefficient is non-zero
  non_zero_idx <- which(coeffs_no_intercept[i, ] != 0)
  
  if (length(non_zero_idx) > 0) {
    # Record the lambda value where coefficient first becomes zero
    drop_out_times[i] <- lambda_values[min(non_zero_idx)]
  } else {
    drop_out_times[i] <- NA
  }
}

# Create a dataframe with variable names and drop-out times
drop_out_df <- data.frame(
  Variable = names(drop_out_times),
  Reg_DropOutTime = log(drop_out_times)
)

# Sort the dataframe by drop-out times from low to high (least important first)
drop_out_df_sorted <- drop_out_df[order(drop_out_df$Reg_DropOutTime), ]

# Add DropOutOrder column, with the least important variable labeled as 1
drop_out_df_sorted$Reg_DropOutOrder <- seq_len(nrow(drop_out_df_sorted))

#View(drop_out_df_sorted)

# Save the dataframe to a CSV file
library(data.table)
fwrite(drop_out_df_sorted, "reg_model_dropouts_10Oct.csv")



### Merge the results for comparison ====

# merge coefficients
all_coefs <- full.coefs.df 
  #full_join(offsetdecay.coefs.df, by="variable") 
 # full_join(simple.coefs.df, by="variable") 
# %>% full_join(fomchange.coefs.df, by = "variable") %>%
#  full_join(nofom.coefs.df, by="variable") %>%

all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

#make the sparsity more evident
all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~ifelse(. == 0.000, "0", .)))

print(all_coefs)
write.csv(file=paste0("coefficients_",savenameaffix, ".csv"), all_coefs)

# Compile forecasts and save
merged_results <- results.full
  #full_join(results.offsetdecay, by="ewp_name") 
 # full_join(results.simple, by="ewp_name") 
#%>% full_join(results.fomchange, by = "ewp_name") %>%
#  full_join(results.nofom, by = "ewp_name") %>%
 
merged_results <- merged_results %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

merged_results = merged_results %>% arrange(desc(risk_1or2.from2023))

#View(merged_results)

write.csv(file=paste0("forecasts",savenameaffix,".csv"), 
          merged_results)


### Setup FOOS testing regime ====

# Create a more generic training-testing function:
traintestyear = function(varnames, lasttrainyear, dvname="mk_onset_1or2"){
  
  traindata = alldata %>% filter(year<=lasttrainyear) %>% 
    select(all_of(c(varnames, outcomenames, extravariables))) %>% 
    filter(complete.cases(.))
  
  testdata = alldata %>% filter(year==lasttrainyear+2) %>% 
    select(all_of(c(varnames, extravariables, "mk_onset_1or2"))) %>%
    filter(complete.cases(.))
  
  trueoutcomes = alldata %>% filter(year==lasttrainyear+2) %>%
    select(mk_onset_1or2)
 
  ytrain = traindata$mk_onset_1or2

  model.out = elogit.fit(ytrain=ytrain, 
                             Xtrain = traindata,
                             Xtest = testdata,
                             alpha = .5,
                             usepredictors = varnames)
  
  minidata = data.frame(risk=model.out$risk, 
                        ewp_name=testdata$ewp_name, 
                        yearfrom = lasttrainyear+2,
                        ytrue=testdata$mk_onset_1or2)
  
  minidata = minidata %>% mutate(riskrank = rank(-risk, ties.method = "min"))
  
  recalls = getrecalls(data=minidata)
  #R = data.frame(year=lasttrainyear+2, recalls=recalls, minidata=minidata)
  R = list()
  R$year= lasttrainyear+2
  R$recalls = recalls
  R$minidata = minidata
  return(R)
}

getrecalls = function(data){
  numevents = sum(data$ytrue)
  
  riskdiff.annual = mean(data$risk[data$ytrue==1])-mean(data$risk[data$ytrue==0])
  riskratio.annual = mean(data$risk[data$ytrue==1])/mean(data$risk[data$ytrue==0])
  
  caught10 = data %>% arrange(desc(risk)) %>% slice(1:10) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught20 = data %>% arrange(desc(risk)) %>% slice(1:20) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught30 = data %>% arrange(desc(risk)) %>% slice(1:30) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  R = data.frame(numevents=numevents, caught10=caught10, 
                 caught20=caught20,caught30=caught30, 
                 riskdiff.annual=riskdiff.annual, riskratio.annual=riskratio.annual)
  return(R)
}

#traintestyear(varnames=predictornames.fullest, dvname = "sdads",lasttrainyear =1986)

# Define a vector of values to iterate over
# 1987 commonly used.
lasttrainyear.series <- seq(1991, 2020, by=1)

# Run with full new model (model 1)
#recallcheck_full <- data.frame(year = numeric(), 
#                        numevents = numeric(),
#                        caught10 = numeric(),
#                        caught20 = numeric(),
#                        caught30 = numeric())

recall_list = list()

FOOSdatalist = list()

varnamesnow = predictornames.fullest

for (j in 1:length(lasttrainyear.series)){
  lasttrainyear=lasttrainyear.series[j]
  print(lasttrainyear)
  
  thisout = traintestyear(lasttrainyear = lasttrainyear, 
                    varnames=varnamesnow, dvname = "sdads")
  #recallcheck_full = rbind(recallcheck_full,thisout)
  
  FOOSdatalist[[j]] = thisout$minidata
  
  recall_list[[j]] = thisout$recalls
}
              
FOOSdata_all = do.call(rbind, FOOSdatalist)
recalldata_all = do.call(rbind, recall_list)

FOOSdata_all = FOOSdata_all %>% rename(year = yearfrom)
#View(recallcheck_full) 

### Use two year mins/maxes as would-be forecasts.
yearmin = min(FOOSdata_all$year)+1
yearmax = max(FOOSdata_all$year)+2

allforecasts = alldata %>% filter(year>=yearmin & year<=yearmax) %>% 
 select(year, ewp_name, ccode, mk_onset)

# Add new columns for the previous and two years prior
allforecasts <- allforecasts %>% 
  mutate(prev_year = year - 1,
         two_years_prior = year - 2)

# Get "risk" and "rank" from the previous year
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(ewp_name, year, risk, riskrank), 
            by = c("ewp_name" = "ewp_name", "prev_year" = "year")) %>% 
  rename(risk_prev = risk, 
         rank_prev = riskrank)

# Get "risk" and "rank" from two years prior
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(ewp_name, year, risk, riskrank), 
            by = c("ewp_name" = "ewp_name", "two_years_prior" = "year")) %>% 
  rename(risk_two_years_prior = risk, 
         rank_two_years_prior = riskrank)

# Print the modified onsetdata with new columns

allforecasts = allforecasts %>% 
  mutate(maxrisk2 = pmax(risk_prev, risk_two_years_prior, na.rm=TRUE)) %>%
  mutate(minrank2 = pmin(rank_prev, rank_two_years_prior, na.rm=TRUE))

#View(allforecasts)

# Just the onsets, for recall purposes:
onsetforecasts = allforecasts %>% filter(mk_onset==1)

write.csv(allforecasts, file=paste0("FOOSforecasts_",savenameaffix,".csv"))

# Recall 30 on this:
mean(onsetforecasts$minrank2<=30)

# Recall "averaging by year":
yearlyrecall30 = onsetforecasts %>% group_by(year) %>% 
  summarize(yearrecall30=mean(minrank2<=30))

#View(yearlyrecall30)
table(yearlyrecall30$yearrecall30)
mean(yearlyrecall30$yearrecall30, na.rm=T)

# A plot of (max)risk in y=1 vs. y=0 cases
distplot = ggplot(allforecasts, aes(x = maxrisk2, fill = as.factor(mk_onset))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("No onset follows", "Onset follows")) +
  labs(fill = "Group",
       x = "Risk",
       y = "Density",
       title = "Density of Risk by Group",
       subtitle = "Grouped by onset following") +
  theme_minimal()

print(distplot)
ggsave(paste0("distplotFOOS_maxrisk2_",savenameaffix,".pdf"), plot = distplot, width=7, height=5, dpi=300)


## Other metrics using the max/min2 approach ====
(risk.mean.y1 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.mean.y0 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))
(risk.median.y1 = median(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.median.y0 = median(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))


(risk.mean.diff = risk.mean.y1 - risk.mean.y0)
(risk.median.diff = risk.median.y1 - risk.median.y0)

(risk.mean.ratio = risk.mean.y1/risk.mean.y0)
(risk.median.ratio = risk.median.y1/risk.median.y0)

# Plot empirical CDF of risk for countries with onsets
fractioncaught_025 = (1-ecdf(onsetforecasts$maxrisk2)(0.025))

catchplot = ggplot(onsetforecasts, aes(x = maxrisk2)) +
  stat_ecdf(geom = "step", colour = "blue") +
  labs(title = "OD Model - Empirical CDF of Risk for Countries with onsets after",
       x = "risk",
       y = "fraction caught") +
  theme_minimal() +
  scale_x_reverse() +
  geom_vline(xintercept = 0.025, linetype="dotted", color = "red") +
  geom_hline(yintercept = fractioncaught_025, linetype="dotted", color = "red") +
  annotate("text", x = 0.025, y = fractioncaught_025, label = sprintf("%.2f", fractioncaught_025), 
           hjust = -0.5, vjust = 0.5, color = "red")

print(catchplot)
ggsave(filename = paste0("catchplot_",savenameaffix,".pdf"), catchplot)


# Fraction of true among those at or above 4%"
onsetforecasts %>% 
  summarise(fraction_at_or_over_0.04 = mean(maxrisk2 >= 0.04, na.rm = TRUE),
            fraction_at_or_over_0.025 = mean(maxrisk2 >= 0.025, na.rm = TRUE))

# Get false positive per true positive. Could get from that table or by:
meanytrueatthreshold = allforecasts %>% filter(maxrisk2>.04) %>% 
  summarize(meany = mean(mk_onset, na.rm=TRUE))

# false positive to true positive
1/meanytrueatthreshold - 1

# Create a sample data frame
# Create the ROC curve
roc_curve <- roc(allforecasts$mk_onset, allforecasts$maxrisk2)

# Print the AUC
cat("AUC:", auc(roc_curve), "\n")

# Plot the ROC curve
plot(roc_curve, main="ROC Curve", col="blue", lwd=2)




STOPHEREERROR





# Additional analysis for 2024 update

### Comparison b/w 2023 and 2024 training data

#now remove 2021 to compare to last year's observations
traindata.no2021 = traindata.full %>% 
  filter(year != 2021)

nrow(traindata.no2021)

#import 2023 training data for comparison

traindata.2023 <- read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling/traindata2023.csv")
nrow(traindata.2023)

8571-8232

#see what's different between the two
differences <- traindata.no2021 %>% 
  anti_join(traindata.2023, by = c("ccode","year")) %>% 
  select(ewp_name, year)

#View(differences)
names(traindata.no2021)
names(traindata.2023)

traindata.2023 <- traindata.2023 %>% 
  select(country,year,ccode,everything())

traindata.no2021 <- traindata.no2021 %>% 
  select(ewp_name,year,ccode,everything())

# Rows in traindata.no2021 but not in traindata.2023
n_in_no2021_not_2023 <- nrow(traindata.no2021 %>% anti_join(traindata.2023, by = c("ccode", "year")))

# Rows in traindata.2023 but not in traindata.no2021
n_in_2023_not_no2021 <- nrow(traindata.2023 %>% anti_join(traindata.no2021, by = c("ccode", "year")))

# Print the results
n_in_no2021_not_2023
n_in_2023_not_no2021

in_2023_not_no2021 <- traindata.2023 %>% anti_join(traindata.no2021, by = c("ccode", "year"))
in_no2021_not_2023 <- traindata.no2021 %>% anti_join(traindata.2023, by = c("ccode", "year"))

View(in_2023_not_no2021)
View(in_no2021_not_2023)

in_no2021_not_2023 %>% 
  filter(mk_onset_1or2 == 1) %>% 
  View()

names(in_no2021_not_2023)

write.csv(in_2023_not_no2021,"in2023not2024.csv")

write.csv(in_no2021_not_2023,"in2024not2023.csv")

398-64


#View(traindata.2023)


# Get correlation of risk scores b/w 2023 and 2024 update

forecasts2024 <- read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling/final_forecasts_data_10Oct2024.csv")
forecasts2023 <- read.csv("/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/3.Modeling/final_forecasts_data_9Nov_hackregion.csv")

#remove risk.2023and2024.from2022 from 2024 forecasts

forecasts2024 <- forecasts2024 %>% 
  select(-risk.2023and2024.from2022)

mergedforecasts <-  forecasts2024 %>% 
  full_join(forecasts2023, by = c("ccode")) %>% 
  select(ccode,risk.2024and2025.from2023, risk.2023and2024.from2022)

#View(mergedforecasts)


correlation <- cor(mergedforecasts$risk.2024and2025.from2023, mergedforecasts$risk.2023and2024.from2022, use = "complete.obs")
print(correlation)

#See what ranking would have been using 2023 model on 2024 data

# Load the model object from the .rds file
model_path <- "/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/3.Modeling/model_full_out_27Sep_2024.rds"
model.2023 <- readRDS(model_path)

#Need to transform the 2024 data to match the 2023 training data

data2024 = read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/2.Make-new-base-data/2023-alldata--2024-10-10.csv")

#names(data2024)

#need to change newcountry and countryage based on "years in dataset" instead of independence

data2024 <- data2024 %>% 
  mutate(countryage_new_ln = log2(year - datastartyear),
    newcountry = ifelse(countryage_new_ln <= 2, 1, 0))


#fix scale for GDP per cap growth

data2024 <- data2024 %>% 
  mutate(gdppcgrowth.combined = gdppcgrowth/100) 

#fix scale for tradeshare

data2024 <- data2024 %>% 
  mutate(tradeshare.log2.combined = log2(2^tradeshare.log2 / 100))

#need to create "reg.afr"
data2024 <- data2024 %>%
  mutate(
    reg.afr = if_else(
      reg.na == 0 & reg.eap == 0 & reg.eur == 0 & reg.mna == 0 & reg.sca == 0,
      1, 0
    )
  )

#need to rename other vars, first have to remove popsize

data2024 <- data2024 %>% 
  select(-wdi.popsize.log2)

data2024 <- data2024 %>% 
  rename(imr.fwd.fill.sqrt = un_imr_sqrt,
         wdi.popsize.log2 = popsize.log2,
         battledeaths.ln2 = battledeaths.log2,
         mk_ongoing_count_log = mk_ongoing_count_log2
  )




predictornames.2023 <- c("mean_mk_onset","mk_onset_prev_year", "year", "year_sq",
                            "mk_ever", "mk_ongoing_count_log", "newcountry",
                            "widetargeting", "narrowtargeting",
                            "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
                            "countryage_new_ln", "wdi.popsize.log2",
                            "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
                            "minorityrule", "battledeaths.ln2","judicialreform",
                            "religiousfreedom", "pol_killing_approved","freediscussion",
                            "social_inequality","even_civilrights","repress_civilsoc",
                            "social_power_dist", "ses_power_dist","tradeshare.log2.combined",
                            "coup.try.5yr", "efindex", "discrimpop",
                            "partyban.new.0","partyban.new.1", "partyban.new.2",
                            "v2csgender_binary","v2mecenefm_binary", "imr.fwd.fill.sqrt")

extravariables.2023 = c("ewp_name","ccode")

# Test data:
testdata.2024 = data2024 %>% filter(year==2023) %>% 
  select(all_of(c(predictornames.2023,extravariables.2023)))

# Separate the predictors and extravariables
predictors_2024 <- testdata.2024[, predictornames.2023]  # Only predictors
extravariables_2024 <- testdata.2024[, c("ewp_name", "ccode", "year")]  # Keep extravariables

nrow(predictors_2024)


testdata.2024 %>% 
  filter(if_any(everything(), is.na)) %>% 
  select(ewp_name)


testdata.2024 = testdata.2024 %>% filter(complete.cases(.))
nrow(testdata.2024)


# Step 3: Make predictions
predictions <- predict(model.2023, 
                       newx = as.matrix(predictors_2024), 
                       s = "lambda.min", type = "response")

# Create a results data frame
results.comparison <- data.frame(ewp_name = extravariables_2024$ewp_name, 
                                risk_2023model = as.vector(predictions)) %>%
  mutate(rank_2023model = rank(desc(risk_2023model), na.last = "keep")) %>% 
  arrange(desc(risk_2023model)) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

View(results.comparison)


#round the original results
results.full <- results.full %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

#merge with results from new model 

full.comparison <- results.full %>% 
  full_join(results.comparison, by = c("ewp_name"))

full.comparison <- full.comparison %>% 
  mutate(risk_difference = risk_1or2.from2023 - risk_2023model,
    rank_difference = risk_rank.from2023 - rank_2023model)


#rename for clarity

full.comparison <- full.comparison %>% 
  rename(risk.2024moodel = risk_1or2.from2023,
         rank.2024model = risk_rank.from2023) %>% 
  arrange(rank_difference)


View(full.comparison)


write.csv(file=paste0("forecasts_comparisons_",savenameaffix,".csv"), 
          full.comparison)


# Get average risk of mk onset all time
# Load necessary libraries
library(dplyr)

average_probabilities <- alldata %>%
  group_by(year) %>%  # Group data by year
  summarize(
    onsets = sum(mk_onset),  # Total onsets in the year
    total_countries = n() # Total number of countries (rows) in the year
  ) %>%
  mutate(probability = (onsets / total_countries) * 100) %>%  # Calculate probability as percentage
  summarize(average_probability = mean(probability))  # Calculate overall average

# Print the overall average probability
print(average_probabilities$average_probability)

