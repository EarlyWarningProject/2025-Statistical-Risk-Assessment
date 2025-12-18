library(pbmcapply)
library(glmnet)
library(PRROC)

# function to create training and test datasets for each base year
create_datasets <- function(base_year = "2017", outcomenames, predictornames){
    
    subset <- subset(dat, select = c("sftgcode","year", outcomenames, 
                                  predictornames, "country_name","year"))
    
    keep <- as.integer(base_year) - 2
    
    # Train on all 
    yXtrain <- na.omit(subset) #drops years with NA any.mk variables
    yXtrain <- yXtrain[yXtrain$year <= keep, ]
    Xtrain <- subset(yXtrain, select = predictornames)
    ytrain_1yr <- subset(yXtrain, select = outcomenames[1]) #single leaded year outcome
    ytrain_2yr <- subset(yXtrain, select = outcomenames[2]) # two year window outcome
    
    # Prediction time data:
    Xtest <- na.omit(subset(dat[dat$year == as.integer(base_year),], 
                            select =  c("sftgcode", "COWcode", predictornames, "country_name","year")))
    out <- list(Xtest, Xtrain, ytrain_1yr, ytrain_2yr)
    names(out) <- c("Xtest", "Xtrain", "ytrain_1yr", "ytrain_2yr")
    return(out)
}

# function to run model 
model <- function(ytrain, Xtrain, Xtest, alpha = .5, predictornames){
    
    elastic.cv <- cv.glmnet(y=unlist(ytrain),  
                            x=as.matrix(subset(Xtrain, select = predictornames)), 
                            alpha=alpha, family="binomial")
    coeffs <- coef(elastic.cv, s = "lambda.min")
    
    elastic.predictions = signif(predict(elastic.cv, 
                                                   newx=as.matrix(subset(Xtest, select = predictornames)), 
                                                   s="lambda.min", type="response"),4)
    
    risk <- as.numeric(elastic.predictions)
    out <- list(risk, coeffs)
    out
}

# function to format results, model embedded
# 1) added year to export variables
# 2) stopped reordering the results
# 3) stopped renaming risk1yr to risk_1980 etc

format_results <- function(base_year, predictornames, outcomenames){
    
    dat_list <- create_datasets(base_year = base_year, 
                                predictornames = predictornames, 
                                outcomenames = outcomenames)
    #list2env(dat_list, .GlobalEnv)
    
    risk.1yr <- model(ytrain = dat_list$ytrain_1yr, 
                      Xtrain = dat_list$Xtrain,
                      Xtest = dat_list$Xtest,
                      predictornames = predictornames)
    risk.2yr <- model(ytrain = dat_list$ytrain_2yr, 
                      Xtrain = dat_list$Xtrain, 
                      Xtest = dat_list$Xtest,
                      predictornames = predictornames)
    
    coeffs1 <- as.data.frame(as.matrix(risk.1yr[[2]]))
    coeffs1$vars <- rownames(coeffs1)
    coeffs2 <- as.data.frame(as.matrix(risk.2yr[[2]]))
    coeffs2$vars <- rownames(coeffs2)
    coeffs <- merge(coeffs1, coeffs2, by = "vars")
    colnames(coeffs) <- c("Variables", "Weights for 1-year forecast", "Weights for 2-year forecast")
    
    dat_list$Xtest$risk.1yr <- risk.1yr[[1]]
    dat_list$Xtest$risk.2yr <- risk.2yr[[1]]
    
    everything <- subset(dat_list$Xtest,
                         select = c("country_name", "sftgcode", "COWcode", 
                                    "risk.1yr", "risk.2yr", "year"))

    out <- list( everything = everything, coeffs = coeffs)
    return(out)
}

#parallelization will not work on Windows

run_model_multiyear <- function(startyear, endyear, model.name, predictornames, outcomenames, ...){

    #print title, but doesn't matter when using Markdown
    #cat("Generating model historical predictions.")
    base_years <- c(as.character(startyear:endyear))
    predictornames <- unlist(predictornames)
    outcomenames <- unlist(outcomenames)
    
    #check if a file already exists
    file.path <- list.files(path="performance/compare-models/models", 
                            pattern=paste0(model.name,".Rdata"), full.names = TRUE)
    
    #if it does, load it, otherwise, re-run
    if(length(file.path)==0){
    
        #print that you're generating a new model
        print(paste0("Generating model run for ", model.name))
        
        # run the model on multiple cores, should take less than 2 min
        results <- pbmclapply(base_years, 
                              function(x) format_results(base_year = x,
                                        predictornames = predictornames,
                                        outcomenames = outcomenames),
                              mc.cores =  detectCores() - 1)
        
        
        #save results for future use
        save(results, file = paste0("performance/compare-models/models/", 
                                    model.name, ".Rdata"))
        
    } else {
        
        #load the pre-existing model run
        load(file.path)
        
        #print that you're generating a preloaded file
        print(paste0("Using ", model.name, " model generated on ", 
                     str_extract(file.info(file.path)$ctime, "[0-9-]*")))
    
    }
    
    #return the predictions and original data
    everything <- lapply(results, function(x) x[[1]])
    names(everything) <- paste0("everything_", base_years)
    
    #return the model coefficients
    coeffs <- lapply(results, function(x) x[[2]])
    names(coeffs) <- paste0("coeffs_", base_years)
    
    return(tibble(everything = everything, coeffs = coeffs))
    
}

grab_predictions <- function(results, ...){
    
    #extract the predictions and reshape into country-year matrix
    
        #extract
        predictions_2year <- lapply(results$everything, function(x) x[,c("country_name",
                                                                   "sftgcode",
                                                                   "risk.1yr", 
                                                                   "risk.2yr", 
                                                                   "year")]) %>% do.call(rbind,.)
        
        #add missing observations as NAs instead of missing rows
        ref <- expand.grid(year=unique(predictions_2year$year), 
                           country_name=unique(predictions_2year$country_name), 
                           stringsAsFactors = FALSE)
        ref <- left_join(x=ref, y=unique(predictions_2year[,c("country_name", "sftgcode")]), by="country_name")
        predictions_2year <- left_join(ref, predictions_2year, by=c("country_name", "year", "sftgcode"))
        predictions_2year <- predictions_2year[order(predictions_2year$country_name, predictions_2year$year),]
        
        #merge the actual outcomes from the training data
        add.vars <- c("country_name", "year", "anymk.start","anymk.ongoing", "anymk.start.2window")
        predictions_2year <- left_join(predictions_2year, dat[,add.vars], by=c("country_name", "year"))
        
        #return
        return(predictions_2year)
        }

grab_coefs <- function(results, ...){
    
        #define base years
        base_years <- c(as.character(startyear:endyear))
    
        #extract the coefficients 
        coeffs <- lapply(results$coeffs, function(x) x[,3]) %>% do.call(rbind,.) %>% t() %>% data.frame
        coeffs$variable <- results$coeffs[[2]][,"Variables"]
        coeffs <- gather(coeffs, year, estimate, paste0("coeffs_", base_years))
        coeffs$year <- as.numeric(str_extract(coeffs$year,"[0-9]{4}"))
        
        #export
        return(coeffs)
        }


get_auc <- function(predictions_2year, ...){
    
    #drop observations with NAs
    predictions_2year <- filter(predictions_2year, 
                                !is.na(risk.2yr),
                                !is.na(anymk.start.2window))
    
    auroc <- pROC::auc(roc(
        cases = predictions_2year$risk.2yr[predictions_2year$anymk.start.2window ==1],
        controls = predictions_2year$risk.2yr[predictions_2year$anymk.start.2window ==0],
        direction = "<"))[[1]]
    
    aupr <- pr.curve(scores.class1=predictions_2year[predictions_2year$anymk.start.2window==0, "risk.2yr"],
                     scores.class0=predictions_2year[predictions_2year$anymk.start.2window==1, "risk.2yr"])$auc.integral
    
    return(list("auroc"=auroc, "aupr"=aupr))
}

key_stats <- function(predictions_2year, startyear,...){
    
    n.countries <- data.frame(
        value = length(unique(predictions_2year$sftgcode)),
        description = "number of countries"
    )
    
    n.obs <- data.frame(
        value = nrow(predictions_2year[predictions_2year$year >= startyear,]),
        description = "number of observations since start year"
    )
    
    key.stats <- bind_rows(
        n.countries = n.countries,
        n.obs = n.obs, 
        .id = "key"
    )
    
    return(key.stats)
    
}

