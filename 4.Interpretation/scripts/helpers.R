
    #general
    library(tidyverse)
    library(reshape)
    library(cobalt)  #f.build function
    library(lubridate)
    library(stringr)
    library(pROC)
    
    
    #tables
    library(stargazer)
    library(xtable)
    
    #robust standard errors
    library(lmtest)
    library(sandwich)    
    
    p.value <- function(x) ifelse(x <= .01, "$^{***}$", ifelse(x <= .05, "$^{**}$", ifelse(x <= .1, "$^{*}$", "")))
    
    hijack <- function (FUN, ...) {
        .FUN <- FUN; args <- list(...)
        invisible(lapply(seq_along(args), function(i) {
            formals(.FUN)[[names(args)[i]]] <<- args[[i]]
        })); .FUN }

    
    
    
    versionControl <- function(script, output, frequency = "minute", interactive = FALSE){
        
        #get current directory either from RStudio or knitr
        if(length(knitr::current_input()) == 0){  
            #being run from the console, as Rstudio
            input.script <- rstudioapi::getSourceEditorContext()$path 
            #if(!grepl(".Rnw$", input.file)) { stop("Only .Rnw files currently supported")}
        } else {  
            #being run by knitr
            input.script <- knitr::current_input(dir=TRUE)  
        } 
        
        #get the base directory
        dir <- str_extract(input.script, "^.*/")
        
        #check if there's a version control folder, if not, create one
        dir.create(file.path(dir, "version_control"), showWarnings = FALSE)
        
        #check the date modified, NOT USING THIS
        
        if(frequency == "daily"){
            stamp <- Sys.Date()
        } else if(frequency == "hourly"){
            stamp <- gsub(" ", "-", gsub(":00:00", "", round_date(Sys.time(), "hour"), fixed=TRUE))
        } else if(frequency == "minute"){
            stamp <- gsub(":", "-", gsub(" ", "-", gsub(":00$", "", round_date(Sys.time(), "minute"))), fixed = TRUE)
        } else {stop("Unknown frequency type, please specify one of: daily, hourly, or minute ")}
        
        #select the rendered file
        input.file <- gsub(script, output, input.script)
        
        #stamp date
        output.file <- gsub(output, paste0("_", stamp, output), input.file) 
        output.script <- gsub(script, paste0("_", stamp, script), input.script) 
        
        #change directory
        output.file <- gsub(dir, paste0(dir, "version_control/"), output.file) 
        output.script <- gsub(dir, paste0(dir, "version_control/"), output.script)
        
        #copy, and break if the copying fails
        stopifnot(file.copy(input.file, output.file, overwrite=TRUE))
        stopifnot(file.copy(input.script, output.script, overwrite=TRUE))
        
        #add notes
        comments <- function() {
            answer <- readline("Please enter any notes about this update: ")
            if(nchar(answer) > 0) {
                output.txt <- gsub(script, paste0("_", stamp, ".txt"), input.file) 
                output.txt <- gsub(dir, paste0(dir, "version_control/"), output.txt) 
                writeLines(answer, con=output.txt)
            }
        }
        if(interactive){ comments() }
        
    }
    