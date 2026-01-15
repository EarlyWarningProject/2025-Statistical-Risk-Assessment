Replication files for the 2025 edition of USHMM's statistical risk assessment, using R version 2025.05.0+496.

To reproduce the input data "prepared2024predictors-2024-09-03.csv", start with 1.Buildcountryyears_and_mk_data. 
The file "build_countryyears_mkevents_2025.R" creates the initial country-year dataset with mass killing data from 1945 - 2024, available in the file "basedata2025_4aug2025.csv". 
The files stored in 2.Make-new-base-data take this initial template and add data on other variables as well as new data for 2024. 
The file "Adding in new data-2025.Rmd" is an R Markdown file that appends data for each country in 2024 to the prepared data through 2023. 
This creates the input file used for modeling, "prepared2023predictors-2025-09-03.csv".
To run the models and replicate the risk rankings without reproducing the base data, go to "3.Modeling" and open the file "model_3Sep2025.R". 
Use the input file "prepared2024predictors-2025-09-03.csv" that is available in the same folder. 
"final_forecasts_data_3Sep2025.csv" and "coefficients_3Sep2025.csv" are the outputs of the "model_3Sep2025.R" file. 
The former file shows the predicted probabilities for each country, and the latter shows the weights for each of the predictors selected.
