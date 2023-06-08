# sdm_functions
functions for importing and loading outputs from SDMs. 

The functions that import output files typically expect a data_source/family/species folder structure and generate a named list of the output with species as names. 

Functions for predicting output from maxent were originally writted to bring the output generated from GUI interface into R. However, existing output within R using eg. dismo or ENMeval, can be used by first dumping the .lambdas files on disk. This skips loading the newdata rasters into workspace. 
