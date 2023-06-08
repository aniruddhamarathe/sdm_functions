predict_maxent <- function(lambda,clamp = T,pred,out,
                           path_to_maxent = "D:\\software\\maxent_3.4\\maxent\\maxent.jar"){
  #from answers in https://groups.google.com/g/MAXENT/c/VUZu3ja_Wp0 and code
  #provided by Ahmed El-Gabbas
  #lambda = character.".lambda" file in the maxent output
  #pred = character. Folder where projection layers are stored
  #out = character. file name with full path for output file
  #path_to_maxent = character. Folder to locate maxent
  #clamp = logical. If TRUE do clamping 
  if(clamp){
    ARGS_Clamp <- c(" doclamp=true")
    system(paste0("java -cp ", path_to_maxent, " density.Project ", lambda, " ", pred, " ", out, " ", ARGS_Clamp))
    # OutRaster_Clamp <- readAll(raster(out, native=F))
    # projection(OutRaster_Clamp) <- projection(predictors[[1]])
    # return(OutRaster_Clamp)
    
  }else{
    ARGS_Clamp <- c(" doclamp=false")
    system(paste0("java -cp ", path_to_maxent, " density.Project ", lambdas, " ", EnvFolder, " ", out, " ", ARGS_Clamp))
  }
  
}


predict_maxent_spp <- function(path,preds,overwrite = F){
  #locates the 'lambdas' files in maxent output folder and generates output
  #which is mean of all model replicates; saves the output as ascii raster in a
  #separate folder 'projections' inside 'path'
  #overwtite : if TRUE redo the projection when file exists. else NULL
  if(!overwrite){
    spp_name <- tail(strsplit(path,"/")[[1]],n = 1)
    pred_name <- tail(strsplit(preds,"/")[[1]],n = 1)
    
    outname <- paste0(spp_name,"_",pred_name,"_mean.asc")
    if(
      file.exists(paste0(path,"/projections/",outname))){
      NULL}else{
    f <- list.files(path,pattern = ".lambdas",full.names = F)
    
    
    
    r <- lapply(f,
                function(x){
                  out_name <- gsub("lambdas","asc",x)
                  predict_maxent(lambda = paste0(path,"/",x),pred = preds,
                                 out = paste0("E:/backups/maxent_pred_temp/",
                                              out_name))
                  
                  raster(paste0("E:/backups/maxent_pred_temp/",out_name))
                })
    
    r <- stack(r)
    
    out <- calc(r,mean)
    
    spp_name <- tail(strsplit(path,"/")[[1]],n = 1)
    pred_name <- tail(strsplit(preds,"/")[[1]],n = 1)
    
    outname <- paste0(spp_name,"_",pred_name,"_mean.asc")
    if(!dir.exists(paste0(path,"/","projections"))){
      dir.create(paste0(path,"/","projections"))
    }
    
    writeRaster(out,paste0(path,"/projections/",outname),overwrite = T)
    outname}
  }else{
    
    f <- list.files(path,pattern = ".lambdas",full.names = F)
    
    
    
    r <- lapply(f,
                function(x){
                  out_name <- gsub("lambdas","asc",x)
                  predict_maxent(lambda = paste0(path,"/",x),pred = preds,
                                 out = paste0("E:/backups/maxent_pred_temp/",
                                              out_name))
                  
                  raster(paste0("E:/backups/maxent_pred_temp/",out_name))
                })
    
    r <- stack(r)
    
    out <- calc(r,mean)
    
    spp_name <- tail(strsplit(path,"/")[[1]],n = 1)
    pred_name <- tail(strsplit(preds,"/")[[1]],n = 1)
    
    outname <- paste0(spp_name,"_",pred_name,"_mean.asc")
    if(!dir.exists(paste0(path,"/","projections"))){
      dir.create(paste0(path,"/","projections"))
    }
    
    writeRaster(out,paste0(path,"/projections/",outname),overwrite = T)
    outname
  }
}

predict_maxent_batch <- function(preds,dsn){
  #expects folder structure as 'dsn/Family/species' to use predict_maxent_spp
  paths_fam <- list.dirs(dsn,
                         full.names = T,recursive = F)
  
  #paths_spp <- unlist(lapply(paths_fam,list.dirs,full.names = T,recursive = F))
  
  lapply(paths_fam,
         function(x){
           spp <- list.dirs(x,full.names = T,recursive = F)
           check <- c(grep("plots",x = spp,value = F),
                      grep("maxent.cache",x = spp,value = F))
           if(length(check)>0){
           spp <- spp[-check]}
           lapply(spp, predict_maxent_spp,preds = preds)
         })
}
