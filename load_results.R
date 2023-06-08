####@@@@get pred and eval@@@@####
fetch_eval <- function(dsn){
  #take maxent eval files and load as dataframe
  
  paths_fam <- list.dirs(dsn,
                         full.names = T,recursive = F)
  
  names_fam <- list.dirs(dsn,full.names = F,recursive = F)
  
  names(paths_fam) <- names_fam
  
  # paths_spp <- unlist(lapply(paths_fam,list.dirs,full.names = T,recursive = F))
  
  eval_current <- lapply(paths_fam,
                         function(x){
                           read.csv(
                             paste0(x,"/maxentResults.csv"),
                             header = T,row.names = 1
                           )
                         })
}

get_pred <- function(path,target,crs_obj = CRS("+proj=longlat +ellps=WGS84"),recursive){
  #expects the file matching 'target' to be raster
  #default crs is CRS("+proj=longlat +ellps=WGS84")
  f <- list.files(path,pattern = target,full.names = T,recursive = recursive) 
  
  
  if(length(f)>1){
    r <- stack(f)
  }else{
    r <- raster(f)
    
  }
  crs(r) <- crs_obj
  r
  
}

fetch_pred <- function(dsn,target,recursive = F){
  #expects folder structure as 'dsn/Family/species' to use get_pred
  #with recursive = T, subfolders within '/species' will be searched,
  #returns named list with '/species' as names  
  
  paths_fam <- list.dirs(dsn,
                         full.names = T,recursive = F)
  
  # names_fam <- list.dirs(dsn,full.names = F,recursive = F)
  # 
  # names(paths_fam) <- names_fam
  
  paths_spp <- unlist(lapply(paths_fam,
                             function(x){
                               spp <- list.dirs(x,full.names = T,recursive = F)
                               if(length(c(grep("maxent.cache",spp),grep("plots",spp)))>0){
                                 spp <- spp[-c(grep("maxent.cache",spp),
                                               grep("plots",spp)
                                 )]}
                               spp
                             })
  )
  
  names_spp <- do.call(rbind,strsplit(paths_spp,"/"))
  
  names(paths_spp) <- names_spp[,ncol(names_spp)]
  
  
  pred_current <- lapply(paths_spp,
                         function(y){
                           tryCatch(
                             get_pred(y,target = target,recursive = recursive),
                             error = function(e){
                               return(NULL)}
                           )
                         })
}



fetch_files <- function(dsn,target,fun,...){
  #expects folder structure as 'dsn/Family/species'
  #uses 'fun' on the files that contain target
  
  paths_fam <- list.dirs(dsn,
                         full.names = T,recursive = F)
  
  # names_fam <- list.dirs(dsn,full.names = F,recursive = F)
  # 
  # names(paths_fam) <- names_fam
  
  paths_spp <- unlist(lapply(paths_fam,
                             function(x){
                               spp <- list.dirs(x,full.names = T,recursive = F)
                               if(length(c(grep("maxent.cache",spp),grep("plots",spp)))>0){
                                 spp <- spp[-c(grep("maxent.cache",spp),
                                               grep("plots",spp)
                                 )]}
                               spp
                             })
  )
  
  names_spp <- do.call(rbind,strsplit(paths_spp,"/"))
  
  names(paths_spp) <- names_spp[,ncol(names_spp)]
  
  paths <- lapply(paths_spp,list.files,pattern = target,full.names = T)
  pred_current <- lapply(paths,fun,...)
  names(pred_current) <- names_spp[,ncol(names_spp)]
  
  pred_current
}
