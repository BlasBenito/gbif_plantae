nball_volume <- function(n, r)
{
  return(pi^(n/2) * r^n / gamma(n/2+1))
}

kdtree_build_intl <- function(d, nr, nc) {
  .Call('_hypervolume_kdtree_build_intl', PACKAGE = 'hypervolume', d, nr, nc)
}

kdtree_ball_query_multiple <- function(tr, ptlist, nr, nc, r, verb) {
  .Call('_hypervolume_kdtree_ball_query_multiple', PACKAGE = 'hypervolume', tr, ptlist, nr, nc, r, verb)
}

kdtree_ball_query_id_multiple <- function(tr, ptlist, nr, nc, r, verb) {
  .Call('_hypervolume_kdtree_ball_query_id_multiple', PACKAGE = 'hypervolume', tr, ptlist, nr, nc, r, verb)
}

kdtree_range_query_multiple <- function(tr, pminlist, pmaxlist, nr, nc, verb) {
  .Call('_hypervolume_kdtree_range_query_multiple', PACKAGE = 'hypervolume', tr, pminlist, pmaxlist, nr, nc, verb)
}

fastPdist2 <- function(Ar, Br) {
  .Call('_hypervolume_fastPdist2', PACKAGE = 'hypervolume', Ar, Br)
}

kdtree_build <- function(data, verbose=TRUE)
{
  if (any(class(data) == "data.frame"))
  {
    data <- as.matrix(data)
  }
  if (any(class(data) == "matrix"))
  {
    if (verbose==TRUE)
    {
      cat("\nBuilding tree... \n")
    }
    
    kdt <- kdtree_build_intl(t(data),nrow(data),ncol(data))
    
    if (verbose==TRUE)
    {
      cat("done.\n")
    }
    
    return(kdt)
  }
  else
  {
    stop("Input data not a matrix or data frame")
  }
}

sample_ellipsoid = function(center, n, scales) {
  k = length(center)
  points = matrix(rnorm(n * k), nrow = n)   # Start with noise
  points = points / sqrt(rowSums(points^2)) # Project to sphere surface
  radii = runif(n, 0)^(1/k) # Each point has a radius, prior to scaling
  for (i in 1:k) {
    points[,i] = points[,i] * radii * scales[i] + center[i]
  }
  return(points)
}

ellipsoid_volume = function(scales) {
  nball_volume(n = length(scales), r = 1) * prod(scales)
}

ellipsoid_inverse_weight = function(samples, centers, scales, verbose) {
  # Returns the number of center-based ellipsoids that contain each sampled point
  
  # scale to unit ball
  for (i in 1:ncol(centers)) {
    samples[ , i] = samples[ , i] / scales[i]
    centers[ , i] = centers[ , i] / scales[i]
  }
  
  # Count the overlap with unit ball
  tree <- kdtree_build(centers,verbose=verbose)
  query <- kdtree_ball_query_multiple(tree, t(samples), 
                                      nrow(samples), ncol(samples), 
                                      r = 1, verb = verbose)
  rm(tree)
  
  return(query)
}


sample_model_ellipsoid <- function(predict_function=NULL, data, scales, min.value, samples.per.point, chunk.size=1e3, verbose=TRUE, return.full=FALSE)
{
  # Use only complete cases
  data = na.omit(as.matrix(data))
  
  # determine dimensionality
  d = ncol(data)
  
  N.samples <- ceiling(samples.per.point * nrow(data))
  
  if (is.null(dimnames(data)[[2]]))
  {
    dimnames(data) <- list(NULL,paste("X",1:d,sep=""))
  }
  
  if (verbose==TRUE)
  {
    pb <- progress_bar$new(total = N.samples)
    pb$tick(0)
  }
  
  samples = list()
  volume_sampling_extent_all = list()
  total_accepted <- 0
  total_tried <- 0
  
  while(total_accepted < N.samples)
  {
    if (verbose==TRUE)
    {
      if (!pb$finished==TRUE)
      {
        pb$update(total_accepted/N.samples)
      }
    }
    
    ## STEP ONE: Collect samples from ellipsoids around each data point
    full_samples = lapply(1:nrow(data),
                          function(i)
                          {
                            se = sample_ellipsoid(data[i, ], 
                                                  chunk.size, 
                                                  scales = scales)
                            return(data.frame(se))
                          })
    full_samples = as.matrix(rbindlist(full_samples))
    
    
    
    # Discard samples from regions that were over-sampled.
    iweight = ellipsoid_inverse_weight(full_samples, centers = data, 
                                       scales = scales, verbose = verbose)
    # scale weights so that the largest value is 1
    weight = 1/(iweight / min(iweight))
    # This is the average weight of the samples
    mean_weight = sum(1/iweight) / nrow(full_samples)
    # Total volume is the volume of one ellipse, times the number of ellipse,
    # times the proportion of samples that can be retained, times the fraction of points included above threshold
    volume_sampling_extent = ellipsoid_volume(scales) * nrow(data) * mean_weight
    
    # resample sampled points down to uniform density
    included = as.logical(rbinom(length(iweight), size = 1, prob = weight))
    
    # now we have a uniform grid of points around each data point, samples_retained
    samples_retained = full_samples[included, , drop = FALSE]
    
    ### STEP TWO: estimate function
    # predict function value at each point
    predicted_values <- predict_function(samples_retained)
    
    included_thresholded = ( as.numeric(predicted_values) > min.value )
    
    samples_retained_thresholded = samples_retained[included_thresholded, , drop=FALSE]
    predicted_values_thresholded = predicted_values[included_thresholded]
    
    samples_final_this = cbind(samples_retained_thresholded, predicted_values_thresholded)
    dimnames(samples_final_this) <- list(NULL, c(dimnames(data)[[2]],"value"))
    
    # count progress
    total_tried <- total_tried + nrow(samples_retained) # for eventual volume counting
    total_accepted <- total_accepted + nrow(samples_retained_thresholded) 
    
    # store loop output
    samples <- c(samples, list(data.frame(samples_final_this))) # must be df format for rbindlist
    volume_sampling_extent_all <- c(volume_sampling_extent_all, volume_sampling_extent)
  }
  
  # concatenate results  
  samples <- as.matrix(rbindlist(samples))
  samples <- samples[sample(1:nrow(samples),N.samples),,drop=FALSE]
  # calculate volumes
  volume_sampling_extent_all_mean = mean(unlist(volume_sampling_extent_all),na.rm=T)
  volume = volume_sampling_extent_all_mean * total_accepted / total_tried
  
  if (verbose==TRUE)
  {
    pb$terminate()
  }
  
  if (return.full==TRUE)
  {
    return(list(samples = samples, full_samples=full_samples, volume=volume))
  }
  else
  {
    return(list(samples = samples, volume=volume))
  }
}

hypervolume_svm_ <- function(data, name=NULL, samples.per.point=ceiling((10^(3+sqrt(ncol(data))))/nrow(data)), svm.nu=0.01, svm.gamma=0.5, scale.factor=1, chunk.size=1e3, verbose=TRUE)
{
  
  require(hypervolume)
  
  data <- as.matrix(data)
  d <- ncol(data)
  if (is.null(dimnames(data)[[2]]))
  {
    dimnames(data) <- list(NULL,paste("X",1:d,sep=""))
  }
  
  if (svm.nu <= 0)
  {
    stop("Parameter svm.nu must be >0.")
  }
  
  if (svm.gamma <= 0)
  {
    stop("Parameter svm.gamma must be >0.")
  }
  
  if (verbose == TRUE)
  {
    cat('Building support vector machine model...')
  }
  svm_this <- e1071::svm(data,
                         y=NULL,
                         type='one-classification',
                         nu= svm.nu,
                         gamma= svm.gamma,
                         scale=FALSE,
                         kernel="radial")	
  if (verbose == TRUE)
  {
    cat(' done\n')
  }
  
  # Assuming all of the support vectors were right on top of each other,
  # how far could we move away from that point before we left the interior
  # of the hypervolume? 
  # `solve b * exp(-g d_2) == r for d_2` into Wolfram Alpha, where
  #    * "b" is the sum of the SVM coefficients
  #    * "g" is the SVM kernel bandwidth (gamma)
  #    * "r" is the minimum kernel density of the hypervolume (rho)
  #    * "d_2" is squared distance  
  squared_scaled_dist = log(sum(svm_this$coefs) / svm_this$rho) / svm.gamma
  scales = scale.factor * apply(data, 2, sd) * sqrt(squared_scaled_dist)  
  
  predict_function_svm <- function(x)
  {
    return(predict(svm_this, x))
  }  
  
  # do elliptical sampling over the hyperbox
  samples_all = sample_model_ellipsoid(
    predict_function = predict_function_svm,
    data = data[svm_this$index,,drop=FALSE],
    scales = scales, 
    min.value = 0, # because output is binary
    samples.per.point = samples.per.point, 
    chunk.size=chunk.size, 
    verbose=verbose)
  
  random_points = samples_all$samples[,1:d]
  values_accepted <- samples_all$samples[,d+1]
  volume <- samples_all$volume
  point_density = nrow(random_points) / volume
  
  hv_svm <- new("Hypervolume",
                Data=data,
                Method = 'One-class support vector machine',
                RandomPoints= random_points,
                PointDensity= point_density,
                Volume= volume,
                Dimensionality=d,
                ValueAtRandomPoints=values_accepted,
                Name=ifelse(is.null(name), "untitled", toString(name)),
                Parameters = list(svm.nu=svm.nu, svm.gamma=svm.gamma, samples.per.point=samples.per.point))
  
  return(hv_svm)	
}


#' Automatic variance inflation factor (VIF) analysis for variable selection
#'
#' @description Selects variables within a training dataframe that are not linear combinations of other variables by using the variance inflation factor (VIF) criteria implemented in the \code{\link[HH]{vif}} function (Heilberger and Holland 2004). This function has three modes:
#' \itemize{
#' \item 1. When the arguments \code{preference.order} and \code{biserial.cor} are \code{NULL}: It removes on on each iteration the variable with the highest VIF until all VIF values are lower than 5. This operation is performed by the hidden function \code{.select_by_max_vif}.
#' \item 2. When the argument \code{biserial.cor} is provided with an object of the class \code{s_biserial_cor} produced by the function \code{\link{s_biserial_cor}}: It adds variables one by one in the order of preference defined by the \code{s_biserial_cor} object. Any variable increasing the VIF value of any other variable beyond 5 is not added to the final variable selection. This operation is performed by the hidden function \code{.select_by_preference}. This is the most recommended option for this analysis.
#' \item 3. When the argument \code{preference.order} is provided: The variables in \code{preference.order} are selected as shown above in option 2, the variables not in \code{preference.order} are selected as in option 1, and finally, all variables are put together and selected again as in option 2. This method preserves the variables desired by the user as much as possible.
#' }
#'
#' @usage auto_vif(
#'   training.df,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   preference.order = NULL,
#'   biserial.cor = NULL,
#'   verbose = TRUE
#' )
#'
#' @param training.df A training data frame. Non-numeric columns are excluded from the analysis.
#' @param select.cols Character vector, names of the columns which VIF wants to be assessed. If \code{NULL}, all numeric variables but \code{presence.column} are considered. It is recommended to use the variable order of the \code{variable} column from the data frame output of \code{\link{s_biserial_cor}}.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param preference.order Character vector, column names of \code{training.df} in an order of selection priority desired by the user. For example, if \code{preference.order = c("bio1", "bio2", "bio3")}, the algorithm will first compute vif for all variables in \code{training.df} not included in \code{preference.order}, and remove on each step the variable with a higher vif. Then, vif is computed iteratively on the variables in \code{preference.order}, but removing always the variable with the lowest priority (instead of the variable with the higher vif). Finally, all variables resulting from both vif analyses are grouped together, and a new vif analysis is performed, removing first the variables not in \code{preference.order}. In summary, this option will try to preserve a set of variables as long as their vif values allow it. This option is incompatible with the argument \code{biserial.cor} (see below).
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables. In fact, the column "variable" of the data frame within \code{biserial.cor} is used as input for the argument \code{preference.order} explained above. This is just a convenient way to set the priority in variable selection according to the output of \code{s_biserial_cor}.
#' @param verbose Boolean, defaults to TRUE. Triggers messages describing what variables are being removed.
#'
#' @return An object of the class \code{s_vif_auto}. It is a list with two slots: "df" and "vars". The former contains a dataframe with the VIF values of the selected variables, while the latter contains the names of the selected variables.
#'
#' @examples
#' \dontrun{
#'
#' data(virtual.species.training)
#'
#' #1. only training.df and omit.cols are provided
#' #variables with max vif are removed on each step
#'
#' vif.auto.out <- auto_vif(
#'   training.df = virtual.species.training
#' )
#'
#'
#' #2. biserial.cor is provided
#' #variables are processed according to the
#' #priority established by s_biserial_cor()
#'
#' biserial.cor <- s_biserial_cor(
#'   training.df = virtual.species.training,
#'   response.col = "presence",
#'   omit.cols = c("x", "y"),
#'   plot = FALSE
#' )
#'
#' vif.auto.out <- auto_vif(
#'   training.df = virtual.species.training,
#'   biserial.cor = biserial.cor
#' )
#'
#' #3, preference.order is provided
#' #variables in preference.order are selected by preference
#' #the other variables are selected by removing those with max vif
#'
#' vif.auto.out <- auto_vif(
#'   training.df = virtual.species.training,
#'   preference.order = c("bio1", "bio5", "bio6", "bio12")
#' )
#'
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[HH]{vif}} is authored by Richard M. Heiberger <rmh@temple.edu>.
#' @references Heiberger, Richard M. and Holland, Burt (2004). Statistical Analysis and Data Display: An Intermediate Course with Examples in S-Plus, R, and SAS. Springer Texts in Statistics. Springer. ISBN 0-387-40270-5.
#' @export
auto_vif <- function(
  training.df,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  preference.order = NULL,
  biserial.cor = NULL,
  verbose = TRUE
){
  
  #dropping omit.cols
  if(sum(omit.cols %in% colnames(training.df)) == length(omit.cols)){
    training.df <-
      training.df %>%
      dplyr::select(-tidyselect::all_of(omit.cols))
  }
  
  #selecting select.cols
  if(is.null(select.cols) == FALSE){
    if(sum(select.cols %in% colnames(training.df)) == length(select.cols)){
      training.df <-
        training.df %>%
        dplyr::select(tidyselect::all_of(select.cols))
    }
  }
  
  #getting numeric columns only and removing cases with NA
  training.df <-
    training.df[, unlist(lapply(training.df, is.numeric))] %>%
    na.omit()
  
  #preparing preference order if provided
  if (is.null(preference.order) == FALSE){
    preference.order <- preference.order[preference.order %in% colnames(training.df)]
  }
  
  #message
  if(verbose == TRUE){cat("Removed variables: ")}
  
  #IF biserial.cor IS NOT PROVIDED
  if(is.null(biserial.cor) == TRUE){
    
    #AND preference.order IS NOT PROVIDED
    if(is.null(preference.order) == TRUE){
      
      #OPTION 3: SELECT BY MAX VIF
      output.list <- .select_by_max_vif(
        training.df = training.df,
        verbose = verbose
      )
      
    } else {
      
      #OPTION 2: preference.order IS PROVIDED
      
      #selecting by preference
      output.list.by.preference <- .select_by_preference(
        preference.order = preference.order,
        training.df = training.df,
        verbose = verbose
      )
      
      #selecting by max vif (variables not in preference.order)
      output.list.by.max.vif <- .select_by_max_vif(
        training.df = training.df[, !(colnames(training.df) %in% preference.order)],
        verbose = verbose
      )
      
      #merging selected.vars
      selected.vars <- c(
        output.list.by.preference$vars,
        output.list.by.max.vif$vars
      )
      
      #vif by preference again
      output.list <- .select_by_preference(
        preference.order = selected.vars,
        training.df = training.df,
        verbose = verbose
      )
      
    }
    
  } else {
    
    if(inherits(biserial.cor, "s_biserial_cor") == TRUE){
      
      #option 1: computing vif by preference
      output.list <- .select_by_preference(
        preference.order = biserial.cor$df$variable,
        training.df = training.df,
        verbose = verbose
      )
      
    } else {
      
      if(verbose == TRUE){
        
        stop("Argument biserial cor is not an s_biserial_cor object.")
        
      }
      
    }
    
  }
  
  
  #message
  if(verbose == TRUE){cat("Done! \n")}
  return(output.list)
  
}



#' @export
.vif_to_df <- function(x){
  
  #turns vif output into tidy df
  df <-
    data.frame(
      diag(Matrix::solve(cor(x))),
      stringsAsFactors = FALSE
    ) %>%
    dplyr::rename(vif = 1) %>%
    tibble::rownames_to_column(var = "variable") %>%
    dplyr::arrange(dplyr::desc(vif))
  
  return(df)
}


#' @export
.select_by_max_vif <- function(training.df, verbose){
  
  #initializing selected vars
  selected.vars <- colnames(training.df)
  
  #computes vif
  repeat {
    
    #computes vif
    vif.df <- .vif_to_df(x = training.df[, selected.vars])
    
    #selects variables with vif lower than 5
    var.to.remove <-
      vif.df %>%
      dplyr::filter(vif > 2.5) %>%
      dplyr::filter(vif == max(vif)) %>%
      dplyr::slice(1) %>%
      dplyr::select(variable) %>%
      as.character()
    
    #if the first row contains a vif higher than 5
    if(var.to.remove != "character(0)"){
      
      #updates select.cols
      if(verbose == TRUE){cat(paste(var.to.remove, ", ", sep = ""))}
      selected.vars <- selected.vars[selected.vars != var.to.remove]
      
      #stops if there are less than 3 vars left
      if(length(selected.vars) == 1){
        break
      }
      
    } else {
      break
    } #end of "if(var.to.remove != "character(0)")"
    
  } #end of repeat
  
  #final vif.df
  vif.df <- .vif_to_df(x = training.df[, selected.vars])
  
  #output list
  output.list <- list()
  output.list$df <- vif.df
  output.list$vars <- selected.vars
  class(output.list) <- c("list", "s_vif_auto")
  return(output.list)
  
}


#' @export
.select_by_preference <- function(preference.order, training.df, verbose){
  
  #subsets to the variables already available in training.df
  preference.order <- preference.order[preference.order %in% colnames(training.df)]
  
  #initiating selected vars
  selected.vars <- preference.order[1]
  
  #iterates through preference order
  for(i in 2:length(preference.order)){
    
    #new.var
    new.var <- preference.order[i]
    
    #computes vif
    vif.df <- .vif_to_df(x = training.df[, c(selected.vars, new.var)])
    
    #if vif of new.var lower than 5, keep it
    if(max(vif.df$vif) <= 2.5){
      
      selected.vars <- c(selected.vars, new.var)
      
    } else {
      
      #message
      if(verbose == TRUE){cat(paste(new.var, ", ", sep = ""))}
      next
      
    }
    
  }
  
  #final vif.df
  vif.df <- .vif_to_df(x = training.df[, selected.vars])
  
  #output list
  output.list <- list()
  output.list$df <- vif.df[, c("variable", "vif")]
  output.list$vars <- selected.vars
  class(output.list) <- c("list", "s_vif_auto")
  return(output.list)
  
}