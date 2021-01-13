add_pca_factors_optimized <- function(
  pca.factors.df,
  pca.factors.rank,
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){
  
  #initializing data for loop
  rank.pca.factors.i <- pca.factors.rank
  pca.factors.candidates.i <- pca.factors.rank$rank
  
  #copy of data
  data.i <- data
  predictor.variable.names.i <- predictor.variable.names
  
  #vectors to build optimization.df
  optimization.index <- vector()
  optimization.pca.factor.name <- vector()
  optimization.moran.i <- vector()
  optimization.r.squared <- vector()
  optimization.sum <- vector()
  i <- 0
  
  #iterating
  while(length(pca.factors.candidates.i) > 0){
    
    i <- i + 1
    
    #subset pca.factors
    last.pca.factor.name <- colnames(pca.factors.i)[ncol(pca.factors.i)]
    pca.factors.i <- pca.factors[, pca.factors.candidates.i]
    
    #add the first factor to data
    data.i <- data.frame(
      data.i,
      pca.factors[, pca.factors.candidates.i[1]]
    )
    colnames(data.i)[ncol(data.i)] <- pca.factors.candidates.i[1]
    

    #remove used column
    if(!is.null(ncol(pca.factors.i))){
      pca.factors.i[, pca.factors.candidates.i[1]] <- NULL
    } else {
      break
    }
    
    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names.i, 
      pca.factors.candidates.i[1]
    )
    
    #reference moran I
    reference.moran.i <- rank.pca.factors.i$selection.criteria[rank.pca.factors.i$selection.criteria$pca.factor.name == pca.factors.candidates.i[1], "model.moran.i"]
    
    #rank pca factors
    rank.pca.factors.i <- rank_pca_factors(
      pca.factors.df = pca.factors.i,
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      reference.moran.i = reference.moran.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port,
      vif.filtering = FALSE
    )
    
    #redo pca.factors.candidates.i
    pca.factors.candidates.i <- rank.pca.factors.i$rank
    
    #fixing NA in last pca.factor.candidate
    if(is.na(pca.factors.candidates.i[1])){
      pca.factors.candidates.i[1] <- last.pca.factor.name
    }
    
    #gathering data for optimization df.
    optimization.index[i] <- i
    optimization.pca.factor.name[i] <- pca.factors.candidates.i[1]
    optimization.moran.i[i] <- rank.pca.factors.i$selection.criteria[1, "model.moran.i"]
    optimization.r.squared[i] <- rank.pca.factors.i$selection.criteria[1, "model.r.squared"]
    optimization.sum <- (1 - optimization.moran.i[i]) + optimization.r.squared[i]
    
  }
  
  optimization.df <- data.frame(
    pca.factor.index = optimization.index,
    pca.factor.name = optimization.pca.factor.name,
    moran.i = optimization.moran.i,
    r.squared = optimization.r.squared,
    sum = (1 - optimization.moran.i) + optimization.r.squared
  ) %>% 
    dplyr::arrange(desc(sum))
  
  #get index pca factor with optimized r-squared and moran.i
  optimized.index <- optimization.df[1, "pca.factor.index"]
  
  #prepare vector with best factor names
  best.pca.factors <- optimization.df[optimization.df$pca.factor.index %in% 1:optimized.index, "pca.factor.name"]
  
  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.pca.factors <- best.pca.factors
  
  #plot
  x11()
  par(mfrow = c(2, 1))
  par(mar = c(1.5,4,2,2))
  plot(
    optimization.df$pca.factor.index,
    optimization.df$r.squared,
    xlab = "",
    ylab = "R-squared",
    xaxt = 'n'
  )
  par(mar = c(4,4,0,2))
  plot(
    optimization.df$pca.factor.index, 
    optimization.df$moran.i,
    xlab = "PCA factors added",
    ylab = "Moran's I"
  )
  
  #return output
  out.list
  
}


add_pca_factors_ordered <- function(
  pca.factors.df,
  pca.factors.rank,
  data,
  dependent.variable.name,
  predictor.variable.names,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000
){
  
  #getting pca.factors.order
  pca.factors.order <- pca.factors.rank$rank
  
  #importing functions from the global environment
  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)
  
  #preparing cluster for stand alone machine
  if(is.null(cluster.ips) == TRUE){
    
    #number of available cores
    n.cores <- parallel::detectCores() - 1
    if(.Platform$OS.type == "windows" | !is.null(cluster.ips)){
      temp.cluster <- parallel::makeCluster(
        n.cores, 
        type = "PSOCK"
      )
    } else {
      temp.cluster <- parallel::makeCluster(
        n.cores, 
        type = "FORK"
      )
    }
    
    #preparing beowulf cluster  
  } else {
    
    #preparing the cluster specification
    cluster.spec <- cluster_spec(
      ips = cluster.ips,
      cores = cluster.cores,
      user = cluster.user
    )
    
    #setting parallel port
    Sys.setenv(R_PARALLEL_PORT = cluster.port)
    
    #cluster setup
    temp.cluster <- parallel::makeCluster(
      master = cluster.ips[1], 
      spec = cluster.spec,
      port = Sys.getenv("R_PARALLEL_PORT"),
      outfile = "",
      homogeneous = TRUE
    )
    
  }
  print(temp.cluster)
  doParallel::registerDoParallel(cl = temp.cluster)
  
  optimization.df <- foreach::foreach(
    pca.factors.i = 1:length(pca.factors.order),
    .combine = "rbind",
    .packages = c(
      "ranger", 
      "magrittr", 
      "quantable"
    ),
    .export = c(
      "root_mean_squared_error", 
      "rescale_vector",
      "multiscale_moran",
      "moran"
    )
  ) %dopar% {
    
    #pca factor names
    pca.factor.selected.names.i <- pca.factors.order[1:pca.factors.i]
    
    #add pca factor to training data
    data.i <- data.frame(
      data,
      pca.factors.df[, pca.factor.selected.names.i]
    )
    colnames(data.i)[(ncol(data)+1):ncol(data.i)] <- pca.factor.selected.names.i
    
    #new predictor.variable.names
    predictor.variable.names.i <- c(
      predictor.variable.names, 
      pca.factor.selected.names.i
    )
    
    #fitting model i
    m.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      seed = pca.factors.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )
    
    #output.df
    out.df <- data.frame(
      pca.factor.index = pca.factors.i,
      moran.i = m.i$spatial.correlation.residuals$max.moran,
      r.squared = m.i$r.squared,
      sum = (1 - m.i$spatial.correlation.residuals$max.moran) + m.i$r.squared
    )
    
    return(out.df)
    
  }
  
  #arranging optimization df
  optimization.df <- dplyr::arrange(
    optimization.df,
    desc(sum)
  )
  
  #get index pca factor with optimized r-squared and moran.i
  optimized.index <- optimization.df[1, "pca.factor.index"]
  
  #prepare vector with best factor names
  best.pca.factors <- pca.factors.order[1:optimized.index]
  
  #output list
  out.list <- list()
  out.list$optimization <- optimization.df
  out.list$best.pca.factors <- best.pca.factors
  
  #plot
  x11()
  par(mfrow = c(2, 1))
  par(mar = c(1.5,4,2,2))
  plot(
    optimization.df$pca.factor.index,
    optimization.df$r.squared,
    xlab = "",
    ylab = "R-squared",
    xaxt = 'n'
  )
  par(mar = c(4,4,0,2))
  plot(
    optimization.df$pca.factor.index, 
    optimization.df$moran.i,
    xlab = "PCA factors added",
    ylab = "Moran's I"
  )
  
  #return output
  out.list
  
}


# slope <- function(x, y){
#     
#     mx  = mean(x)
#     my  = mean(y)
#     sxx = sum((x - mx)*(x - mx))
#     sxy = sum((x - mx)*(y - my))
#     syy = sum((y - my)*(y - my))
#     slope  = round(sxy/sxx, 3)
#     
#     slope
#   
# }


rank_pca_factors <- function(
  pca.factors.df,
  data,
  dependent.variable.name,
  predictor.variable.names,
  reference.moran.i,
  distance.matrix,
  distance.thresholds,
  ranger.arguments,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000,
  vif.filtering = TRUE
){
  
  #importing functions from the global environment
  rf <- get("rf", envir = .GlobalEnv)
  root_mean_squared_error <- get("root_mean_squared_error", envir = .GlobalEnv)
  rescale_vector <- get("rescale_vector", envir = .GlobalEnv)
  multiscale_moran <- get("multiscale_moran", envir = .GlobalEnv)
  moran <- get("moran", envir = .GlobalEnv)
  auto_vif <- get("auto_vif", envir = .GlobalEnv)
  .select_by_max_vif <- get(".select_by_max_vif", envir = .GlobalEnv)
  .select_by_preference <- get(".select_by_preference", envir = .GlobalEnv)
  
    #preparing cluster for stand alone machine
    if(is.null(cluster.ips) == TRUE){
      
      #number of available cores
      n.cores <- parallel::detectCores() - 1
      if(.Platform$OS.type == "windows" | !is.null(cluster.ips)){
        temp.cluster <- parallel::makeCluster(
          n.cores, 
          type = "PSOCK"
        )
      } else {
        temp.cluster <- parallel::makeCluster(
          n.cores, 
          type = "FORK"
        )
      }
      
      #preparing beowulf cluster  
    } else {
      
      #preparing the cluster specification
      cluster.spec <- cluster_spec(
        ips = cluster.ips,
        cores = cluster.cores,
        user = cluster.user
      )
      
      #setting parallel port
      Sys.setenv(R_PARALLEL_PORT = cluster.port)
      
      #cluster setup
      temp.cluster <- parallel::makeCluster(
        master = cluster.ips[1], 
        spec = cluster.spec,
        port = Sys.getenv("R_PARALLEL_PORT"),
        outfile = "",
        homogeneous = TRUE
      )
      
    }
  doParallel::registerDoParallel(cl = temp.cluster)
  
  #3.2.3 PREPARING PARALLELIZED LOOP TO ITERATE THROUGH distance.matrix.pca
  pca.factor.order <- foreach::foreach(
    pca.factors.i = 1:ncol(pca.factors.df),
    .combine = "rbind",
    .packages = c(
      "ranger", 
      "magrittr", 
      "quantable"
      ),
    .export = c(
      "root_mean_squared_error", 
      "rescale_vector",
      "multiscale_moran",
      "auto_vif",
      ".select_by_max_vif",
      ".select_by_preference"
      )
  ) %dopar% {
    
    #3.2.3.1 preparing data
    
    #pca factor name
    pca.factor.name.i <- names(pca.factors.df)[pca.factors.i]
    
    #training data
    data.i <- data.frame(
      data,
      pca.factors.df[, pca.factors.i]
    )
    colnames(data.i)[ncol(data.i)] <- pca.factor.name.i
    
    #new predictor.variable.names
    predictor.variable.names.i <- c(predictor.variable.names, pca.factor.name.i)
    
    #fitting model I
    m.i <- rf(
      data = data.i,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names.i,
      seed = pca.factors.i,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      scaled.importance = FALSE,
      ranger.arguments = ranger.arguments
    )
    
    #loop output
    out.i <- data.frame(
      pca.factor.name = pca.factor.name.i,
      pca.factor.moran.i = moran(
        x = pca.factors.df[, pca.factors.i],
        distance.matrix = distance.matrix,
        distance.threshold = distance.thresholds[1]
        )$moran.i,
      model.moran.i = m.i$spatial.correlation.residuals$max.moran,
      model.r.squared = m.i$r.squared,
      effect.on.moran.i = reference.moran.i - m.i$spatial.correlation.residuals$max.moran,
      interpretation = m.i$spatial.correlation.residuals$df[which.max(m.i$spatial.correlation.residuals$df$moran.i), "interpretation"]
    )
    
    #returning output
    return(out.i)
    
  } #end of parallelized loop
  
  #stopping cluster
  parallel::stopCluster(cl = temp.cluster)
  
  #order dataframe
  pca.factor.order <- pca.factor.order %>% 
    dplyr::arrange(dplyr::desc(effect.on.moran.i))
  
  #selected pca.factors
  pca.factor.order.selected <- dplyr::filter(
      pca.factor.order,
      effect.on.moran.i > 0,
      pca.factor.moran.i > 0
      )
  
  #apply vif filtering if requested
  if(vif.filtering == TRUE){
      
    pca.factors.vif <- auto_vif(
      x = pca.factors.df[, pca.factor.order.selected$pca.factor.name],
      preference.order = pca.factor.order.selected$pca.factor.name,
      verbose = FALSE
    )
  
    #subset pca.factor.order
    pca.factor.order.selected <- pca.factor.order.selected[pca.factor.order.selected$pca.factor.name %in% pca.factors.vif$vars, ]
  
  }
  
  #returns one pca factor if it is enough to reduce spatial correlation of the residuals
  if(sum("No spatial correlation" %in% rank.pca.factors$interpretation) > 0){
    
    pca.factors.selected <- pca.factor.order.selected %>% 
      dplyr::filter(interpretation == "No spatial correlation") %>% 
      dplyr::slice(1) %>% 
      dplyr::select(pca.factor.name) %>% 
      .$pca.factor.name
    
  #returns the best candidates
  } else {
    
    pca.factors.selected <- pca.factor.order.selected$pca.factor.name
    
  }
  
  #return output
  out.list <- list()
  out.list$selection.criteria <- pca.factor.order
  out.list$rank <- pca.factors.selected
  
  #returning output list
  out.list
  
}


#generates a cluster specification for the "spec" argument of the parallel::makeCluster() function
#ips: vector of computer ips within the local network: c('10.42.0.1', '10.42.0.34', '10.42.0.104')
#cores: vector of integers with the number of cores available on each computer, in the same order as ips: cores = c(7, 4, 4)
#user: character, name of the user in the different computers. Only one user name allowed.
#returns a list that can be used directly as input for the "spec" argument of makeCluster().
cluster_spec <- function(
  ips,
  cores,
  user
){
  
  #creating initial list
  spec <- list()
  
  for(i in 1:length(ips)){
    spec[[i]] <- list()
    spec[[i]]$host <- ips[i]
    spec[[i]]$user <- user
    spec[[i]]$ncore <- cores[i]
  }
  
  #generating nodes from the list of machines
  spec <- lapply(
    spec, 
    function(spec.i) rep(
      list(
        list(
          host = spec.i$host, 
          user = spec.i$user)
      ), 
      spec.i$ncore
    )
  )
  
  #formating into a list of lists
  spec <- unlist(
    spec, 
    recursive = FALSE
  )
  
  return(spec)
  
}


pca_distance_matrix <- function(
  distance.matrix = NULL,
  distance.threshold = NULL
  ){
  
  #if distance.threshold is null, 0
  if(is.null(distance.threshold)){
    distance.threshold <- 0
  }
  
  #list to store pca factors
  pca.factors.list <- list()
  
  #iterating through distance thresholds
  for(distance.threshold.i in distance.threshold){
    
    #copy distance matrix
    distance.matrix.i <- distance.matrix
    
    #applying threshold to distance matrix
    distance.matrix.i[distance.matrix.i <= distance.threshold.i] <- distance.threshold.i
    
    #computing pca factors
    pca.factors.list[[as.character(distance.threshold.i)]] <- pca(
      distance.matrix = distance.matrix.i,
      colnames.prefix = paste0(
        "spatial_structure_", 
        distance.threshold.i
        ),
      plot = FALSE
    )
    
  }
  
  #removing names
  names(pca.factors.list) <- NULL
  
  #to data frame
  pca.factors <- do.call("cbind", pca.factors.list)
  
  #returning output
  pca.factors
  
}


moran <- function(
  x = NULL,
  distance.matrix = NULL,
  distance.threshold = NULL #cases below this distance are considered neighbors
){
  
  #thresholding distance matrix 
  if(!is.null(distance.threshold)){
    # distance.matrix[distance.matrix > distance.threshold] <- distance.threshold
    distance.matrix[distance.matrix < distance.threshold] <- 0
  }
  
  #computing weights
  weight <- 1/distance.matrix
  weight[is.infinite(weight)] <- 1
  diag(weight) <- 0
  
  #normalizing weights
  weight.rowsums <- rowSums(weight)
  weight.rowsums[weight.rowsums == 0] <- 1
  weight <- weight/weight.rowsums
  
  #computing expected Moran I
  n <- length(x)
  expected.moran <- round(-1/(n - 1), 4)
  
  #computing observed Moran I
  s <- sum(weight)
  m <- mean(x)
  y <- x - m #centering x
  cv <- sum(weight * y %o% y)
  v <- sum(y^2)
  observed.moran <- (n/s) * (cv/v)
  i.max <- (n/s) * (sd(rowSums(weight) * y)/sqrt(v/(n - 1)))
  observed.moran <- round(observed.moran/i.max, 4)
  
  #computing p-value
  s1 <- 0.5 * sum((weight + t(weight))^2)
  s2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
  s.sq <- s^2
  k <- (sum(y^4)/n) / (v/n)^2
  expected.standard.deviation <- sqrt(
    (n*((n^2 - 3*n + 3)*s1 - n*s2 + 3*s.sq) - 
       k*(n*(n - 1)*s1 - 2*n*s2 + 6*s.sq)) / 
      ((n - 1)*(n - 2)*(n - 3)*s.sq) - 1/((n - 1)^2)
  )
  p.value <- pnorm(observed.moran, mean = expected.moran, sd = expected.standard.deviation)
  p.value <- if (observed.moran <= expected.moran) 2*p.value else 2*(1 - p.value)
  p.value <- round(p.value, 4)
  
  #adding interpretation
  if(observed.moran > 0 & p.value <= 0.05){
    interpretation <- "Positive spatial correlation"
  }
  if(observed.moran < 0 & p.value <= 0.05){
    interpretation <- "Negative spatial correlation"
  }
  if(p.value > 0.05){
    interpretation <- "No spatial correlation"
  }
  
  #preparing output
  out <- data.frame(
    moran.i = observed.moran,
    p.value = p.value,
    interpretation = interpretation
  )
  return(out)
  
}


multiscale_moran <- function(
  x,
  distance.matrix,
  distance.thresholds = NULL,
  plot = TRUE
){
  
  #creating distance thresholds
  if(is.null(distance.thresholds) == TRUE){
    distance.thresholds <- floor(
      seq(
        0, 
        max(distance.matrix
            ), 
        length.out = 4
        )
      )
  }
  
  #create output dataframe
  out.df <- data.frame(
    distance.threshold = distance.thresholds,
    moran.i = NA,
    moran.p = NA,
    interpretation = NA
  )
  
  #iterating over out.df
  for(i in 1:nrow(out.df)){
    
    #compute Moran's I
    moran.out <- moran(
      x = x, 
      distance.matrix = distance.matrix,
      distance.threshold = out.df[i, "distance.threshold"]
      )
    
    out.df[i, "moran.i"] <- moran.out["moran.i"]
    out.df[i, "moran.p"] <- moran.out["p.value"]
    out.df[i, "interpretation"] <- moran.out["interpretation"]
    
  }
  
  p <- ggplot2::ggplot(data = out.df) + 
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i
    ) + 
    ggplot2::geom_hline(
      yintercept = 0, 
      col = "red4", size = 0.5
      ) +
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::xlab("Distance threshold") + 
    ggplot2::ylab("Moran's I") + 
    ggplot2::ggtitle("Multiscale Moran's I")
  
  #getting scale of max moran
  distance.threshold.max.moran <- out.df[which.max(out.df$moran.i), "distance.threshold"]
  
  #preparing output list
  out.list <- list()
  out.list$df <- out.df
  out.list$plot <- p
  out.list$max.moran <- max(out.df$moran.i)
  out.list$max.moran.distance.threshold <- distance.threshold.max.moran
  
  if(plot == TRUE){print(p)}

  return(out.list)
  
}

pca <- function(
  distance.matrix = NULL, 
  colnames.prefix = "pca_factor", 
  plot = TRUE
  ){
  
  #removing columns with zero variance
  distance.matrix <- distance.matrix[ , which(apply(distance.matrix, 2, var) != 0)]
  
  #computing pca of distance matrix
  distance.matrix.pca <- prcomp(distance.matrix, scale. = TRUE)
  
  if(plot == TRUE){
    p <- factoextra::fviz_eig(distance.matrix.pca)
    print(p)
  }

  #getting pca factors
  distance.matrix.pca.factors <- as.data.frame(distance.matrix.pca$x)
  colnames(distance.matrix.pca.factors) <- paste(colnames.prefix, 1:ncol(distance.matrix.pca.factors), sep = "_")
  
  #robust scaling
  # distance.matrix.pca.factors <- quantable::robustscale(
  #   data = distance.matrix.pca.factors,
  #   dim = 2,
  #   center = TRUE,
  #   scale = TRUE,
  #   preserveScale = FALSE
  # )$data
  
  #returning output
  distance.matrix.pca.factors
}



statistical_mode <- function(x) {
  x.unique <- unique(x)
  x.unique[which.max(tabulate(match(x, x.unique)))]
  x.unique <- unlist(x.unique[1])
  return(x.unique)
}

#modified from https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
root_mean_squared_error <- function(o, p, type = "iq") {
  
  squared_sums <- sum((o - p)^2)
  mse <- squared_sums/length(o)
  rmse <- round(sqrt(mse), 4)
  
  if(is.null(type)){
    return(rmse)
    
  } else {
    
    if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
    
    #computing nrmse
    nrmse.sd <- rmse/sd(o)
    nrmse.mean <- rmse/mean(o)
    nrmse.maxmin <- rmse/ (max(o) - min(o))
    nrmse.iq <- rmse/ (quantile(o, 0.75) - quantile(o, 0.25))
    
    #building vector with nrmse values
    nrmse <- c(nrmse.iq, nrmse.maxmin, nrmse.mean, nrmse.sd)
    names(nrmse) <- c("iq", "maxmin", "mean", "sd")
    
    #removing infinites
    nrmse[!is.finite(nrmse)] <- NA
    nrmse <- na.omit(nrmse)
    
    #if the selected type has a valid value
    if(type %in% names(nrmse)){
      nrmse <- nrmse[names(nrmse) == type]
    } else {
      #otherwise return the next one
      nrmse <- nrmse[1]
    }
    
    return(nrmse)
  }
  
}

repeat_rf <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  iterations = 10,
  white.noise = FALSE,
  autocorrelated.noise = FALSE,
  trees.per.variable = 100,
  ranger.arguments = list(
    formula = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
){
  
  #initializes local.importance
  if(is.null(ranger.arguments$local.importance)){
    local.importance <- FALSE
  } else {
    local.importance <- ranger.arguments$local.importance
  }
  
  #lists to store results of the iterations
  predictions <- list()
  if(local.importance == TRUE){variable.importance.local <- list()}
  variable.importance <- list()
  prediction.error <- list()
  r.squared <- list()
  pseudo.r.squared <- list()
  rmse <- list()
  nrmse <- list()
  residuals <- list()
  spatial.correlation.residuals <- list()
  
  #applying robust scaling to the data
  data.scaled <- quantable::robustscale(
    data = data[, c(
      dependent.variable.name,
      predictor.variable.names
    )],
    dim = 2,
    center = TRUE,
    scale = TRUE,
    preserveScale = FALSE
  )$data
  
  #if scaling fails, use regular scaling
  if(sum(is.nan(data.scaled[, 1])) > 0 | sum(is.infinite(data.scaled[, 1])) > 0){
    data.scaled <- as.data.frame(scale(data))
  }
  
  #iterations
  for(i in 1:iterations){
    
    #setting seed
    set.seed(i)
    
    #fitting model
    m.i <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      white.noise = white.noise,
      autocorrelated.noise = autocorrelated.noise,
      trees.per.variable = trees.per.variable,
      seed = i,
      ranger.arguments = ranger.arguments
    )
    
    m.i.scaled <- rf(
      data = data.scaled,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      distance.matrix = NULL,
      white.noise = white.noise,
      autocorrelated.noise = autocorrelated.noise,
      trees.per.variable = trees.per.variable,
      seed = i,
      ranger.arguments = ranger.arguments
    )
    
    #gathering results
    predictions[[i]] <- m.i$predictions
    if(local.importance == TRUE){
      variable.importance.local[[i]] <- m.i.scaled$variable.importance.local
      }
    variable.importance[[i]] <- m.i.scaled$variable.importance$vector
    prediction.error[[i]] <- m.i$prediction.error
    r.squared[[i]] <- m.i$r.squared
    pseudo.r.squared[[i]] <- m.i$pseudo.r.squared
    rmse[[i]] <- m.i$rmse
    nrmse[[i]] <- m.i$nrmse
    residuals[[i]] <- m.i$residuals
    spatial.correlation.residuals[[i]] <- m.i$spatial.correlation.residuals
      
  }#end of iterations
  
  #fitting complete model to allow plotting partial dependence curves
  m.curves <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    seed = i,
    white.noise = FALSE,
    autocorrelated.noise = FALSE,
    trees.per.variable = trees.per.variable,
    ranger.arguments = ranger.arguments
  )
  
  #names of iterations columns
  iteration.columns <- paste("iteration", 1:iterations, sep = "_")
  
  #gathering predictions
  predictions.by.iteration <- as.data.frame(do.call("cbind", predictions))
  colnames(predictions.by.iteration) <- iteration.columns
  predictions.mean <- data.frame(
    prediction_mean = rowMeans(predictions.by.iteration),
    standard_deviation = apply(predictions.by.iteration, 1, sd)
  )
  m.curves$predictions <- NULL #to avoid warning
  m.curves$predictions$df.wide <- predictions.by.iteration
  m.curves$predictions$df <- predictions.mean
  
  #gathering variable.importance.local
  if(local.importance == TRUE){
    m.curves$variable.importance.local <- apply(simplify2array(variable.importance.local), 1:2, mean)
  }
  
  #gathering variable.importance
  m.curves$variable.importance <- NULL
  
  #wide format
  variable.importance.df.wide <- as.data.frame(do.call("cbind", variable.importance))
  colnames(variable.importance.df.wide) <- iteration.columns
  variable.importance.df.wide <- data.frame(
    variable = rownames(variable.importance.df.wide),
    variable.importance.df.wide,
    row.names = NULL
  )
  
  #mean
  variable.importance.mean <- data.frame(
    variable = variable.importance.df.wide$variable,
    importance = rowMeans(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)]),
    standard_deviation = apply(variable.importance.df.wide[, tidyselect::all_of(iteration.columns)], 1, sd),
    row.names = NULL
  ) %>% 
    dplyr::arrange(desc(importance)) %>% 
    as.data.frame()

  variable.importance.df.long <- tidyr::pivot_longer(
    data = variable.importance.df.wide,
    cols = tidyselect::all_of(iteration.columns),
    names_to = "iteration",
    values_to = "importance"
  ) %>% 
    as.data.frame()
  
  variable.importance.plot <- ggplot2::ggplot(data = variable.importance.df.long) + 
    ggplot2::aes(y = reorder(
      variable, 
      importance,
      FUN = max), 
      x = importance
      ) + 
    ggplot2::geom_boxplot() + 
    ggplot2::ylab("") + 
    ggplot2::xlab("Importance score") + 
    ggplot2::ggtitle(paste("Response variable: ", dependent.variable.name, sep = ""))
  
  m.curves$variable.importance <- list()
  m.curves$variable.importance$df <- variable.importance.mean
  m.curves$variable.importance$df.wide <- variable.importance.df.wide
  m.curves$variable.importance$df.long <- variable.importance.df.long
  m.curves$variable.importance$plot <- variable.importance.plot

  
  #gathering prediction.error
  m.curves$prediction.error <- unlist(prediction.error)
  
  #gathering r.squared
  m.curves$r.squared <- unlist(r.squared)
  
  #gathering pseudo R squared
  m.curves$pseudo.r.squared <- unlist(pseudo.r.squared)
  
  #gathering rmse
  m.curves$rmse <- unlist(rmse)
  
  #gathering nrmse
  m.curves$nrmse <- unlist(nrmse)
  names(m.curves$nrmse) <- NULL
  
  #gathering spatial.correlation.residuals
  spatial.correlation.residuals.by.iteration <- do.call("rbind", lapply(spatial.correlation.residuals, "[[", 1)) %>% 
    dplyr::arrange(distance.threshold)
  spatial.correlation.residuals.by.iteration$iteration <- rep(1:iterations, length(unique(spatial.correlation.residuals.by.iteration$distance.threshold)))
  
  spatial.correlation.residuals.mean <- spatial.correlation.residuals.by.iteration %>% 
    dplyr::group_by(distance.threshold) %>% 
    dplyr::summarise(
      moran.i = mean(moran.i),
      p.value = mean(moran.p),
      interpretation = statistical_mode(interpretation)
    ) %>% 
    as.data.frame()
  
  m.curves$spatial.correlation.residuals <- list()
  
  m.curves$spatial.correlation.residuals$df <- spatial.correlation.residuals.mean
  
  m.curves$spatial.correlation.residuals$df.long <- spatial.correlation.residuals.by.iteration
  
  m.curves$spatial.correlation.residuals$plot <- ggplot2::ggplot(data = spatial.correlation.residuals.by.iteration) + 
    ggplot2::aes(
      x = distance.threshold,
      y = moran.i,
      group = iteration
    ) + 
    ggplot2::geom_point(alpha = 0.5) + 
    ggplot2::geom_line(alpha = 0.5) + 
    ggplot2::geom_hline(yintercept = 0, color = "red4") +
    ggplot2::xlab("Distance threshold") + 
    ggplot2::ylab("Moran's I") + 
    ggplot2::ggtitle("Multiscale Moran's I")
  
  m.curves$spatial.correlation.residuals$max.moran <-  mean(unlist(lapply(spatial.correlation.residuals, "[[", 3)))
  
  m.curves$spatial.correlation.residuals$max.moran.distance.threshold <- statistical_mode(unlist(lapply(spatial.correlation.residuals, "[[", 4)))

  #gathering residuals
  residuals <- as.data.frame(do.call("cbind", residuals))
  colnames(residuals) <- iteration.columns
  
  residuals.mean <- data.frame(
    residuals_mean = rowMeans(residuals),
    standard_deviation = apply(residuals, 1, sd),
    row.names = NULL
  )
  
  m.curves$residuals <- NULL
  m.curves$residuals$df <- residuals.mean
  m.curves$residuals$df.long <- residuals
  m.curves$residuals$stats <- summary(residuals.mean$mean)

  #returning results list
  return(m.curves)
}




#function to rescale vectors between given bounds
rescale_vector <- function(x = rnorm(100),
                          new.min = 0,
                          new.max = 100,
                          integer = FALSE){
  
  
  #data extremes
  old.min = min(x)
  old.max = max(x)
  
  
  #SCALING VECTOR
  #----------------------
  
  x = ((x - old.min) / (old.max - old.min)) * (new.max - new.min) + new.min
  
  
  #FORCES VECTOR INTO INTEGER
  #----------------------
  
  if(integer == TRUE){
    x = floor(x)
  }
  
  return(x)
  
}





#rf model with:
#pseudo R-squared (cor(observations, predictions))
#dataframe with variable importance
#Moran's I of the residuals
rf <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  seed = NULL,
  white.noise = FALSE,
  autocorrelated.noise = FALSE,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  trees.per.variable = NULL,
  scaled.importance = TRUE,
  ranger.arguments = list(
    formula = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    seed = NULL,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
){
  
  #subsetting data
  if(!is.null(data)){
    
    if(!is.null(predictor.variable.names) & !is.null(dependent.variable.name)){
      
      predictor.variable.names <- predictor.variable.names[predictor.variable.names %in% colnames(data)]
      data <- data[, c(dependent.variable.name, predictor.variable.names)]
      
    }
    
    #removing NA
    data <- na.omit(data)
    
  }
  
  #default model arguments
  default.ranger.arguments <- list(
    formula = NULL,
    num.trees = 500,
    trees.per.variable = NULL,
    mtry = NULL,
    mtry = NULL,
    importance = "permutation",
    write.forest = TRUE,
    probability = FALSE,
    min.node.size = NULL,
    max.depth = NULL,
    replace = TRUE,
    case.weights = NULL,
    class.weights = NULL,
    splitrule = NULL,
    num.random.splits = 1,
    alpha = 0.5,
    minprop = 0.1,
    split.select.weights = NULL,
    always.split.variables = NULL,
    respect.unordered.factors = NULL,
    scale.permutation.importance = TRUE,
    local.importance = FALSE,
    regularization.factor = 1,
    regularization.usedepth = FALSE,
    keep.inbag = FALSE,
    inbag = NULL,
    holdout = FALSE,
    quantreg = FALSE,
    oob.error = TRUE,
    num.threads = parallel::detectCores() - 1,
    save.memory = FALSE,
    verbose = TRUE,
    seed = NULL,
    classification = NULL,
    x = NULL,
    y = NULL,
    sample.fraction = 1
  )
  list2env(default.ranger.arguments, envir=environment())
  
  #user arguments
  list2env(ranger.arguments, envir=environment())
  
  #setting up seed if available
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  if(!is.null(trees.per.variable)){
    num.trees <- trees.per.variable * (ncol(data) - 1)
  }
  
  #ranger model for r-squared and predictions
  m <- ranger::ranger(
    data = data,
    dependent.variable.name = dependent.variable.name,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    verbose = verbose,
    seed = seed,
    classification = classification,
    x = x,
    y = y
  )
  
  #if scaled.importance is TRUE
  if(scaled.importance == TRUE){
  
    #applying robust scaling to the data
    data.scaled <- quantable::robustscale(
      data = data,
      dim = 2,
      center = TRUE,
      scale = TRUE,
      preserveScale = FALSE
    )$data
    
    #if scaling fails, use regular scaling
    if(sum(is.nan(data.scaled[, 1])) > 0 | sum(is.infinite(data.scaled[, 1])) > 0){
      data.scaled <- as.data.frame(scale(data))
    }
    
    #ranger model for variable importance
    m.scaled <- ranger::ranger(
      data = data.scaled,
      dependent.variable.name = dependent.variable.name,
      num.trees = num.trees,
      mtry = mtry,
      importance = importance,
      write.forest = write.forest,
      probability = probability,
      min.node.size = min.node.size,
      max.depth = max.depth,
      replace = replace,
      sample.fraction = sample.fraction,
      case.weights = case.weights,
      class.weights = class.weights,
      splitrule = splitrule,
      num.random.splits = num.random.splits,
      alpha = alpha,
      minprop = minprop,
      split.select.weights = split.select.weights,
      always.split.variables = always.split.variables,
      respect.unordered.factors = respect.unordered.factors,
      scale.permutation.importance = FALSE,
      local.importance = local.importance,
      regularization.factor = regularization.factor,
      regularization.usedepth = regularization.usedepth,
      keep.inbag = keep.inbag,
      inbag = inbag,
      holdout = holdout,
      quantreg = quantreg,
      oob.error = oob.error,
      num.threads = num.threads,
      save.memory = save.memory,
      verbose = verbose,
      seed = seed,
      classification = classification,
      x = x,
      y = y
    )
  
  } else {
    
    m.scaled <- m
    
  }
  
  #adding model arguments
  m$ranger.arguments <- list(
    dependent.variable.name = dependent.variable.name,
    num.trees = num.trees,
    mtry = mtry,
    importance = importance,
    write.forest = write.forest,
    probability = probability,
    min.node.size = min.node.size,
    max.depth = max.depth,
    replace = replace,
    sample.fraction = sample.fraction,
    case.weights = case.weights,
    class.weights = class.weights,
    splitrule = splitrule,
    num.random.splits = num.random.splits,
    alpha = alpha,
    minprop = minprop,
    split.select.weights = split.select.weights,
    always.split.variables = always.split.variables,
    respect.unordered.factors = respect.unordered.factors,
    scale.permutation.importance = scale.permutation.importance,
    local.importance = local.importance,
    regularization.factor = regularization.factor,
    regularization.usedepth = regularization.usedepth,
    keep.inbag = keep.inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    oob.error = oob.error,
    num.threads = num.threads,
    save.memory = save.memory,
    verbose = verbose,
    seed = seed,
    classification = classification
  )
  
  #importance dataframe
  importance.vector <- m.scaled$variable.importance
  m$variable.importance <- list()
  m$variable.importance$vector <- importance.vector
  m$variable.importance$df <- data.frame(
    variable = names(m.scaled$variable.importance),
    importance = m.scaled$variable.importance
  ) %>%
    tibble::remove_rownames() %>%
    dplyr::arrange(desc(importance))
  
  m$variable.importance$plot <- ggplot2::ggplot(data = m$variable.importance$df) + 
    ggplot2::aes(
      x = importance, 
      y = reorder(
        variable, 
        importance, 
        FUN = max
      )
    ) + 
    ggplot2::geom_point(size = 2) + 
    ggplot2::ylab("")
  
  #getting residuals
  
  #predicted data
  predicted <- m$predictions
  
  #getting observed data
  
  #if data is provided
  if(!is.null(data)){
    
    #the user used a formula
    if(!is.null(formula)){
      
      #observed
      observed <- data[, all.vars(formula)[1]]
      
    }
    
    #user gave dependent.variable.name
    if(!is.null(dependent.variable.name)){
      
      #observed
      observed <- data[, dependent.variable.name]
      
    }
    
  }
  
  if(!is.null(y) & !is.null(x)){
    
    observed = y
    
  }
  
  
  m$pseudo.r.squared <- cor(
    observed,
    predicted
  )
  
  m$rmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    type = NULL
  )
  
  m$nrmse <- root_mean_squared_error(
    o = observed,
    p = predicted,
    type = "iq"
  )
  
  m$residuals <- observed - predicted
  
  
  #compute moran I of residuals if distance.matrix is provided
  if(!is.null(distance.matrix)){
    
    m$spatial.correlation.residuals <- multiscale_moran(
      x = m$residuals,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      plot = FALSE
      )

  }
  
  #replacing variable importance with the scaled one
  m$variable.importance.local <- m.scaled$variable.importance.local
  
  return(m)
  
}


#computes vif of a dataframe
vif <- function(x){
  out <- x %>% 
    na.omit() %>% 
    as.matrix() %>% 
    cor() %>% 
    solve() %>% 
    diag() %>% 
    sort(decreasing = TRUE) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "variable") 
  colnames(out)[2] <- "vif"
  return(out)
}

#' Correlation dendrogram to help reduce multicollinearity in a training dataset.
#'
#' @description Computes the correlation between all pairs of variables in a training dataset and computes a cluster through the expression \code{hclust(as.dist(abs(1 - correlation.matrix)))}. If a \code{\link{s_biserial_cor}} output is provided, the clustering is computed as \code{hclust(as.dist(abs(1 - correlation.matrix)), method = "single")}, and the algorithm selects variables automatically based on the R-squared value obtained by each variable in the biserial correlation analysis.
#'
#' @usage cor_dendrogram(
#'   x,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   max.cor = 0.75,
#'   biserial.cor = NULL,
#'   plot = TRUE,
#'   text.size = 6
#'   )
#'
#'
#' @param x A data frame with a presence column with 1 indicating presence and 0 indicating background, and columns with predictor values.
#' @param select.cols Character vector, names of the columns representing predictors. If \code{NULL}, all numeric variables but \code{presence.column} are considered.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param max.cor Numeric in the interval [0, 1], maximum Pearson correlation of the selected variables. Defaults to 0.75.
#' @param biserial.cor List, output of the function \code{\link{s_biserial_cor}}. Its R-squared scores are used to select variables.
#' @param plot Boolean, prints biserial correlation plot if \code{TRUE}.
#' @param text.size Numeric, size of the dendrogram labels.
#'
#' @return If \code{biserial.cor} is not NULL, a list with two slots named "dendrogram" (a ggplot2 object) and "selected.variables" with the dendrogram and the character vector with the selected variables. Otherwise it only returns the dendrogram, and the users have to select the variables by themselves.
#'
#' @examples
#' \dontrun{
#'data("virtualSpeciesPB")
#'
#'biserial.cor <- s_biserial_cor(
#'  x = virtualSpeciesPB,
#'  omit.cols = c("x", "y")
#')
#'
#'selected.vars <- cor_dendrogram(
#'  x = virtualSpeciesPB,
#'  select.cols = NULL,
#'  omit.cols = c("x", "y", "presence"),
#'  max.cor = 0.75,
#'  biserial.cor = biserial.cor
#')$selected.variables
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>.
#'
#' @export
cor_dendrogram <- function(
  x,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  max.cor = 0.75,
  biserial.cor = NULL,
  plot = TRUE,
  text.size = 4
){
  
  #preparing output list
  output.list <- list()
  
  #dropping omit.cols
  if(sum(omit.cols %in% colnames(x)) == length(omit.cols)){
    x <-
      x %>%
      dplyr::select(-tidyselect::all_of(omit.cols))
  }
  
  #selecting select.cols
  if(is.null(select.cols) == FALSE){
    if(sum(select.cols %in% colnames(x)) == length(select.cols)){
      x <-
        x %>%
        dplyr::select(tidyselect::all_of(select.cols))
    }
  }
  
  #getting numeric columns only and removing cases with NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()
  
  #computes correlation matrix
  cor.matrix <-
    x %>%
    cor() %>%
    as.dist() %>%
    abs()
  
  #if biserial.cor == NULL
  #-------------------------------------
  if(is.null(biserial.cor) == TRUE | inherits(biserial.cor, "s_biserial_cor") == FALSE){
    
    #cluster (converts correlation to distance)
    temp.cluster <- hclust(1 - cor.matrix)
    
    #generates cluster data
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)
    
    #plots cluster
    cluster.plot <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = ggdendro::segment(temp.cluster.data),
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend)
      ) +
      ggplot2::geom_text(
        data = ggdendro::label(temp.cluster.data),
        aes(
          label = label,
          x = x,
          y = 0,
          hjust = 1
        ),
        size = text.size
      ) +
      ggplot2::coord_flip(ylim = c(-0.4, 1)) +
      viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
      ggplot2::theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1,2,1,2), "lines"),
        axis.text.x = element_text(size = text.size * 2),
        legend.position = "bottom",
        legend.key.width = unit(2, "lines")
      ) +
      ggplot2::labs(colour = "R2") +
      ggplot2::geom_hline(
        yintercept = 1 - max.cor,
        col = "red4",
        linetype = "dashed",
        size = 1,
        alpha = 0.5
      ) +
      ggplot2::scale_y_continuous(breaks = c(1 - max.cor, 0, 0.25, 0.5, 0.75, 1)) +
      ggplot2::ylab("1 - correlation")
    
    if(plot == TRUE){
      ggplot2::theme_set(cowplot::theme_cowplot())
      print(cluster.plot)
    }
    
    #prepare output
    selected.variables <- colnames(x)
    
  } else {
    
    #cluster (converts correlation to distance)
    temp.cluster <- hclust(1 - cor.matrix, method = "single")
    
    #gets range of heights of the cluster
    height.range <- round(range(temp.cluster$height), 2)
    
    #gets change step
    height.step <- (max(height.range) - min(height.range))/200
    
    #initial value for observed.max.cor
    observed.max.cor <- 1
    
    #iterator counter
    i <- 0
    
    #iterations to find right height
    while(observed.max.cor > max.cor){
      
      #plus one iteration
      i <- i + 1
      
      #computes height cutoff
      height.cutoff <- min(height.range) + (height.step * i)
      
      #table of groups
      temp.cluster.groups <- data.frame(group = cutree(
        temp.cluster,
        h = height.cutoff
      ))
      temp.cluster.groups$variable <- row.names(temp.cluster.groups)
      temp.cluster.groups <- temp.cluster.groups[
        order(
          temp.cluster.groups$group,
          decreasing = FALSE
        ), ]
      row.names(temp.cluster.groups) <- 1:nrow(temp.cluster.groups)
      
      #adds biserial correlation to cluster labels
      temp.cluster.groups$R2 <- biserial.cor$df[
        match(
          temp.cluster.groups$variable,     #cluster labels
          biserial.cor$df$variable #variables in biserial correlation output
        ), "R2"
      ]
      
      #gets the maximum of each group
      selected.variables <-
        temp.cluster.groups %>%
        dplyr::group_by(group) %>%
        dplyr::slice(which.max(R2)) %>%
        .$variable
      
      #computes observed max cor
      observed.max.cor <-
        x[, selected.variables] %>%
        cor() %>%
        as.dist() %>%
        as.vector() %>%
        abs() %>%
        max()
      
      observed.max.cor
      
    }#end of while
    
    
    #prepares cluster plotting
    temp.cluster.data <- ggdendro::dendro_data(temp.cluster)
    
    #gets R2
    temp.cluster.data$labels$R2 <- biserial.cor$df[
      match(
        temp.cluster.data$labels$label, #cluster labels
        biserial.cor$df$variable        #variables biserial.cor
      ), "R2"
    ]
    
    #gets labels
    labs <- ggdendro::label(temp.cluster.data)
    
    #adds arrow to label if the variable is selected
    labs$label <- as.character(labs$label)
    for(i in 1:nrow(labs)){
      if(labs[i, "label"] %in% selected.variables){
        labs[i, "label"] <- paste("\u{2192} ", labs[i, "label"], sep = "")
      }
    }
    labs$label <- factor(labs$label)
    
    
    #plots dendrogram
    cluster.plot <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = ggdendro::segment(temp.cluster.data),
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend)
      ) +
      ggplot2::geom_text(
        data = ggdendro::label(temp.cluster.data),
        aes(
          label = labs$label,
          x = x,
          y = 0,
          colour = labs$R2,
          hjust = 1
        ),
        size = text.size
      ) +
      ggplot2::coord_flip(ylim = c(-(max(height.range)/2), max(height.range))) +
      viridis::scale_colour_viridis(direction = -1, end = 0.9)  +
      ggplot2::theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(2,2,2,2), "lines"),
        axis.text.x = element_text(size = text.size * 2),
        legend.position = "bottom",
        legend.key.width = unit(2, "lines")
      ) +
      ggplot2::labs(colour = "Biserial correlation") +
      ggplot2::geom_hline(
        yintercept = height.cutoff,
        col = "red4",
        linetype = "dashed",
        size = 1,
        alpha = 0.5
      ) +
      ggplot2::scale_y_continuous(breaks = c(1 - (max(height.range) / 2), 0, 0.25, 0.5, 0.75, 1)) +
      ggplot2::ylab("Correlation difference")
    
    if(plot == TRUE){
      ggplot2::theme_set(cowplot::theme_cowplot())
      print(cluster.plot)
    }
    
  }
  
  #preparing output
  output.list$plot <- cluster.plot
  output.list$vars <- selected.variables
  return(output.list)
  
}


#plot map and histogram of a variable in ecoregions
plot_variable_distribution <- function(
  ecoregions_polygons,
  ecoregions,
  variable = bias_records_per_km2,
  title = "Records per km2",
  viridis.direction = 1,
  binwidth = 1){
  
  pa <- ggplot2::ggplot(data = ecoregions_polygons) +
    ggplot2::geom_sf(aes_string(fill = variable), size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = viridis.direction) + 
    ggplot2::theme(legend.position = "right", legend.key.height = unit(0.5, "cm")) + 
    ggplot2::ggtitle(title) + 
    ggplot2::labs(fill = "")
  
  pb <- ggplot2::ggplot(data = ecoregions) +
    ggplot2::geom_histogram(aes_string(
      x = variable, 
      fill = factor(ecoregions[, variable])
    ), binwidth = binwidth
    ) +
    ggplot2::scale_fill_viridis_d(direction = viridis.direction) + 
    ggplot2::theme(legend.position = "none") + 
    ggplot2::ylab("") + 
    ggplot2::xlab("")
  
  p <- pa | pb
  
  return(p)
  
}

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


#computes betadiversity scores for two ecoregions
betadiversity <- function(
  ecoregion.1,
  ecoregion.2,
  taxa.list,
  taxa.column
){
  
  #getting species lists
  eco1 <- unique(taxa.list[taxa.list$ecoregion %in% ecoregion.1, taxa.column])
  eco2 <- unique(taxa.list[taxa.list$ecoregion %in% ecoregion.2, taxa.column])
  
  #extracting betadiversity components
  a <- length(intersect(eco1, eco2))
  b <- length(setdiff(eco1, eco2))
  c <- length(setdiff(eco2, eco1))
  abc <- (a + b + c)
  
  #betadiversity components as percentage of the species pool
  a. <- (a * 100) / abc
  b. <- (b * 100) / abc
  c. <- (c * 100) / abc
  
  #richness similarity
  R <- abs((a + b) - (a + c))
  R. <- abs((a. + b.) - (a. + c.))
  
  #composition similarity
  # C <- (b + c) / a
  
  #Sorensen similarity index
  #there are different equations out there for this index
  #Koleff: 2 * a / 2 * a + b + c
  #Baselga: b + c / 2 * a + b + c
  #here I substract it to 1 to convert it's values into dissimilarity, as in Benito et al. 2011.
  Bsor <- 1 - (2 * a / (2 * a + b + c))
  
  #Simpson's similarity index
  Bsim <- min(b, c) / (min(b, c) + a)
  
  #saving output
  out.list <- list()
  
  out.list$a <- a
  out.list$b <- b
  out.list$c <- c
  out.list$a_percent <- a.
  out.list$b_percent <- b.
  out.list$c_percent <- c.
  out.list$R <- R
  out.list$R_percent <- R.
  # out.list$C <- C
  out.list$Bsor <- Bsor
  out.list$Bsim <- Bsim
  
  #as named vector
  out.list <- unlist(
    out.list, 
    use.names = TRUE
    )
  
  return(out.list)
  
}

#returns ecoregion geom from database connection
pull_ecoregion_geom <- function(ecoregion, plot = TRUE){
  
  #finding a postgresql connection in the environment
  connection.name <- Filter(
    function(x)
      'PostgreSQLConnection' %in% class(get(x) ), 
    ls(envir=.GlobalEnv)
    )[1]
  
  #if connection not found
  if(is.na(connection.name)){
    stop("PostgreSQL connection not found")
  }
  
  #pulling geometry
  geom <- rpostgis::pgGetGeom(
    conn = get(connection.name),
    name = c("public", "ecoregions"),
    geom = "geom",
    other.cols = c("ecoregion"),
    clauses = paste(
      "WHERE ecoregion = '", 
      ecoregion, 
      "';", 
      sep = ""
    )
  )
  
  #plotting geom
  if(plot == TRUE){
  sp::plot(geom, main = ecoregion)
  }
  
  return(geom)
  
}

#extracting climate values for a given ecoregion
ecoregion_raster_extract <- function(
  ecoregion,
  raster
  ){
  
  #pulling ecoregion geom
  ecoregion.geom <- suppressWarnings(
      suppressMessages(
        pull_ecoregion_geom(
        ecoregion,
        plot = FALSE
        )
      )
    )
  
  #raster extractioin
  out.df <- raster::extract(
    x = raster,
    y = ecoregion.geom,
    df = TRUE,
    cellnumbers = FALSE
  ) %>% 
    na.omit() %>% 
    dplyr::select(-ID) %>% 
    dplyr::mutate(
      ecoregion = ecoregion
    )
  
  return(out.df)
  
}

#compute hypervolume of an ecoregion
ecoregion_hypervolume <- function(
  ecoregion,
  raster
){
  
  #pulling ecoregion geom and extracting raster values
  ecoregion.data <- ecoregion_raster_extract(
    ecoregion,
    raster
  )
  
  #sampling the data if nrow is too high
  if(nrow(ecoregion.data) > 10000){
    ecoregion.data <- ecoregion.data[sample(
      1:nrow(ecoregion.data), 
      size = round(nrow(ecoregion.data)/4, 0)
      ), ]
  }
  
  #computing hypervolume
  hv <- hypervolume_svm_(
    data = ecoregion.data[, names(raster)],
    name = ecoregion,
    samples.per.point = 10,
    verbose = FALSE,
    chunk.size = 20000
  )
  
  return(hv)
  
}

#function to compute ecoregion fragmentation
ecoregion_fragmentation <- function(
  ecoregion
){
  
  #pulling geometry of ecoregion and reprojecting to LAEA
  ecoregion.geom <- suppressWarnings(
    suppressMessages(
      pull_ecoregion_geom(
        ecoregion = ecoregion,
        plot = FALSE
        )
      )
    ) %>% 
    sp::spTransform(CRSobj = "+proj=laea")
  
  #creating raster template at 5km resolution
  raster.template <- raster::raster(
    x = ecoregion.geom, 
    resolution = 2000
  )
  
  #rasterizing geom
  ecoregion.geom.raster <- raster::rasterize(
    x = ecoregion.geom,
    y = raster.template,
    background = 0,
    silent = TRUE
    )
 
  
  #computing fragmentation measures
  ecoregion.fragmentation <- landscapemetrics::calculate_lsm(
    landscape = ecoregion.geom.raster,
    what = c(
      "lsm_c_ca", #total (class) area
      "lsm_c_te", #total edge
      "lsm_c_ed", #edge density
      "lsm_c_ai", #aggregation index	
      "lsm_c_cohesion", #patch cohesion index	
      "lsm_c_division", #division index
      "lsm_c_clumpy", #clumpiness index
      "lsm_c_lsi", #landscape shape index	
      "lsm_c_nlsi", #normalized landscape shape index
      "lsm_c_mesh", #effective mesh size	
      "lsm_c_tca", #total core area
      "lsm_c_cpland", #core area percentage of landscape	
      "lsm_c_core_mn", #core area mean
      "lsm_c_dcore_mn", #mean disjunct core area
      "lsm_c_ndca", #number of disjunct core areas
      "lsm_c_dcad", #disjunct core area density
      "lsm_c_np", #number of patches
      "lsm_c_pd", #patch density
      "lsm_c_area_mn", #mean patch area
      "lsm_c_contig_mn", #contiguity index mean
      "lsm_c_para_mn", #perimeter area ratio mean
      "lsm_c_shape_mn" #shape index mean
    )
  ) %>% 
    dplyr::filter(
      class == 1
    ) %>% 
    dplyr::select(
      metric,
      value
    ) %>% 
    as.data.frame()
  
  return(ecoregion.fragmentation)
  
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
#' @usage s_lower_vif(
#'   x,
#'   select.cols = NULL,
#'   omit.cols = c("x", "y", "presence"),
#'   preference.order = NULL,
#'   biserial.cor = NULL,
#'   verbose = TRUE
#' )
#'
#' @param x A training data frame. Non-numeric columns are excluded from the analysis.
#' @param select.cols Character vector, names of the columns which VIF wants to be assessed. If \code{NULL}, all numeric variables but \code{presence.column} are considered. It is recommended to use the variable order of the \code{variable} column from the data frame output of \code{\link{s_biserial_cor}}.
#' @param omit.cols Character vector, variables to exclude from the analysis. Defaults to \code{c("x", "y", "presence")}.
#' @param preference.order Character vector, column names of \code{x} in an order of selection priority desired by the user. For example, if \code{preference.order = c("bio1", "bio2", "bio3")}, the algorithm will first compute vif for all variables in \code{x} not included in \code{preference.order}, and remove on each step the variable with a higher vif. Then, vif is computed iteratively on the variables in \code{preference.order}, but removing always the variable with the lowest priority (instead of the variable with the higher vif). Finally, all variables resulting from both vif analyses are grouped together, and a new vif analysis is performed, removing first the variables not in \code{preference.order}. In summary, this option will try to preserve a set of variables as long as their vif values allow it. This option is incompatible with the argument \code{biserial.cor} (see below).
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
#' #1. only x and omit.cols are provided
#' #variables with max vif are removed on each step
#'
#' vif.auto.out <- s_lower_vif(
#'   x = virtual.species.training
#' )
#'
#'
#' #2. biserial.cor is provided
#' #variables are processed according to the
#' #priority established by s_biserial_cor()
#'
#' biserial.cor <- s_biserial_cor(
#'   x = virtual.species.training,
#'   response.col = "presence",
#'   omit.cols = c("x", "y"),
#'   plot = FALSE
#' )
#'
#' vif.auto.out <- s_lower_vif(
#'   x = virtual.species.training,
#'   biserial.cor = biserial.cor
#' )
#'
#' #3, preference.order is provided
#' #variables in preference.order are selected by preference
#' #the other variables are selected by removing those with max vif
#'
#' vif.auto.out <- s_lower_vif(
#'   x = virtual.species.training,
#'   preference.order = c("bio1", "bio5", "bio6", "bio12")
#' )
#'
#'}
#'
#' @author Blas Benito <blasbenito@gmail.com>. The function \code{\link[HH]{vif}} is authored by Richard M. Heiberger <rmh@temple.edu>.
#' @references Heiberger, Richard M. and Holland, Burt (2004). Statistical Analysis and Data Display: An Intermediate Course with Examples in S-Plus, R, and SAS. Springer Texts in Statistics. Springer. ISBN 0-387-40270-5.
#' @export
s_lower_vif <- function(
  x,
  select.cols = NULL,
  omit.cols = c("x", "y", "presence"),
  preference.order = NULL,
  biserial.cor = NULL,
  verbose = TRUE
){
  
  #dropping omit.cols
  if(sum(omit.cols %in% colnames(x)) == length(omit.cols)){
    x <-
      x %>%
      dplyr::select(-tidyselect::all_of(omit.cols))
  }
  
  #selecting select.cols
  if(is.null(select.cols) == FALSE){
    if(sum(select.cols %in% colnames(x)) == length(select.cols)){
      x <-
        x %>%
        dplyr::select(tidyselect::all_of(select.cols))
    }
  }
  
  #getting numeric columns only and removing cases with NA
  x <-
    x[, unlist(lapply(x, is.numeric))] %>%
    na.omit()
  
  #preparing preference order if provided
  if (is.null(preference.order) == FALSE){
    preference.order <- preference.order[preference.order %in% colnames(x)]
  }
  
  #message
  if(verbose == TRUE){cat("Removed variables: ")}
  
  #IF biserial.cor IS NOT PROVIDED
  if(is.null(biserial.cor) == TRUE){
    
    #AND preference.order IS NOT PROVIDED
    if(is.null(preference.order) == TRUE){
      
      #OPTION 3: SELECT BY MAX VIF
      output.list <- .select_by_max_vif(
        x = x,
        verbose = verbose
      )
      
    } else {
      
      #OPTION 2: preference.order IS PROVIDED
      
      #selecting by preference
      output.list.by.preference <- .select_by_preference(
        preference.order = preference.order,
        x = x,
        verbose = verbose
      )
      
      #selecting by max vif (variables not in preference.order)
      output.list.by.max.vif <- .select_by_max_vif(
        x = x[, !(colnames(x) %in% preference.order)],
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
        x = x,
        verbose = verbose
      )
      
    }
    
  } else {
    
    if(inherits(biserial.cor, "s_biserial_cor") == TRUE){
      
      #option 1: computing vif by preference
      output.list <- .select_by_preference(
        preference.order = biserial.cor$df$variable,
        x = x,
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



auto_vif <- function(
  x,
  preference.order = NULL,
  verbose = TRUE
){
  
  #message
  if(verbose == TRUE){cat("Removed variables: ")}
    
    #AND preference.order IS NOT PROVIDED
    if(is.null(preference.order)){
      
      #OPTION 3: SELECT BY MAX VIF
      output.list <- .select_by_max_vif(
        x = x,
        verbose = verbose
      )
      
    } else {
      
      #OPTION 2: preference.order IS PROVIDED
      
      #getting only preference.order in colnames(x)
      preference.order <- preference.order[preference.order %in% colnames(x)]
      
      #selecting by preference
      output.list <- .select_by_preference(
        x = x,
        preference.order = preference.order,
        verbose = verbose
      )
      
      #if there are variables outside of preference.order
      if(sum(preference.order %in% colnames(x)) != ncol(x)){
      
        #selecting by max vif (variables not in preference.order)
        output.list.by.max.vif <- .select_by_max_vif(
          x = x[, !(colnames(x) %in% preference.order)],
          verbose = verbose
        )
        
        #merging selected.vars
        selected.vars <- c(
          output.list$vars,
          output.list.by.max.vif$vars
        )
        
        #vif by preference again
        output.list <- .select_by_preference(
          preference.order = selected.vars,
          x = x,
          verbose = verbose
        )
      
      }
      
    }
  
  #message
  if(verbose == TRUE){cat("Done! \n")}

  #returning output
  output.list
  
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
.select_by_max_vif <- function(x, verbose){
  
  #initializing selected vars
  selected.vars <- colnames(x)
  
  #computes vif
  repeat {
    
    #computes vif
    vif.df <- .vif_to_df(x = x[, selected.vars])
    
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
  vif.df <- .vif_to_df(x = x[, selected.vars])
  
  #output list
  output.list <- list()
  output.list$df <- vif.df
  output.list$vars <- selected.vars
  class(output.list) <- c("list", "s_vif_auto")
  return(output.list)
  
}


#' @export
.select_by_preference <- function(
  x, 
  preference.order, 
  verbose
  ){
  
  #subsets to the variables already available in x
  preference.order <- preference.order[preference.order %in% colnames(x)]
  
  #initiating selected vars
  selected.vars <- preference.order[1]
  
  #iterates through preference order
  for(i in 2:length(preference.order)){
    
    #new.var
    new.var <- preference.order[i]
    
    #computes vif
    vif.df <- .vif_to_df(x = x[, c(selected.vars, new.var)])
    
    #if vif of new.var lower than 10, keep it
    if(max(vif.df$vif) <= 10){
      
      selected.vars <- c(selected.vars, new.var)
      
    } else {
      
      #message
      if(verbose == TRUE){cat(paste(new.var, ", ", sep = ""))}
      next
      
    }
    
  }
  
  #final vif.df
  vif.df <- .vif_to_df(x = x[, selected.vars])
  
  #output list
  output.list <- list()
  output.list$df <- vif.df[, c("variable", "vif")]
  output.list$vars <- selected.vars
  class(output.list) <- c("list", "s_vif_auto")
  return(output.list)
  
}
