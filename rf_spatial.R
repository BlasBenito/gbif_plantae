rf_spatial <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  distance.matrix = NULL,
  distance.thresholds = NULL,
  method = c("rfsp", "hengl", "pca.ordered", "pca.optimized"),
  seed = NULL,
  white.noise = FALSE,
  autocorrelated.noise = FALSE,
  iterations = 1,
  trees.per.variable = NULL,
  cluster.ips = NULL,
  cluster.cores = NULL,
  cluster.user = NULL,
  cluster.port = 11000,
  verbose = TRUE,
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
  
  #testing method argument
  method <- match.arg(method)
  
  #fitting initial model
  if(iterations == 1){
    m <- rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      seed = seed,
      white.noise = white.noise,
      autocorrelated.noise = autocorrelated.noise,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments
    )
  } else {
    m <- repeat_rf(
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      seed = seed,
      white.noise = white.noise,
      iterations = iterations,
      autocorrelated.noise = autocorrelated.noise,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
      )
  }
  
  #plot moran I temporary
  print(m$spatial.correlation.residuals$plot)
  
  #extracting autocorrelation of the residuals
  m.spatial.correlation.residuals <- m$spatial.correlation.residuals$df %>% 
    dplyr::arrange(desc(moran.i)) %>% 
    dplyr::filter(interpretation == "Positive spatial correlation")
  
  #if residuals are not autocorrelated, return original model
  if(nrow(m.spatial.correlation.residuals) == 0){
    
    if(verbose == TRUE){
      message("Residuals are not spatially correlated, this model is good to go!")
      print(m$spatial.correlation.residuals$plot)
    }
   
   return(m) 
    
  #beginning of methods
  } else {
    
    #names of spatial_structure columns
    spatial.variable.names <- paste0("spatial_structure_", 1:ncol(distance.matrix))
    
    #METHOD RFsp or Hengl
    #########################################################
    if(method %in% c("rfsp", "hengl")){
      
      #change colnames of the distance matrix to spatial_structure_X
      colnames(distance.matrix) <- spatial.variable.names
      
      #add distance matrix to data
      data.spatial <- cbind(
        data,
        distance.matrix
      )
      
      #new predictor variable names
      predictor.variable.names.spatial <- c(
        predictor.variable.names,
        spatial.variable.names
      )
      
      #only one iteration
      if(iterations == 1){
        
          #fit model
          m.spatial <- rf(
            data = data.spatial,
            dependent.variable.name = dependent.variable.name,
            predictor.variable.names = predictor.variable.names.spatial,
            seed = seed,
            white.noise = white.noise,
            autocorrelated.noise = autocorrelated.noise,
            distance.matrix = distance.matrix,
            distance.thresholds = NULL,
            trees.per.variable = trees.per.variable,
            ranger.arguments = ranger.arguments
          )
          
          #preparing variable importance
          importance.df <- m.spatial$variable.importance$df
          spatial.structure.df <- importance.df[grepl(
            "spatial_structure", 
            importance.df$variable
          ),]
          spatial.structure.df$variable <- "spatial_structure"
          predictors.df <- importance.df[!grepl(
            "spatial_structure", 
            importance.df$variable
          ),]
          
          #plot
          importance.df <- rbind(
            spatial.structure.df,
            predictors.df
          )
          importance.plot <- ggplot2::ggplot(data = importance.df) + 
            ggplot2::aes(
              x = importance, 
              y = reorder(
                variable, 
                importance, 
                FUN = max
              )
            ) + 
            ggplot2::geom_point(alpha = 0.5, size = 4) + 
            ggplot2::ylab("")
          
          m.spatial$variable.importance$df <- importance.df
          m.spatial$variable.importance$plot <- importance.plot
          
          if(verbose == TRUE){
            
            print(m.spatial$spatial.correlation.residuals$plot)
            
            if(sum(m.spatial$spatial.correlation.residuals$df$interpretation == "No spatial correlation") == nrow(m.spatial$spatial.correlation.residuals$df)){
              message("The model residuals are not spatially correlated.")
            } else {
              if(sum("Positive spatial correlation" %in% m.spatial$spatial.correlation.residuals$df$interpretation) > 0){
                message("The model residuals are spatially correlated")
              }
            }
            
          }
          
          return(m.spatial)
        
      } else {
          
        #fit model
        m.spatial <- repeat_rf(
          data = data.spatial,
          dependent.variable.name = dependent.variable.name,
          predictor.variable.names = predictor.variable.names.spatial,
          distance.matrix = distance.matrix,
          distance.thresholds = distance.thresholds,
          iterations = iterations,
          white.noise = white.noise,
          autocorrelated.noise = autocorrelated.noise,
          trees.per.variable = trees.per.variable,
          ranger.arguments = ranger.arguments
        )
        
        print(m.spatial$spatial.correlation.residuals$plot)
        
        #preparing variable importance
        importance.df <- m.spatial$variable.importance$df.long
        spatial.structure.df <- importance.df[grepl(
          "spatial_structure", 
          importance.df$variable
        ),]
        spatial.structure.df$variable <- "spatial_structure"
        predictors.df <- importance.df[!grepl(
          "spatial_structure", 
          importance.df$variable
        ),]
        
        #plot
        importance.df <- rbind(
          spatial.structure.df,
          predictors.df
        )
        importance.plot <- ggplot2::ggplot(data = importance.df) + 
          ggplot2::aes(
            x = importance, 
            y = reorder(
              variable, 
              importance, 
              FUN = mean
            )
          ) + 
          ggplot2::geom_boxplot() + 
          ggplot2::ylab("")
        
        m.spatial$variable.importance$df <- importance.df
        m.spatial$variable.importance$plot <- importance.plot
        
        return(m.spatial)
        
      }# END OF if(iterations == 1){
      
    #END OF METHOD RFsp or Hengl
    #########################################################
    }# END OF if(method %in% c("rfsp", "hengl")){
    
    #PCA-based methods start here
    
    #creating fast version of ranger.arguments
    ranger.arguments.fast <- ranger.arguments
    ranger.arguments.fast$importance <- "none"
    ranger.arguments.fast$scale.permutation.importance <- FALSE
    ranger.arguments.fast$keep.inbag <- FALSE
    ranger.arguments.fast$local.importance <- FALSE
    
    #computing pca factors for pca methods
    pca.factors <- pca_distance_matrix(
      distance.matrix = distance.matrix,
      distance.threshold =  m.spatial.correlation.residuals$distance.threshold
    )
    
    #ranking pca factors
    #as per tests, best practice here is to compute the VIF of the selected pca factors inside of rank_pca_factors()
    rank.pca.factors <- rank_pca_factors(
      pca.factors.df = pca.factors,
      data = data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = predictor.variable.names,
      reference.moran.i = m$spatial.correlation.residuals$max.moran,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments.fast,
      cluster.ips = cluster.ips,
      cluster.cores = cluster.cores,
      cluster.user = cluster.user,
      cluster.port = cluster.port,
      vif.filtering = TRUE
    )
      
    #METHOD pca.fast
    #########################################################
    if(method == "pca.ordered"){
      
      #parallelized version
      best.pca.factors <- add_pca_factors_ordered(
        pca.factors.df = pca.factors,
        pca.factors.rank = rank.pca.factors,
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments,
        cluster.ips = cluster.ips,
        cluster.cores = cluster.cores,
        cluster.user = cluster.user,
        cluster.port = cluster.port
      )
      
      best.pca.factors.names <- best.pca.factors$best.pca.factors
      
    }#end of if(method == "pca.fast"){
    
    
    #METHOD pca.full
    #########################################################
    if(method == "pca.optimized"){
      
      best.pca.factors <- add_pca_factors_optimized(
        pca.factors.df = pca.factors,
        pca.factors.rank = rank.pca.factors,
        data = data,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = predictor.variable.names,
        distance.matrix = distance.matrix,
        distance.thresholds = distance.thresholds,
        ranger.arguments = ranger.arguments.fast,
        cluster.ips = NULL,
        cluster.cores = NULL,
        cluster.user = NULL,
        cluster.port = NULL
      )
      
    }#end of if(method == "pca.tree.search"){
      
  } #end of methods
  
  #fitting best model
  
  #prepare data with best pca factors
  best.data <- data.frame(
    data,
    pca.factors[, best.pca.factors$best.pca.factors]
  )
  colnames(best.data) <- c(colnames(data), best.pca.factors$best.pca.factors)
  
  #prepare predictor variable names
  best.predictor.variable.names <- c(
    predictor.variable.names,
    best.pca.factors$best.pca.factors
  )
  
  #fitting best model
  if(iterations == 1){
    
    m.spatial <- rf(
      data = best.data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = best.predictor.variable.names,
      white.noise = white.noise,
      autocorrelated.noise = autocorrelated.noise,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      ranger.arguments = ranger.arguments
    )
    
  } else {
    
    m.spatial <- repeat_rf(
      data = best.data,
      dependent.variable.name = dependent.variable.name,
      predictor.variable.names = best.predictor.variable.names,
      seed = seed,
      white.noise = white.noise,
      iterations = iterations,
      autocorrelated.noise = autocorrelated.noise,
      distance.matrix = distance.matrix,
      distance.thresholds = distance.thresholds,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )
    
  }
  
  return(m.spatial)
  
}