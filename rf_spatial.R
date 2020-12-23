rf_spatial <- function(
  data = NULL,
  dependent.variable.name = NULL,
  predictor.variable.names = NULL,
  seed = NULL,
  white.noise = FALSE,
  autocorrelated.noise = FALSE,
  weighted.distance.matrix = NULL,
  pca.factors = NULL,
  iterations = 10,
  trees.per.variable = NULL,
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
  
  #1 CHECKING AUTOCORRELATION OF THE RESPONSE
  moran(x = ,
        weighted.distance.matrix = 
          )
  
  #2 FITTING INITIAL MODEL
  
  #fitting first model to check autocorrelation of residuals
  m <- rf(
    data = data,
    dependent.variable.name = dependent.variable.name,
    predictor.variable.names = predictor.variable.names,
    seed = seed,
    white.noise = white.noise,
    autocorrelated.noise = autocorrelated.noise,
    weighted.distance.matrix = weighted.distance.matrix,
    trees.per.variable = trees.per.variable,
    ranger.arguments = ranger.arguments
  )
  
  #extracting autocorrelation of the residuals
  residuals.moran <- m$residuals.moran %>% 
    dplyr::arrange(desc(moran.i))
  
  #adding "matrix" column if pca.factors is not a list
  if(!("matrix" %in% colnames(residuals.moran)) | !is.list(pca.factors)){
    
    #coercing pca.factors into a list
    pca.factors.list <- list()
    pca.factors.list[[1]] <- pca.factors
    pca.factors <- pca.factors.list
    
    #adding matrix column
    residuals.moran$matrix <- 1
    
  }
  
  
  #SELECTION OF PCA FACTORS
  #list to store selected factors
  selected.factors <- list()
  
  #iterating through rows in residuals.moran
  for(i in 1:nrow(residuals.moran)){
    
    #generate data
    residuals.data.i <- data.frame(
      residuals = m$residuals,
      pca.factors[[residuals.moran[i, "matrix"]]]
    )
    
    #fit model residuals ~ pca.factors
    residuals.vs.pca.i <- rf(
      data = residuals.data.i,
      dependent.variable.name = "residuals",
      predictor.variable.names = colnames(pca.factors[[residuals.moran[i, "matrix"]]]),
      seed = seed,
      white.noise = FALSE,
      autocorrelated.noise = FALSE,
      weighted.distance.matrix = weighted.distance.matrix,
      trees.per.variable = trees.per.variable,
      ranger.arguments = ranger.arguments
    )
    
    #gather importance to set the order
    pca.factor.order.i <- residuals.vs.pca.i$variable.importance.df$variable
    
    #fitting models with increasing list of factors
    for(factor.j in 1:length(pca.factor.order.i)){
      
      #select the factors to use
      factors.j <- pca.factor.order[1:factor.j]
      
      #generating training data
      training.data.j <- data.frame(
        data,
        pca.factors[[residuals.moran[i, "matrix"]]][, factors.j]
      )
      colnames(training.data.j) <- c(colnames(data), factors.j)
      
      #fitting model
      m.j <- rf(
        data = training.data.j,
        dependent.variable.name = dependent.variable.name,
        predictor.variable.names = c(predictor.variable.names, factors.j),
        seed = seed,
        white.noise = FALSE,
        autocorrelated.noise = FALSE,
        weighted.distance.matrix = weighted.distance.matrix[[residuals.moran[i, "matrix"]]],
        trees.per.variable = trees.per.variable,
        ranger.arguments = ranger.arguments
      )
      
      #if residuals are not correlated, save and break
      if(m.j$residuals.moran$interpretation == "No autocorrelation"){
        
        selected.factors[[residuals.moran[i, "matrix"]]] <- pca.factors[[residuals.moran[i, "matrix"] ]][, factors.j]
        
        break
        
      }
      
      #if still correlated when done, nothing to be saved
      if(factor.j == length(pca.factor.order.i) & m.j$residuals.moran$interpretation == "Positive autocorrelation"){
        
        selected.factors[[residuals.moran[i, "matrix"]]] <- NULL
        
      }
      
      
    }#end of j iterations
    
    
  }#end of i iterations
  
  
  
}