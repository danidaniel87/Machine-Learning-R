LogisticRegression <- function(data, target.var.name, predictors.var.names,
                               model.specifications) {
  #
  # Construir modelo de regresion logistica
  #
  # Args
  #   data.train           : datos completos (target + explicativas)
  #   target.var.name      : nombre del target
  #   predictors.var.names : nombre de las variables explicativas
  #   model.specifications : parametros especificos del modelo
  #
  # Output
  #   objeto de tipo model con los resultados de la aplicacion

  
  ## Lectura de los parametros de entrada
  predictors.as.factor <- model.specifications$predictors.as.factor
  max.num.predictors   <- model.specifications$max.explicativas
  value.alpha.in       <- model.specifications$alpha.entrada
  value.alpha.out      <- model.specifications$alpha.salida
  
  
  ## ¿Las variables son factores?
  if (predictors.as.factor) {
    data[, predictors.var.names] <- apply(data[, predictors.var.names], 2, factor)
  } 
  

  ## Inicializacion previa a bucle seleccion de variables
  possible.predictors <- predictors.var.names
  possible.xx.all     <- paste(possible.predictors, collapse = " + ")
  possible.xx         <- paste("~", possible.xx.all, sep = " ")

  model.predictors <- "1"
  formula.none     <- paste(target.var.name, model.predictors, sep = " ~ ")


  data.model.iterative           <- data[, c(target.var.name, possible.predictors)]
  force(data.model.iterative)
  colnames(data.model.iterative) <- c(target.var.name, possible.predictors)

  condition           <- TRUE
  k.predictors        <- 0
  max.predictors      <- max.num.predictors
  alpha.in            <- value.alpha.in
  alpha.out           <- value.alpha.out
  formula.model       <- formula.none
  model.iterative     <- eval(substitute(glm(formula.model, data = data.model.iterative, family = "binomial")))
  iter                <- 0
  AIC.values          <- c()
  candidate.drop.name <- ""
  iteration           <- paste("Iteration", iter, sep = " ")
  cat(iteration, "Modelo ", names(model.iterative$coefficients), model.iterative$coefficients, sep = "\n")
  variables           <- possible.xx.all
  possibleVariables   <- possible.xx

  ## Bucle seleccion de variables
  while (condition) {
    # 1º Identificacion de candidato de entrada
    cat(paste('     PASO 1', "\n"))
    iteration  <- paste("Iteration", iter + 1, sep = " ")
    cat(iteration, sep = "\n")
    cat(paste('modelo actual: ',names(model.iterative$coefficients), model.iterative$coefficients,"\n"))
  
    add.result      <- eval(substitute(add1(model.iterative, formula(possibleVariables), test = "Rao")))
    min.add1.pvalue <- which.min(x = add.result[,"Pr(>Chi)"])
     
    candidate.name     <- row.names(add.result[min.add1.pvalue,])
    candidate.p.value  <- add.result[min.add1.pvalue, "Pr(>Chi)"]
      
    cat("Candidato para entrar:", candidate.name, "  --   p.value:", candidate.p.value, sep = " ")
    cat("\n")
    
	# 2º Comprobacion criterio de entrada: alpha.in
	if (candidate.p.value > alpha.in) {
      cat("STOP -- p.value:", candidate.p.value, "exceeds reference:", alpha.in, "\n", sep = " ")
      condition <- FALSE
    } else {
    
      model.predictors       <- paste(model.predictors, candidate.name, sep = " + ")
      possible.formula.model <- formula(paste(target.var.name, model.predictors, sep = " ~ "))
      
	  # 3º Identificacion de candidato de salida
      if (k.predictors >= 1) {
        cat('     PASO 2: k.predictors >= 1')
        drop.result <- eval(substitute(drop1(model.iterative, test="Rao")))
        max.drop1.1 <- which.max(x = drop.result[,"Pr(>Chi)"])
      
        candidate.drop.name    <- row.names(drop.result[max.drop1.1,])
        candidate.drop.p.value <- drop.result[max.drop1.1, "Pr(>Chi)"]
      
        cat("Candidato para salir:", candidate.drop.name , "  --   p.value:", candidate.drop.p.value, sep = " ")
        cat("\n")
        
		# 4º Comprobacion criterio de salida: alpha.out
        if (candidate.drop.p.value < alpha.out) {
          cat("  continua -- p.value:", candidate.drop.p.value, "is smaller than the reference:", alpha.out, "\n", sep = " ")
        } else {    
          cat(candidate.drop.name, "sale", sep = " ")
          cat("\n")
          # Elimininacion de la variable del modelo
		  #   Para eliminar la variable del modelo
          #   model.predictors es un string con las variables explicativas separadas por "+": "nombreVar1+nombreVar2+nombreVar3" 
          model.predictors <- unlist(strsplit(model.predictors, "\\+")) #Partir el string por el caracter "+"
          model.predictors <- model.predictors[-(grep(candidate.drop.name, model.predictors, fixed = TRUE))] # Eliminar la posicion de la variable a excluir
          model.predictors <- paste(model.predictors, collapse = "+") #Recontruir model.predictors con el formato correcto
        
          # Elimininacion de la variable de la lista de posibles predictoras
          variables <- unlist(strsplit(gsub(" ", "", variables, fixed = TRUE), "\\+"))
          variables <- variables[-(grep(candidate.drop.name, variables))]
          variables <- paste(variables, collapse = "+")
          possibleVariables <- paste("~", variables, sep = " ")
        
          # Formula sin la variable eliminada
          possible.formula.model <- formula(paste(target.var.name, model.predictors, sep = " ~ ")) 
          k.predictors <- k.predictors - 1
        }
      
      }
    
      # 5º Construccion del posible modelo 
      cat("Nueva posible formula", deparse(possible.formula.model), sep = "\n")
    
      possible.model <- eval(substitute(glm(formula = possible.formula.model, 
                            family  = "binomial", 
                            data    = data.model.iterative)))
      cat("Posible Modelo", sep = "\n")
      print(possible.model$coefficients)
      cat("\n")
      
	  # 6º Comprobacion de los coeficientes del modelo
      na.coefficients       <- which(is.na(possible.model$coefficients))
      negative.coefficients <- sum(possible.model$coefficients[2:length(possible.model$coefficients)] < 0, na.rm = TRUE)
      gt25.coefficients     <- sum(possible.model$coefficients[2:length(possible.model$coefficients)]>2.5, na.rm = TRUE)
      if (length(na.coefficients) == 0 && negative.coefficients == 0  &&  gt25.coefficients == 0) {
        coefficient.condition <- TRUE
        cat('     PASO 3: coefficient.condition <- TRUE ')
        cat("\n")
        cat(paste('na.coefficients:', na.coefficients))
        cat("\n")
        cat(paste('negative.coefficients:', negative.coefficients))
        cat("\n")
        cat(paste('gt25.coefficients', gt25.coefficients ))
        cat("\n")
      } else {
        coefficient.condition <- FALSE
        
        cat('     PASO 3: coefficient.condition <- FALSE ')
        cat("\n")
        cat(paste('na.coefficients:', na.coefficients))
        cat("\n")
        cat(paste('negative.coefficients:', negative.coefficients)) 
        cat("\n")
        cat(paste('gt25.coefficients', gt25.coefficients ))
        cat("\n")
        
      }
    
	  # 7º Consolidacion del modelo
      if (coefficient.condition) {
        model.iterative      <- possible.model
        formula.model        <- possible.formula.model
        k.predictors         <- k.predictors + 1
        iter                 <- iter+1
        AIC.values[iter]     <- extractAIC(model.iterative)[2]
      } else {
        cat("EXCLUDE -- coefficients don't satisfy conditions", sep = "\n")

        # Elimininacion de la variable del modelo
        model.predictors <- unlist(strsplit(model.predictors, "\\+")) 
        model.predictors <- model.predictors[-(grep(candidate.name, model.predictors, fixed = TRUE))] 
        model.predictors <- paste(model.predictors, collapse = "+") 
        
        # Elimininacion de la variable de la lista de posibles predictoras
        variables <- unlist(strsplit(gsub(" ", "", variables, fixed = TRUE), "\\+"))
        variables <- variables[-(grep(candidate.name, variables))]
        variables <- paste(variables, collapse = "+")
        possibleVariables <- paste("~", variables, sep = " ")
     
        # Formula sin la variable eliminada
        possible.formula.model <- formula(paste(target.var.name, model.predictors, sep = " ~ ")) 
        
      }
    
	  # 8º Comprobacion de condicion de parada de numero de coeficientes
      if (k.predictors == max.predictors) {
        cat("STOP -- maximun coefficients number achieved", sep = "\n")
        condition <- FALSE
      }
    }
  } 
  
  # Grafico de valores AIC del modelo en cada iteracion
  plot(AIC.values)
  title(target.var.name)

  return(model.iterative)
}




