# This file contains functions to create a CompartmentalModel object from a visnetwork object
visNetworkToCM <- function(vn) {
  # This function takes a visNetwork object and returns a CompartmentalModel object
  # The object won't be sufficient to run the model but it will be a start
  # The user will need to specify the parameters, initial conditions and timesteps
  # Some defaults will be chosen for the user however in the correct structure
  vnNodes <- vn$x$nodes
  vnEdges <- vn$x$edges
  compartmentNames <- vnNodes$label
  tbTransitions <- vnEdges |>
    rename(rate=label) |>
    left_join(vnNodes |> rename(src=label), by=c("from"="id")) |>
    left_join(vnNodes |> rename(dst=label), by=c("to"="id")) |>
    transmute(src = factor(src, levels=compartmentNames),
              dst = factor(dst, levels=compartmentNames),
              rate = rate) |>
    as_tibble()
  
  parameterNames <- collectSymbols(tbTransitions$rate)
  parameters <- setNames(rep(1, length(parameterNames)), parameterNames) |>
    as.list()
  # tbTransitions
  initialConditions = setNames(rep(100, length(compartmentNames)), compartmentNames)
  resultCM <- CompartmentalModel(
    tbTransitions = tbTransitions,
    parameters = parameters,
    initialConditions = initialConditions,
    timesteps = seq(0, 365, 1)
  )
  resultCM
}

# TODO: Consider introducing a updateCMFromVN function:
updateCMFromVN <- function(cm, vn) {
  # This function takes a compartmental model and a visnetwork object and updates the compartmental model
  # The tbTransitions will be completely replaced
  # The parameters and initial conditions will be updated as required
  # The timesteps will be left as they are
  vnNodes <- vn$x$nodes
  vnEdges <- vn$x$edges
  compartmentNames <- vnNodes$label
  tbTransitions <- vnEdges |>
    rename(rate=label) |>
    left_join(vnNodes |> rename(src=label), by=c("from"="id")) |>
    left_join(vnNodes |> rename(dst=label), by=c("to"="id")) |>
    transmute(src = factor(src, levels=compartmentNames),
              dst = factor(dst, levels=compartmentNames),
              rate = rate) |>
    as_tibble()

  oldParameterNames <- names(cm$parameters)
  oldCompartmentNames <- names(cm$initialConditions)
  excludedSymbols <- union(union(oldParameterNames, oldCompartmentNames), compartmentNames)
  newParameterNames <- collectSymbols(tbTransitions$rate, excludedSymbols = excludedSymbols)
  newParameters <- setNames(rep(1, length(newParameterNames)), newParameterNames) |>
    as.list()
  parameters <- c(cm$parameters, newParameters)
  # Update initial conditions
  oldInitialConditions <- cm$initialConditions
  initialConditions = setNames(rep(1, length(compartmentNames)), compartmentNames)
  for (compartment in compartmentNames) {
    if (compartment %in% names(oldInitialConditions)) {
      initialConditions[compartment] <- oldInitialConditions[compartment]
    }
  }
  # Create the object
  resultCM <- CompartmentalModel(
    tbTransitions = tbTransitions,
    parameters = parameters,
    initialConditions = initialConditions,
    timesteps = cm$timesteps
  )
  
  resultCM
}

# TODO: Move somewhere nicer/rename ####
mt_findSymbolsRecursively <- function(txt, excludedSymbols) {
  parts <- as.list(rlang::parse_expr(txt))
  result <- c()
  for (part in parts) {
    if(is.symbol(part)) {
      if(!(deparse(part) %in% excludedSymbols)) {
        result <- c(result, part)
      } 
    } else {
      if(!(is.numeric(part) | is.character(part)))
        result <- c(result, mt_findSymbolsRecursively(deparse(part), excludedSymbols))
    }
  }
  result <- unique(result)
  blockList <- c('(','*','/','+','-','^')
  result[!sapply(result,deparse) %in% blockList]
}

collectSymbols <- function(rates, excludedSymbols=c('S','E','I','R','.pop','N')) {
  # Given some characters of expressions (representing rates),
  # collect all the symbols used (parameters, compartments etc) and return
  # a single character vector of everything 
  # excluding that which is passed into excludedSymbols
  collapsed <- map(rates, function(x) mt_findSymbolsRecursively(x, excludedSymbols)) |> unlist() |> as.character() |> unique()
  collapsed |> setdiff(excludedSymbols)
}

CM2VN <- function(cm) {
  # This function takes a CompartmentalModel object and returns a visNetwork object
  tbTransitions <- cm$tbTransitions
  nodeNames <- names(cm$initialConditions)
  
  # Create the nodes
  nodes <- data.frame(id = seq_along(nodeNames), label = nodeNames)
  # Create the edges
  edges <- tbTransitions |>
    transmute(from=as.integer(src), to=as.integer(dst), label=rate) |>
    as.data.frame()
  resultVN <- visNetwork(nodes = nodes, edges = edges)
  resultVN
}

runVN2CNTest <- function() {
  # Setup VN
  sampleVN <- visNetwork(nodes = data.frame(id = 1:3, label = c("S", "I", "R")),
                         edges = data.frame(from = c(1, 2), to = c(2, 3), label = c("beta*I", "gamma")))
  
  # Test conversion to CN
  cm <- visNetworkToCM(sampleVN)
  # Update parms
  cm$parameters$beta <- 0.1
  cm$parameters$gamma <- 0.05
  # Update initial conditions
  cm$initialConditions['S'] <- 99
  cm$initialConditions['I'] <- 1
  cm$initialConditions['R'] <- 0
  
  # Check outputs from conversion
  print(cm)
  cat(makeRatesFunction(cm))
  cm |> plot()
}

# runVN2CNTest()
