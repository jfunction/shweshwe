# S3 representation of a compartmental model

# Low-level functions ####
new_CompartmentalModel <- function(tbTransitions=tibble(src=factor(), dst=factor(), rate=character()),
                                   parameters=list(),
                                   initialConditions=double(),
                                   timesteps=double()) {
  # Input validation
  stopifnot(is_tibble(tbTransitions))
  stopifnot(is_list(parameters))
  stopifnot(is.double(initialConditions))
  stopifnot(is.double(timesteps))
  
  # Create the model object
  structure(list(
    tbTransitions=tbTransitions,
    parameters=parameters,
    initialConditions=initialConditions,
    timesteps=timesteps
  ), class = "CompartmentalModel")
}

validate_CompartmentalModel <- function(x) {
  # Transitions should have columns `src`, `dst`, and `rate`
  tbTransitionColNamesValid <- all.equal(sort(colnames(x$tbTransitions)), sort(c("src", "dst", "rate"))) == TRUE
  stopifnot(tbTransitionColNamesValid)
  
  # Transition src and dst columns should be factors with the same levels, representing the compartments
  compartmentNamesSrc <- levels(x$tbTransitions$src)
  compartmentNamesDst <- levels(x$tbTransitions$dst)
  stopifnot(all.equal(compartmentNamesSrc, compartmentNamesDst) == TRUE)
  compartmentNames <- compartmentNamesSrc

  # Initial conditions should have the same names as the compartments
  compartmentNamesIC <- names(x$initialConditions)
  stopifnot(all.equal(compartmentNames, compartmentNamesIC) == TRUE)
  
  x
}

# Constructor and Generics ####
CompartmentalModel <- function(tbTransitions=tibble(src=factor(c("S","E","I"),c("S","E","I","R")),
                                                    dst=factor(c("E","I","R"),c("S","E","I","R")),
                                                    rate=c("beta*I", "sigma", "gamma")),
                               parameters=list(beta=1e-3, sigma=1/2, gamma=1/10),
                               initialConditions=c(S=100, E=1, I=0, R=0),
                               timesteps=seq(0, 365, 1)) {
  # Create the model object
  model <- new_CompartmentalModel(tbTransitions, parameters, initialConditions, timesteps)
  # Validate and return the model object
  validate_CompartmentalModel(model)
}

print.CompartmentalModel <- function(x, ...) {
  numCompartments <- length(x$initialConditions)
  numTransitions <- nrow(x$tbTransitions)
  numParameters <- length(x$parameters)
  timeRange <- range(x$timesteps)
  result <- paste0("CompartmentalModel with ",
                   numCompartments, " compartments, ",
                   numTransitions, " transitions, ",
                   numParameters, " parameters, ",
                   "and time range [", timeRange[1], ", ", timeRange[2],
                   "] in " , length(x$timesteps), " timesteps\n")
  print(result, ...)
}

as_tibble.CompartmentalModel <- function(x, ...) {
  y0 <- x$initialConditions
  times <- x$timesteps
  rates <- eval(parse(text=makeRatesFunction(x)))
  parms <- x$parameters
  out <- deSolve::ode(y = y0, times = times, func = rates, parms = parms)
  tibble::as_tibble(as.data.frame(out))
}

plot.CompartmentalModel <- function(x, ...) {
  out <- as_tibble(x) |>
    tidyr::pivot_longer(cols = -time, names_to = "compartment", values_to = "population") |>
    dplyr::mutate(compartment = factor(compartment, levels=names(x$initialConditions)))
  ggplot2::ggplot(out) +
    ggplot2::aes(x = time, y = population, color = compartment) +
    ggplot2::geom_line() +
    labs(title = "Compartmental Model",
         color = "Compartment",
         x = "Time",
         y = "Population")
}

# Higher level functions ####
makeRatesFunction <- function(cm, indent=2) {
  # TODO: Add support for multipatch models, transition setup, etc
  result <- "function(t, y, parms) {\n"
  spaces <- paste0(rep(' ', indent),collapse='')
  result <- paste0(result, spaces, "with(as.list(c(y, parms)), {\n")
  # Now we need to get the rates code, dS, dI, dR, etc.
  # Using the tbTransitions from cm
  tbTransitions <- cm$tbTransitions
  compartmentNames <- levels(tbTransitions$src)
  rateEqnsTxt <- map_chr(compartmentNames, function(compartment) {
    tbTransitionsIn <- tbTransitions %>% filter(dst == compartment)
    tbTransitionsOut <- tbTransitions %>% filter(src == compartment)
    # 'dS <- '
    line <- paste0(spaces, spaces, 'd', compartment, ' <- ')
    # First record the rates that are coming in, then subtract rates leaving
    hasIn <- nrow(tbTransitionsIn) > 0
    hasOut <- nrow(tbTransitionsOut) > 0
    if (hasIn) {
      lineRatesIn <- paste0(tbTransitionsIn$rate, '*', tbTransitionsIn$src, collapse=' + ')
      line <- paste0(line, lineRatesIn)
    }
    if (hasOut) {
      lineRatesOut <- paste0(tbTransitionsOut$rate, collapse=' + ')
      if (hasIn) {
        line <- paste0(line, ' ')
      }
      if (nrow(tbTransitionsOut)==1) {
        line <- paste0(line, '- ', lineRatesOut, '*', compartment)
      } else {
        line <- paste0(line, '- (', lineRatesOut, ')*', compartment)
      }
    }
    line <- paste0(line, '\n')
    line
  })
  result <- paste0(result, paste0(rateEqnsTxt, collapse=''))
  result <- paste0(result, spaces, spaces,
                   "return(list(c(",
                   paste0('d', compartmentNames, collapse=', '),
                   ")))\n")
  # Close the with()
  result <- paste0(result, spaces, "})\n")
  # Close the function
  result <- paste0(result, "}\n")
  result
}

runCMTests <- function() {
  cm <- CompartmentalModel()
  print(cm)
  print("Rates function:")
  cat(makeRatesFunction(cm))
  plot(cm)
}

# runCMTests()

