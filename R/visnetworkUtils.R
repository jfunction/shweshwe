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

## VN2DrawIO
xmlCompartment <- function(tbRow, tbOther) {
  with(as.list(tbRow, tbOther), {
    glue::glue('
        <mxCell id="{compartmentsUUID}-{compartmentIndex}" value="`{compartment}`" style="whiteSpace=wrap;html=1;" parent="1" vertex="1">
            <mxGeometry x="{x}" y="{y}" width="40" height="40" as="geometry" />
        </mxCell>')
  })
}

xmlTransition <- function(tbRow, tbOther) {
  with(as.list(tbRow, tbOther),{
    glue::glue('
          <mxCell id="{transitionsUUID}-{transitionIndex}" value="{label}" style="rounded=0;orthogonalLoop=1;jettySize=auto;html=1;" parent="1" source="{compartmentsUUID}-{src.ID}" target="{compartmentsUUID}-{dst.ID}" edge="1">
            <mxGeometry relative="1" as="geometry" />
          </mxCell>')
  })
}

xmlDiagram <- function(xmlCompartmentsTxt, xmlTransitionsTxt) {
  with(list(xmlCompartmentsTxt = xmlCompartmentsTxt, xmlTransitionsTxt = xmlTransitionsTxt), {
    glue::glue('
        <mxfile host="app.diagrams.net" modified="2023-08-25T12:01:46.013Z" agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36 Edg/116.0.1938.54" etag="BJcitGzL7aEb84nDGWfy" version="21.6.9" type="device">
  <diagram id="Dpav7CHE1rxV_vT-ojmE" name="Page-1">
    <mxGraphModel dx="372" dy="378" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="850" pageHeight="1100" math="1" shadow="0">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />
		{xmlCompartmentsTxt}
		{xmlTransitionsTxt}
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>')
  })
}

visNetwork2Drawio <- function(vn, filename = "diagram.drawio") {
  graphNodes <- vn$x$nodes # id,label,(x,y)
  missingNodeCols <- c("id","label","x","y") |> setdiff(colnames(graphNodes))
  if(length(missingNodeCols)!=0) {
    stop(paste0("Error making Drawio from visnetwork. Expected node columns id,label,x,y but got ",paste0(missingNodeCols,collapse=",")))
  }
  graphEdges <- vn$x$edges # from,to,(label)
  missingEdgeCols <- c("from","to","label") |> setdiff(colnames(graphEdges))
  if (any(missingEdgeCols %in% c("label"))) {
    warning("The edges data.frame should have a column `label`. If not, the labels will all be empty.")
    graphEdges <- graphEdges |> mutate(label="")
  } else if(length(missingEdgeCols)!=0) {
    stop("Error making Drawio from visnetwork. Expected edge columns from,to,label but got ",paste0(missingEdgeCols,collapse=","))
  }
  
  compartmentsUUID = '_PVGUrJauaZoBftJVT4O'
  transitionsUUID = 'QGI5SdNPsL6WamnX0T07'
  
  tbCompartments <- graphNodes |>
    transmute(ID=id,
              compartment=label,
              x=x-min(x)+50,
              y=y-min(y)+50) |>
    as_tibble()
  
  tbTransitions <- graphEdges |>
    rowid_to_column('transitionID') |>
    rename(src.ID=from, dst.ID=to) |>
    left_join(tbCompartments |> select(src.ID=ID, src.x=x, src.y=y), by=join_by("src.ID")) |>
    left_join(tbCompartments |> rename(dst.ID=ID, dst.x=x, dst.y=y), by=join_by("dst.ID"))
  
  xmlTransitionsTxt <- tbTransitions |>
    rowid_to_column(var='transitionIndex') |>
    mutate(compartmentsUUID=compartmentsUUID, transitionsUUID=transitionsUUID) |> 
    rowwise() |>
    group_map(xmlTransition) |>
    paste0(collapse='\n')
  
  xmlCompartmentsTxt <- tbCompartments |>
    rowid_to_column(var='compartmentIndex') |>
    mutate(compartmentsUUID=compartmentsUUID, transitionsUUID=transitionsUUID) |>
    rowwise() |>
    group_map(xmlCompartment) |>
    paste0(collapse='\n')
  
  # Now we need to use these to construct the drawio file
  drawioTxt <- xmlDiagram(xmlCompartmentsTxt, xmlTransitionsTxt)
  drawioTxt
}
