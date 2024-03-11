library(tidyverse)
library(xml2)
library(htmltidy)

# Most basic HTML -> Text conversion
html2txt <- function(elContent, fixBar=T, fixSub=T) {
  # Bugfix: the overline \u0305 erroneously urlencodes to %CC%85 which then urldecodes to Ì…
  if (fixBar) elContent <- str_replace_all(elContent, URLdecode(URLencode("\u0305")), "\u0305")
  if (fixSub) elContent <- str_replace_all(elContent, "<sub>", "_")
  xml_text(read_html(paste0("<span>",elContent,"</span>")))
}

compartmentsToFactor <- function(compartments, sortFunctions=NA, isSorted=FALSE) {
  if (isSorted!=FALSE)
    sortFunctions <- list(function(x)TRUE, function(x)FALSE) # Always T & stable sort (assume radix)
  has <- function(y){function(x){str_detect(x,y)}}
  starts <- function(y){function(x){str_starts(x,y)}}
  default <- function(x){FALSE}
  sortFunctionsDefault <- list(
    starts('NullS'),
    starts('S'),
    starts('E'),
    starts('I'),
    default,
    starts('V'),
    starts('W'),
    starts('R'),
    starts('D'),
    starts('NullS')
  )
  if(all(is.na(sortFunctions))) {  # Overwrite NA default with the given default
    sortFunctions <- sortFunctionsDefault
  }
  defaultIndex <- which(sapply(sortFunctions,function(sf)all(all.equal(sf,default)==TRUE)))
  if (length(defaultIndex)==0) defaultIndex=1
  ordering <- compartments %>% sapply(function(compartment) {
    # find the first index for which the sortFunction returns true
    # use the defaultIndex otherwise
    validIndices <- sortFunctions %>% sapply(function(isValidIndex){isValidIndex(compartment)})
    # get the first valid index
    validIndex <- first(which(validIndices))
    ifelse(!is.na(validIndex), validIndex, defaultIndex)
  })
  levels <- ordering %>% sort %>% names %>% unique
  factor(compartments, levels=levels)
}

Drawio <- function(fname, tryResolveOrder=T) {
  contentRaw <- read_file(fname)
  contentXML <- as_xml_document(contentRaw)
  pagesXML <- xml_children(contentXML)
  pages <- lapply(pagesXML, function(pageXML) {
    # zlib <- reticulate::import("zlib")
    # encodedCompressedPageContent <- xml_text(xml_contents(pageXML))
    # compressedPageContent <- base64enc::base64decode(encodedCompressedPageContent)
    # pageContent <- URLdecode(zlib$decompress(compressedPageContent,-15L)$decode('utf8'))
    pageContent <- xml_contents(pageXML)[[1]]
    dx <- xml_attr(as_xml_document(pageContent), 'dx')
    dy <- xml_attr(as_xml_document(pageContent), 'dy')
    xmlElements <- xml_children(xml_children(as_xml_document(pageContent)))
    mxObjects <- lapply(xmlElements |> seq_along(), function(i) {
      xmlElement <- xmlElements[[i]]
      print(i)
      childAttr <- function(xmlElement,attr,default=NA){
        children <- xmlElement %>% xml_children
        if (length(children)==0) return(default)
        result <- children[[1]] %>% xml_attr(attr)
        if (length(result)==0 || is.na(result)) result <- default
        return(result)
      }
      tibble(
        name=xmlElement %>% xml_name,
        id=xmlElement %>% xml_attr("id"),
        value=xmlElement %>% xml_attr("value") %>% str_replace_all('`',''),
        source=xmlElement %>% xml_attr("source"),
        target=xmlElement %>% xml_attr("target"),
        style=xmlElement %>% xml_attr("style", default = ''),
        childStyle=xmlElement %>% childAttr("style", default = ''),
        x=xmlElement %>% childAttr("x",default=''),
        y=xmlElement %>% childAttr("y",default=''),
        width=xmlElement %>% childAttr("width",default=''),
        height=xmlElement %>% childAttr("height",default='')
      )
    }) |> bind_rows()
    guessedCompartments <- mxObjects |>
      filter(str_length(id)>5,is.na(source),value!="") |>
      transmute(id=id,
                compartmentName=value,
                compartmentNameSimple=compartmentName %>% sapply(html2txt),
                x,
                y) |>
      drop_na()
    guessedTransitions <- mxObjects |>
      filter(!is.na(source)|!is.na(target)) |>
      select(srcID=source,dstID=target,transitionLabel=value) |>
      left_join(guessedCompartments %>% transmute(srcID=id,src=compartmentNameSimple)) |>
      left_join(guessedCompartments %>% transmute(dstID=id,dst=compartmentNameSimple)) |>
      drop_na()
    compLevels = levels(compartmentsToFactor(unname(guessedCompartments$compartmentNameSimple), isSorted = !tryResolveOrder))
    mkparm <- function(src, dst) {
      # Helper function, given src/dst lists return a parm name for each transition
      sapply(seq_along(src), function(i) {
        paste0(src[[i]], dst[[i]]) |>
          str_to_lower() |>
          str_replace_all("\u0305","n") |>
          str_replace_all("_","") |>
          str_replace_all('nulls','mub_') |>
          str_replace_all('nulld','_mud')
      })
    }
    # Imbue guessed compartments/transitions with levels corresponding to guessed order on compartments
    guessedCompartments <- guessedCompartments %>% mutate(compartmentNameSimple=factor(compartmentNameSimple, levels=compLevels))
    guessedTransitions <- guessedTransitions %>% 
      # TODO: make the spacing nice before the equal sign.
      mutate(string=paste0(src,'->',dst,'   = ',mkparm(src,dst))) |>
      mutate(src=factor(src, levels=compLevels),
             dst=factor(dst, levels=compLevels))
    transitionsAsCF <- guessedTransitions |>
      arrange(src, dst) |>
      group_by(src) |>
      group_map(function(df1,df2){
        src <- df2$src
        paste0('#',df2$src,' ->\n', 
               paste0(df1$string, collapse='\n')
        )
      }) |>
      paste0(collapse='\n')
    # pull(string) %>% 
    # paste0(collapse = '\n')
    result <- list(dx=dx,
                   dy=dy,
                   compartments=guessedCompartments,
                   transitions=guessedTransitions,
                   transitionsAsCF=transitionsAsCF)
    
    return(result)
  })
  structure(list(
    fname = fname,
    pages = pages
  ),
  class = "drawio")
}
