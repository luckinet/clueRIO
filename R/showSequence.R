#' Graph of landuse conversion sequence
#'
#' @param self [`logical(1)`][logical]\cr whether or not to show edges for when
#'   landuse types can be converted to themselves.
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter select left_join
#' @importFrom visNetwork visNetwork visIgraphLayout visNodes visEdges
#'   visOptions visPhysics
#' @export

showSequence <- function(self = FALSE){

  opts <- getOption("lclu")

  edges <- opts$tables$allow
  nodes <- edges %>%
    mutate(id = seq_along(system)) %>%
    select(id, label = system)
  edges$system <- seq_along(edges$system)
  colnames(edges) <- c("system", seq_along(edges$system))
  edges <- edges %>%
    pivot_longer(-system) %>%
    filter(value != 0) %>%
    select(from = name, to = system)

  types <- opts$tables$allowTypes
  types$system <- seq_along(types$system)
  colnames(types) <- c("system", seq_along(types$system))
  types <- types %>%
    pivot_longer(-system) %>%
    select(from = name, to = system, label = value)

  edges <- left_join(edges, types, by = c("from", "to"))

  if(!self){
    edges <- edges %>%
      mutate(self = ifelse(from == to, TRUE, FALSE)) %>%
      filter(!self) %>%
      select(-self)
  }

  visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
    visIgraphLayout(layout = "layout_with_graphopt", smooth = TRUE) %>%
    visNodes(shape = "box", color = list(background = "white", border = "black")) %>%
    visEdges(arrows = 'from', color = "black", smooth = list(type = "continuous", roundness = .7)) %>%
    visOptions(highlightNearest = TRUE) %>%
    visPhysics(enabled = FALSE)

}



