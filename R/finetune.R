#' Finetune a model
#'
#' @importFrom readr read_rds
#' @importFrom dplyr filter select slice mutate mutate_if bind_rows row_number
#'   vars
#' @importFrom tidyselect all_of
#' @importFrom stringr str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line facet_grid
#' @export

finetune <- function(){

  opts <- getOption(x = "lclu")
  meta <- read_rds(file = paste0(opts$path$clu, "/meta.rds"))

  thisDemand <- opts$data$demand %>%
    filter(year == min(meta$years)+1) %>%
    select(all_of(meta$commodities$demand)) %>%
    unlist(use.names = FALSE)

  log <- read_file(file = paste0(opts$path$clu, "/log.fil")) %>%
    str_split("\n") %>%
    unlist()
  yearSteps <- grep(pattern = "______   year [[:digit:]]  ______", x = log)
  yearSteps <- c(yearSteps, length(log))

  outLog <- NULL
  for(i in 1:(length(yearSteps)-1)){
    temp <- log[yearSteps[i]:(yearSteps[i+1]-1)] %>%
      as_tibble() %>%
      slice(-c(1:(2 + dim(meta$landuse)[1] + dim(meta$commodities)[1]*2 + 2))) %>%
      separate(col = 1, into = c("supply", "del", "deviation"), sep = "\t\\*\t") %>%
      select(-del) %>%
      separate(col = 2, into = c("mean_dev", "max_dev"), sep = "\t") %>%
      separate(col = 1, into = meta$commodities$demand, sep = "\t-\t") %>%
      separate(col = 1, into = c("del", meta$commodities$demand[1]), sep = "\t ") %>%
      select(-del) %>%
      separate(col = length(meta$commodities$demand), into = meta$commodities$demand[length(meta$commodities$demand)], sep = "\t") %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(year = i)

    outLog <- bind_rows(outLog, temp)
  }


  supply <- outLog %>%
    select(-mean_dev, -max_dev, -year)
  deviation <- as_tibble((supply - thisDemand) / thisDemand)
  deviation <- deviation %>%
    mutate(year = outLog$year) %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(-c(iteration, year), names_to = "commodity", values_to = "deviation")

  metrics <- outLog %>%
    select(mean_dev, max_dev) %>%
    mutate(year = outLog$year) %>%
    mutate(iteration = row_number()) %>%
    pivot_longer(-c(iteration, year), names_to = "metric", values_to = "deviation")


  deviation %>%
    # filter(iteration < 1000) %>%
    filter(year == 1) %>%
    ggplot(aes(x = iteration)) +
    geom_line(aes(y = deviation)) +
    geom_line(data = metrics %>% filter(metric == "max"), aes(y = deviation), colour = "red") +
    # ylim(c(-1, 4000)) +
    facet_grid(vars(commodity))


  # Ich öffne die File dann meistens in Excel (mit Drag & Drop). Dann rechnest
  # du (supply – demand)/demand. Dann hast du die relative Abweichung für jeden
  # demand type.
  #
  # Als nächstes kannst du dir die Abweichungen entweder in einem line chart
  # anschauen oder du benutzt conditional formatting und makierst alle die, die
  # über deiner maximum deviation sind. Im line chart sieht man, ob ein demand
  # immer mal wieder “springt”. Dann ist es gut in der lusconv die land systems
  # die den demand produzieren auf -1 zu setzen. Mit dem Conditional Formatting
  # siehst du, ob du in der lusconv die land systeme für eine commodity mehr
  # pushen musst.

}


