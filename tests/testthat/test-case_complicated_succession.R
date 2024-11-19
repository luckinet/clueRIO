library(testthat)
library(checkmate)

test_that("minimal examle", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("primary forest", "secondary forest", "grass", "shifting cultivation", "permanent agriculture") # note that neither the order of LUS, drivers nor suitabilities matters, it's matched properly
#   commodities <- c("Bäume", "Heu", "Feldfrüchte")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "sys1_minimal", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_sys1", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "shiftAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "permAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "wiese_suit", dir = data_dir, pattern = "suitWiese", crs = targetCRS)
#   regGridded(name = "primFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#   regGridded(name = "secFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "primary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, primFor_suit = 1) %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "secondary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, secFor_suit = 1) %>%
#     setConversion(to = "primary forest", label = "regrowth") %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "grass", resistance = 0.5) %>%
#     setSuitability(const = 1, wiese_suit = 1) %>%
#     setConversion(to = "secondary forest", label = "regrowth")
#
#   regLanduse(name = "shifting cultivation", resistance = 0.5) %>%
#     setSuitability(const = 1, shiftAgr_suit = 1) %>%
#     setConversion(to = "grass", label = "abandonment") %>%
#     setConversion(to = "permanent agriculture")
#
#   regLanduse(name = "permanent agriculture", resistance = 0.5) %>%
#     setSuitability(const = 1, permAgr_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "Feldfrüchte") %>%
#     setProduction(by = "permanent agriculture", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "region_wuf.gpkg")) %>%
#     mutate(region = "Beispielhausen") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "Beispielhausen", commodity = "Feldfrüchte",
#          grown = seq(from = 1093, to = 1200, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   makeSequence()
#
#   # prepare input files ----
#   makeModel()
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   makeAnimation()
})

test_that("including natural succession", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("primary forest", "secondary forest", "grass", "shifting cultivation", "permanent agriculture") # note that neither the order of LUS, drivers nor suitabilities matters, it's matched properly
#   commodities <- c("Bäume", "Heu", "Feldfrüchte")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "sys1_succession", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_sys1", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "shiftAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "permAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "wiese_suit", dir = data_dir, pattern = "suitWiese", crs = targetCRS)
#   regGridded(name = "primFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#   regGridded(name = "secFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "primary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, primFor_suit = 1) %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation", label = "agricultural expansion") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "secondary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, secFor_suit = 1) %>%
#     setConversion(to = "primary forest", auto = 15, label = "regrowth") %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation", label = "agricultural expansion") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "grass", resistance = 0.5) %>%
#     setSuitability(const = 1, wiese_suit = 1) %>%
#     setConversion(to = "secondary forest", auto = 10, label = "regrowth")
#
#   regLanduse(name = "shifting cultivation", resistance = 0.5) %>%
#     setSuitability(const = 1, shiftAgr_suit = 1) %>%
#     setConversion(to = "grass", label = "abandonment") %>%
#     setConversion(to = "permanent agriculture")
#
#   regLanduse(name = "permanent agriculture", resistance = 0.5) %>%
#     setSuitability(const = 1, permAgr_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "Feldfrüchte") %>%
#     setProduction(by = "permanent agriculture", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "region_wuf.gpkg")) %>%
#     mutate(region = "Beispielhausen") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "Beispielhausen", commodity = "Feldfrüchte",
#          grown = seq(from = 1093, to = 1200, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   makeSequence()
#
#   # prepare input files ----
#   makeModel()
#   setOptions(ls_hist = c(1, 10))
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   makeAnimation()
})

test_that("including natural succession and two demands", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("primary forest", "secondary forest", "grass", "shifting cultivation", "permanent agriculture") # note that neither the order of LUS, drivers nor suitabilities matters, it's matched properly
#   commodities <- c("Bäume", "Heu", "Feldfrüchte")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "sys1_severalComm", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_sys1", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "shiftAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "permAgr_suit", dir = data_dir, pattern = "suitFeld", crs = targetCRS)
#   regGridded(name = "wiese_suit", dir = data_dir, pattern = "suitWiese", crs = targetCRS)
#   regGridded(name = "primFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#   regGridded(name = "secFor_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "primary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, primFor_suit = 1) %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation", label = "agricultural expansion") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "secondary forest", resistance = 0.5) %>%
#     setSuitability(const = 1, secFor_suit = 1) %>%
#     setConversion(to = "primary forest", auto = 15, label = "regrowth") %>%
#     setConversion(to = "grass", label = "deforestation") %>%
#     setConversion(to = "shifting cultivation", label = "agricultural expansion") %>%
#     setConversion(to = "permanent agriculture", label = "agricultural expansion")
#
#   regLanduse(name = "grass", resistance = 0.5) %>%
#     setSuitability(const = 1, wiese_suit = 1) %>%
#     setConversion(to = "secondary forest", auto = 10, label = "regrowth")
#
#   regLanduse(name = "shifting cultivation", resistance = 0.5) %>%
#     setSuitability(const = 1, shiftAgr_suit = 1) %>%
#     setConversion(to = "grass", label = "abandonment") %>%
#     setConversion(to = "permanent agriculture")
#
#   regLanduse(name = "permanent agriculture", resistance = 0.5) %>%
#     setSuitability(const = 1, permAgr_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "Feldfrüchte") %>%
#     setProduction(by = "permanent agriculture", amount = 1)
#   regCommodity(name = "Heu") %>%
#     setProduction(by = "grass", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "region_wuf.gpkg")) %>%
#     mutate(region = "Beispielhausen") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   comm1 <- tibble(commodity = "Feldfrüchte", grown = seq(from = 1093, to = 1200, length.out = 20))
#   comm2 <- tibble(commodity = "Heu", grown = seq(from = 1851, to = 1600, length.out = 20))
#
#   comm1 %>%
#     bind_rows(comm2) %>%
#     add_column(region = "Beispielhausen") %>%
#     add_column(year = rep(c(2000:2019), times = 2)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   makeSequence()
#
#   # prepare input files ----
#   makeModel()
#   setOptions(conv1 = 0.45, conv2 = 0.7, ls_hist = c(1, 10))
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   makeAnimation()
})
