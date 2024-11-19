library(testthat)
library(checkmate)

test_that("automatic change, 3 lus, 1 stable demand", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "pasture", "cropland")
#   commodities <- c("trees", "hey", "crops")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "3lus_1demand_1autoChange", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_3lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "pasture_suit", dir = data_dir, pattern = "suitPasture", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setSuitability(const = 1, cropland_suit = 1)
#   regLanduse(name = "pasture", resistance = 0.5) %>%
#     setConversion(to = "forest", auto = 10, label = "succession") %>%
#     setSuitability(const = 1, pasture_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "crops") %>%
#     setProduction(by = "cropland", amount = 1)
#   regCommodity(name = "trees") %>%
#     setProduction(by = "forest", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "example_region.gpkg")) %>%
#     mutate(region = "somewhere") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "somewhere", commodity = "crops",
#          grown = seq(from = 5845, to = 5845, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   showSequence()
#
#   # prepare input files ----
#   makeModel()
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   showAnimation(interval = 0.5)
})

test_that("automatic change and conversions, 3 lus, 1 increasing demand", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "pasture", "cropland")
#   commodities <- c("trees", "hey", "crops")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "3lus_1demand_conversion_1autoChange", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_3lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "pasture_suit", dir = data_dir, pattern = "suitPasture", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Entwaldung") %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setSuitability(const = 1, cropland_suit = 1)
#   regLanduse(name = "pasture", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Intensivierung") %>%
#     setConversion(to = "forest", auto = 10, label = "Sukkzession") %>%
#     setSuitability(const = 1, pasture_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "crops") %>%
#     setProduction(by = "cropland", amount = 1)
#   regCommodity(name = "trees") %>%
#     setProduction(by = "forest", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "example_region.gpkg")) %>%
#     mutate(region = "somewhere") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "somewhere", commodity = "crops",
#          grown = seq(from = 5845, to = 6500, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   showSequence()
#
#   # prepare input files ----
#   makeModel()
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   showAnimation(interval = 0.5)
})

test_that("several automatic change and conversions, 3 lus, 1 stable demand", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "pasture", "cropland")
#   commodities <- c("trees", "hey", "crops")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "3lus_1demand_conversion_2autoChange", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_3lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "pasture_suit", dir = data_dir, pattern = "suitPasture", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Entwaldung") %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setConversion(to = "pasture", auto = 5, label = "Feldwechsel") %>%
#     setSuitability(const = 1, cropland_suit = 1)
#   regLanduse(name = "pasture", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Intensivierung") %>%
#     setConversion(to = "forest", auto = 10, label = "Sukkzession") %>%
#     setSuitability(const = 1, pasture_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "crops") %>%
#     setProduction(by = "cropland", amount = 1)
#   regCommodity(name = "trees") %>%
#     setProduction(by = "forest", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "example_region.gpkg")) %>%
#     mutate(region = "somewhere") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "somewhere", commodity = "crops",
#          grown = seq(from = 5845, to = 5845, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   showSequence()
#
#   # prepare input files ----
#   makeModel()
#
#   # and finally run the model ----
#   runModel()
#
#   # make an animation ----
#   showAnimation(interval = 0.5)
})

