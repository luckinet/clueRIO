library(testthat)
library(checkmate)

test_that("'wald und feld' where demand of crops implies a decrease but no conversion rule is set", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "cropland")
#   commodities <- c("crops", "trees")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "wuf_fail", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_2lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Intensivierung") %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setSuitability(const = 1, cropland_suit = 1)
#
#   # register demand types ----
#   regCommodity(name = "crops") %>%
#     setProduction(by = "cropland", amount = 1)
#
#   # register regions ----
#   st_read(dsn = paste0(data_dir, "example_region.gpkg")) %>%
#     mutate(region = "somewhere") %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   tibble(year = c(2000:2019), region = "somewhere", commodity = "crops",
#          grown = seq(from = 4760, to = 4000, length.out = 20)) %>%
#     regDemand(values = "grown", region = "region")
#
#   # prepare input files ----
#   makeModel()
})

test_that("'wald und feld' where feld can be turned into forest again, but there is too much demand", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "cropland")
#   commodities <- c("crops", "trees")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "wuf_cycle", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_2lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Intensivierung") %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setConversion(to = "forest", label = "Aufforstung") %>%
#     setSuitability(const = 1, cropland_suit = 1)
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
#   tibble(year = rep(c(2000:2019), times = 2),
#          region = "somewhere",
#          commodity = rep(c("crops", "trees"), each = 20),
#          grown = c(seq(from = 4760, to = 5000, length.out = 10), seq(from = 5000, to = 5500, length.out = 10),
#                    seq(from = 5240, to = 5000, length.out = 10), seq(from = 5000, to = 5240, length.out = 10))) %>%
#     regDemand(values = "grown", region = "region")
#
#   # prepare input files ----
#   makeModel()
})

test_that("'wald und feld' where feld can only remain x years", {
  # this should probably give an error!?
})

test_that("where 'trees' increases, but not conversion rule for its' landuse types is set", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing/"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("forest", "pasture", "cropland") # note that neither the order of LUS, drivers nor suitabilities matters, it's matched properly
#   commodities <- c("trees", "hey", "crops")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "wwf_cycle", start = 2000, end = 2019,
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
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Entwaldung") %>%
#     setSuitability(const = 1, forest_suit = 1)
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setSuitability(const = 1, cropland_suit = 1)
#   regLanduse(name = "pasture", resistance = 0.5) %>%
#     setConversion(to = "cropland", label = "Intensivierung") %>%
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
#   tibble(year = rep(c(2000:2019), times = 2),
#          region = "somewhere",
#          commodity = rep(c("crops", "trees"), each = 20),
#          grown = c(seq(from = 5845, to = 6300, length.out = 10),
#                    seq(from = 6300, to = 5845, length.out = 10),
#                    seq(from = 2478, to = 3000, length.out = 10),
#                    seq(from = 3000, to = 2478, length.out = 10))) %>%
#     regDemand(values = "grown", region = "region")
#
#   # check out the landuse change sequence ----
#   showSequence()
#
#   # prepare input files ----
#   makeModel()
})