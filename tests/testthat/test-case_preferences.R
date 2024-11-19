library(testthat)
library(checkmate)

test_that("location specific preferences, 2 lus, 1 increasing demand", {
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
#   startProject(root = dir, name = "2lus_1demand_locSpec", start = 2000, end = 2019,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   LUS <- tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1)
#
#   regGridded(name = "landuse", dir = data_dir, pattern = "ls_2lus", crs = targetCRS)
#   setLAT(table = LUS)
#   regGridded(name = "cropland_suit", dir = data_dir, pattern = "suitCropland", crs = targetCRS)
#   regGridded(name = "forest_suit", dir = data_dir, pattern = "suitForest_year0", crs = targetCRS)
#   regGridded(name = "pref_wald", dir = data_dir, pattern = "preferences1", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "forest", resistance = 0.5) %>%
#     setSuitability(const = 1, forest_suit = 1) %>%
#     setPreference(layer = "pref_wald", weight = 1) %>%
#     setConversion(to = "cropland", label = "Intensivierung")
#   regLanduse(name = "cropland", resistance = 0.5) %>%
#     setSuitability(const = 1, cropland_suit = 1) %>%
#     setPreference()
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
#          grown = seq(from = 4760, to = 5000, length.out = 20)) %>%
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
