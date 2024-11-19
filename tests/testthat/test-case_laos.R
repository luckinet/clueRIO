library(testthat)
library(checkmate)

test_that("laos minimal example works", {
#
#   # define metadata ----
#   # dir <- tempdir()
#   # dir <- "/home/se87kuhe/idiv-mount/groups/MAS/01_data/LUCKINet/allocation"
#   dir <- "/media/se87kuhe/external/r-dev/luckiCLU_misc/testing"
#   data_dir <- paste0(system.file("test_datasets", package = "luckiCLU", mustWork = TRUE), "/")
#
#   lus <- c("Rock", "Water", "Urban", "Mining concessions", "Tree plantation concessions",
#            "Arable plantation concessions", "Closed forest", "Shifting cultivation",
#            "Permanent cultivation", "Forest - SC mosaic", "Forest - permanent mosaic")
#   commodities <- c("Built-up", "Staple crop", "Arable cash crop", "Tree cash crop")
#   targetCRS <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
#
#   # start project ----
#   startProject(root = dir, name = "laos", start = 2010, end = 2020,
#                landuse = lus, demands = commodities)
#
#   # register gridded objects ----
#   regGridded(name = "landuse", dir = data_dir, pattern = "cov_all.0", crs = targetCRS)
#   tibble(system = lus) %>%
#     mutate(ID = seq_along(system)-1) %>%
#     setLAT()
#
#   # then, set the regular drivers (doesn't need RAT)
#   regGridded(name = "w_3_ab2k", dir = data_dir, pattern = "gr0.fil", crs = targetCRS)
#   regGridded(name = "access", dir = data_dir, pattern = "gr1.fil", crs = targetCRS)
#   regGridded(name = "access_dom", dir = data_dir, pattern = "gr2.fil", crs = targetCRS)
#   regGridded(name = "access_int", dir = data_dir, pattern = "gr3.fil", crs = targetCRS)
#   regGridded(name = "contractp", dir = data_dir, pattern = "gr4.fil", crs = targetCRS)
#   regGridded(name = "popdensity", dir = data_dir, pattern = "gr5.fil", crs = targetCRS)
#   regGridded(name = "slope", dir = data_dir, pattern = "gr6.fil", crs = targetCRS)
#   regGridded(name = "elevation", dir = data_dir, pattern = "gr7.fil", crs = targetCRS)
#   regGridded(name = "precipitation", dir = data_dir, pattern = "gr8.fil", crs = targetCRS)
#   regGridded(name = "temperature", dir = data_dir, pattern = "gr9.fil", crs = targetCRS)
#   regGridded(name = "awc_1", dir = data_dir, pattern = "gr10.fil", crs = targetCRS)
#   regGridded(name = "awc_4", dir = data_dir, pattern = "gr11.fil", crs = targetCRS)
#   regGridded(name = "awc_5", dir = data_dir, pattern = "gr12.fil", crs = targetCRS)
#   regGridded(name = "drain4", dir = data_dir, pattern = "gr13.fil", crs = targetCRS)
#   regGridded(name = "t_clay", dir = data_dir, pattern = "gr14.fil", crs = targetCRS)
#   regGridded(name = "s_clay", dir = data_dir, pattern = "gr15.fil", crs = targetCRS)
#   regGridded(name = "t_gravel", dir = data_dir, pattern = "gr16.fil", crs = targetCRS)
#   regGridded(name = "toc_4", dir = data_dir, pattern = "gr17.fil", crs = targetCRS)
#   regGridded(name = "w_2_un2k", dir = data_dir, pattern = "gr18.fil", crs = targetCRS)
#
#   # register landuse systems ----
#   regLanduse(name = "Rock", resistance = 1)
#   regLanduse(name = "Water", resistance = 1)
#   regLanduse(name = "Urban", resistance = 1)
#   regLanduse(name = "Mining concessions", resistance = 1)
#   regLanduse(name = "Tree plantation concessions", resistance = 0.8) %>%
#     setConversion(to = "Urban", label = "urbanisation")
#   regLanduse(name = "Arable plantation concessions", resistance = 0.6) %>%
#     setConversion(to = "Urban", label = "urbanisation")
#   regLanduse(name = "Closed forest", resistance = 0.2) %>%
#     setConversion(to = "Urban", label = "urbanisation") %>%
#     setConversion(to = "Tree plantation concessions", label = "intensification") %>%
#     setConversion(to = "Arable plantation concessions", label = "intensification") %>%
#     setConversion(to = "Shifting cultivation", label = "intensification") %>%
#     setConversion(to = "Permanent cultivation", label = "intensification") %>%
#     setConversion(to = "Forest - SC mosaic", label = "intensification") %>%
#     setConversion(to = "Forest - permanent mosaic", label = "intensification")
#   regLanduse(name = "Shifting cultivation", resistance = 0.2) %>%
#     setConversion(to = "Urban", label = "urbanisation") %>%
#     setConversion(to = "Tree plantation concessions", label = "intensification") %>%
#     setConversion(to = "Arable plantation concessions", label = "intensification") %>%
#     setConversion(to = "Closed forest", label = "abandonment")
#   regLanduse(name = "Permanent cultivation", resistance = 0.45) %>%
#     setConversion(to = "Urban", label = "urbanisation") %>%
#     setConversion(to = "Tree plantation concessions", label = "intensification") %>%
#     setConversion(to = "Arable plantation concessions", label = "intensification") %>%
#     setConversion(to = "Closed forest", label = "extensification") %>%
#     setConversion(to = "Forest - permanent mosaic", label = "extensification")
#   regLanduse(name = "Forest - SC mosaic", resistance = 0.2) %>%
#     setConversion(to = "Urban", label = "urbanisation") %>%
#     setConversion(to = "Tree plantation concessions", label = "intensification") %>%
#     setConversion(to = "Arable plantation concessions", label = "intensification") %>%
#     setConversion(to = "Closed forest", label = "extensification") %>%
#     setConversion(to = "Shifting cultivation", label = "expansion")
#   regLanduse(name = "Forest - permanent mosaic", resistance = 0.2) %>%
#     setConversion(to = "Urban", label = "urbanisation") %>%
#     setConversion(to = "Tree plantation concessions", label = "intensification") %>%
#     setConversion(to = "Arable plantation concessions", label = "intensification") %>%
#     setConversion(to = "Closed forest", label = "extensification") %>%
#     setConversion(to = "Permanent cultivation", label = "intensification")
#
#   # set suitability ----
#   read_csv(file = paste0(data_dir, "params.csv")) %>%
#     setSuitability(table = .)
#
#   # register demand types ----
#   regCommodity(name = "Built-up") %>%
#     setProduction(by = "Urban", amount = 400)
#   regCommodity(name = "Staple crop") %>%
#     setProduction(by = "Permanent cultivation", amount = 2.72) %>%
#     setProduction(by = "Shifting cultivation", amount = 2.08) %>%
#     setProduction(by = c("Forest - SC mosaic", "Forest - permanent mosaic"), amount = c(0.80, 1.44))
#   regCommodity(name = "Arable cash crop") %>%
#     setProduction(by = "Arable plantation concessions", amount = 6.40)
#   regCommodity(name = "Tree cash crop") %>%
#     setProduction(by = "Tree plantation concessions", amount = 6.40)
#
#   # register regions ----
#   read_rds(path = paste0(data_dir, "gadm36_LAO_0_sf.rds")) %>%
#     mutate(region = tolower(NAME_0)) %>%
#     dplyr::select(region, geometry, -GID_0, -NAME_0) %>%
#     regRegions(region = "region")
#
#   # register demand ----
#   read_csv(file = paste0(data_dir, "demands.csv")) %>%
#     dplyr::select(year, commodity = type, region, values = demand) %>%
#     regDemand(values = "values", region = "region")
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
