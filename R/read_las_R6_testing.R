
# a <- c("C:/Users/giorgfr/OneDrive - Occidental Petroleum Corporation/R_Projects/rpetophy/Data/lasfiles2/CIRA-1880.las")
# sub(".*\\/", "", a)




# library(petroreadr)
# library(dplyr)
#
# path2data <- "C:/Users/giorgfr/OneDrive - Occidental Petroleum Corporation/R_Projects/rpetophy/Data/lasfiles_test01"
# # path2data <- "~/OneDrive - Oxy/R_Projects/rpetophy/Data/lasfiles_test01"
# lasfiles <- list.files(path2data)
#
# # file.path(path2data, "C-2041.las")
# # file.path(path2data, c("C-2041.las", "C-2041XX.las"))
#
# aax <- read_las(file.path(path2data, "C-2041.las"))
# aax0 <- read_las0(file.path(path2data, "C-2041.las"), lasnull = "-999.2500")
# #
# # # microbenchmark::microbenchmark(read_las(file.path(path2data, "C-2041.las")),
# # #                                read_las0(file.path(path2data, "C-2041.las"), lasnull = "-999.2500"))
# # #
# # # Unit: milliseconds
# # # expr           min        lq      mean    median        uq      max neval
# # # read_las  148.5160 151.87466 169.03078 171.49145 174.28046 224.4209   100
# # # read_las0  69.5508  72.05422  82.22599  78.92377  83.23245 363.7202   100
#
# # dim(aax0)
# # dim(aax)
# #
# # summary(aax$data)
# # summary(aax0)
# #
# # head(aax)
# # head(aax0)
# #
# aaxm <- read_las(file.path(path2data, c("C-2041.las","C-2042.las")))
#
# head(aaxm)
# unique(aaxm$data$WELL)
# aaxm$header
# aaxm$version
# # #
# # #
# magrittr::equals(aax$data, aax0)
#
# #
# as_tibble(aax$header)
# colnames(aax$header)
#
# summary(aax)
#
# colnames(aax0)
# colnames(aax$data)
# aax$colnames()
#
# aax$set_header("CIRA2041")
# aax$header
# head(aax)
#
# aax$set_header("WELL2041")
# aax$header
# head(aax)

#
# aax$set_header("LCI", item = "LOC", descr = "Location of the field")
#
#
# aax$set_header(as.character("LCI"), item = "CNTY", units = NA, descr = "Location of the field")
# aax$header
#
# aax$set_header("0.2", item = "NEW", units = "mD", descr = "prueba nuevo")
# aax$header
#
# aax$set_header(0.5, item = "STEP", units = "M")
#
# # ### header as list ??????
# bb <- aax$header
#
#
# bb2 <- bb %>%
#   # mutate(id = row_number()) %>%
#   tidyr::gather(var, val) %>%
#   group_by(var) %>%
#   summarize(x = list(val))
#
# bb2[bb2$var == "MNEM", 2]$x[[1]][2]


#
#
# # fields
# aax$header
# aax$version
# aax$tracks
# head(aax$data)
#
# # methods
# aax
# print(aax)
#
# summary(aax)
# aax$summary()
#
#
# ## works as it is a data frame
# head(aax)
# head(aax, 2L)
#
# tail(aax)
# tail(aax, 8L)
#
# dim(aax)
# aax$dim()

#
# # filter modify header?
# library(dplyr)
# aax$data <- aax$data %>%
#   filter(DEPTH > 2000)
#
# aax$header
# dim(aax)
#
# aax
# aax$data <- aax$data %>%
#   select(-XCAL)
#
# aax
# aax$tracks
#
# aax$data <- aax$data %>%
#   mutate(XCAL2 = 10)
# aax
# aax$tracks
#
# # change order
# aax$data <- aax$data %>%
#   select(WELL, DEPTH, XGR, XSP)
# aax$tracks
# head(aax)
#
# aax$data <- aax$data %>%
#   select(WELL, DEPTH, XSP, XGR)
# aax$tracks
# head(aax)
# aax$header
#
#
# df2 <- aax$data %>%
#   select(WELL, DEPTH, XGR, XSP)
# class(df2)
#
# df2 <- as.las(df2)
# class(df2)
# df2$header
# df2$version
# df2$tracks
# df2
#
#
# aax$track <- rbind(aax$track,
#                    data.frame(MNEM = "AAA", UNIT = "FT", APICODE = "UNKNOWN", CDESCR = "aaaaaaaaaaa"))
#
# aax$track
# head(aax)
#
# aax$header[aax$header$MNEM == "WELL", 3]
# aax$header[aax$header$MNEM == "WELL", 3] <- "CIRA2041"
#
# ## combine las objects
# aax_facies <- read_las(file.path(path2data, "Facies", "CIRA-2041.las"))
# aax_facies_2 <- read_las(file.path(path2data, "dummy", "CIRA-2041.las"))
#
# # cbind(aax, aax_facies)
# # cbind(aax, aax_facies, aax_facies_2)
#
# # aax$combine_tracks(aax_facies)
# # aax
# #
# # aax$combine_tracks(aax_facies_2)
# # aax
#
#
#
# # aax_comb <- aax$tracks_combine(aax_facies)
# #
# # aax_comb <- tracks_combine(aax, aax_facies)
# # aax_comb <- cbind(aax, aax_facies) #aax_facies_2)
#
#
# # header_info
#
#
# aax$header
# aax$tracks
# head(aax$data)



## read_lasoo()
## when playing with data as dataframe, use as.data.frame(aa$data)
## but to convert it back to las.obj, all tracks needs to have curves information
## as_las_obj(data, header, tracks)

# header breakdown
# 1st dot
# 1st space after the dot
# last colon
# stringr::word(gsub("\\s+", " ", stringr::str_trim(header_info[1])), 1)
# aa <- stringr::str_squish(header_info[1])
# bb <- "STRT .FT 100.0000 :START. DEPTH"
#
# units_index <- grep("\\.", stringr::word(aa, 1:10))[1]
# value_index <- grep("\\.", stringr::word(aa, 1:10))[1]
# descr_index <- grep("\\.", stringr::word(aa, 1:10))[1]
#
# MNEM <- stringr::str_squish(sub("\\..*", "", header_info[1]))
# UNITS <- sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1])))
# VALUE <- stringr::str_trim(sub(":.*", "", sub(UNITS, "", sub("\\.", "", sub(sub("\\..*", "", header_info[7]), "", header_info[7])))))
# DESCR <- stringr::str_trim(sub(".*:", "", header_info[1]))
#
#
# x <- sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))
# stringr::str_trim(sub(":.*", "", sub(UNITS, "", sub("\\.", "", sub(sub("\\..*", "", header_info[7]), "", header_info[7])))))
#
#
# for(i in 1:length(header_info)) {
#   if(i == 1) {
#     x <- data.frame(MNEM = stringr::str_squish(sub("\\..*", "", header_info[1])),
#                     UNITS = sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))),
#                     VALUE = stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))), "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))))),
#                     DESCR = stringr::str_trim(sub(".*:", "", header_info[1])))
#   } else {
#     x_temp <- data.frame(MNEM <- stringr::str_squish(sub("\\..*", "", header_info[i])),
#                          UNITS <- sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))),
#                          VALUE <- stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))), "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))))),
#                          DESCR <- stringr::str_trim(sub(".*:", "", header_info[i])))
#     colnames(x_temp) <- colnames(x)
#     x <- rbind(x, x_temp)
#   }
# }
#
#
#
# for(i in 1:length(header_info)) {
#   stringr::str_squish(sub("\\..*", "", header_info[i])) = list(UNITS <- sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))),
#                                                                VALUE <- stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))), "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))))),
#                                                                DESCR <- stringr::str_trim(sub(".*:", "", header_info[i])))
# }

