utils::globalVariables(c("WELL", ".", "DEPTH"))

# ToDo:
#   tracks read from .las goes to private$ptracks, not to self$tracks
#   start and stop depth are not updated in self$header
#   print and summary method does not take format() ??
#   cbind method
#   rbind method
#   colnames ???
#   == set_header (if well, modify the WELL column in data) ------ TESTING
#   set_tracks (modify params, if new track add track to data)
#   add_welltops add_zonation
#   create mlas class
#   write_las automatically write multiple las files from mlas object
#   -----------------------------
#   ++++++ read_las read multiple files automatically
#   ++++++ write las correct DEPT to DEPTH automatically
#   ++++++ methods as private or public? the difference is that obj$print() will not work if private
#   ++++++ scale method as.data.frame(scale(lasdata_train_nn, center = mins, scale = maxs - mins)) + add non-numeric tracks
#   ++++++ min/max/mean method apply(aax$data[2:4], 2, min, na.rm = TRUE) only numeric


#' `las` Class Definition (R6)
#'
#' The class `las` provides customized behaviour of the following functions:
#' \itemize{
#'   \item{\code{print()}} {print method for `las` objects}
#'   \item{\code{summary()}}
#'   \item{\code{head()} / \code{tail()} / \code{dim()} / \code{scale()}, \code{colnames()}} {work directly on \code{obj$data} as it is a data frame}
#'   \item{\code{print()}} {work directly on \code{is_numeric(obj$data)}, applying the function across each column}
#'   \item{\code{print()}} {work directly on \code{obj$data} as it is a data frame, plus modifying accordigly \code{obj$tracks}}
#' }
#'
#' Additional methods are:
#' \itemize{
#'   \item{\code{set_header()}} {set values or new lines on \code{obj$header}}
#'   \item{\code{set_tracks()}} {set values or new lines on \code{obj$tracks}: if a new line is added, then this track is automatically
#'         added to \code{obj$data} as \code{NA}}
#' }
#'
#' @import R6
#' @import dplyr
#' @importFrom utils head tail
#'
las <- R6::R6Class(classname = "las",
                   cloneable = FALSE,
                   # -----------------------------------------------------------------------
                   public = list(

                     ### FIELDS:

                     #' @field header General information from the header
                     header = NULL,
                     #' @field version Las version information, as array
                     version = NULL,
                     #' @field data \code{data.frame} object with tracks data, as data frame
                     data = NULL,

                     ### METHODS:

                     # Create a new las_obj object.
                     initialize = function(header,
                                           version = c("VERS. 2.0 : CWLS Log ASCII Standard - version 2.0", "WRAP.  NO : One line per depth step"),
                                           data = NULL,
                                           tracks = NULL
                                           ) {
                       # check header is a list and WELL has length = 1
                       # stopifnot(is.list(header),
                       #           length(header$WELL) == 1)

                       self$header <- header
                       self$version <- version
                       self$data <- data
                       private$ptracks <- tracks
                       self$tracks <- tracks
                     },

                     # Customized '$min()' method
                     min = function(na.rm = FALSE, group = NULL) {
                       if(is.null(group)) {
                         self$data %>%
                           dplyr::select_if(is.numeric) %>%
                           apply(2, min, na.rm = na.rm) %>%
                           return()
                       } else {
                         self$data %>%
                           tidyr::pivot_longer(cols = -group, names_to = "var") %>%
                           filter(is.numeric(value))%>%
                           dplyr::group_by_("var", group) %>%
                           dplyr::summarize(x = min(value, na.rm = na.rm)) %>%
                           tidyr::spread(var, x) %>%
                           return()
                       }
                     },

                     # Customized '$max()' method
                     max = function(na.rm = FALSE, group = NULL) {
                       if(is.null(group)) {
                         self$data %>%
                           dplyr::select_if(is.numeric) %>%
                           apply(2, max, na.rm = na.rm) %>%
                           return()
                       } else {
                         self$data %>%
                           tidyr::pivot_longer(cols = -group, names_to = "var") %>%
                           filter(is.numeric(value))%>%
                           dplyr::group_by_("var", group) %>%
                           dplyr::summarize(x = max(value, na.rm = na.rm)) %>%
                           tidyr::spread(var, x) %>%
                           return()
                       }
                     },

                     # Customized '$mean()' method
                     mean = function(na.rm = FALSE, group = NULL) {
                       if(is.null(group)) {
                         self$data %>%
                           dplyr::select_if(is.numeric) %>%
                           apply(2, mean, na.rm = na.rm) %>%
                           return()
                       } else {
                         self$data %>%
                           tidyr::pivot_longer(cols = -group, names_to = "var") %>%
                           filter(is.numeric(value))%>%
                           dplyr::group_by_("var", group) %>%
                           dplyr::summarize(x = mean(value, na.rm = na.rm)) %>%
                           tidyr::spread(var, x) %>%
                           return()
                       }
                     },

                     # Customized '$quantile()' method
                     quantile = function(probs = 0.5, na.rm = FALSE, group = NULL) {
                       if(is.null(group)) {
                         self$data %>%
                           dplyr::select_if(is.numeric) %>%
                           apply(2, quantile, probs = probs, na.rm = na.rm) %>%
                           return()
                       } else {
                         self$data %>%
                           tidyr::pivot_longer(cols = -group, names_to = "var") %>%
                           filter(is.numeric(value))%>%
                           dplyr::group_by_("var", group) %>%
                           dplyr::summarize(x = quantile(value, probs = probs, na.rm = na.rm)) %>%
                           tidyr::spread(var, x) %>%
                           return()
                       }
                     },

                     # Set Values to Header
                     set_header = function(value, item = "WELL", units = NA, descr = NA) {
                       if(item %in% self$header$MNEM & item == "WELL") {
                         self$header[self$header$MNEM == "WELL", 3] <- as.character(value)
                         self$data$WELL <- value
                         if(!is.na(units)) self$header[self$header$MNEM == "WELL", 2] <- units
                         if(!is.na(descr)) self$header[self$header$MNEM == "WELL", 4] <- descr
                       } else if (item %in% self$header$MNEM) {
                         self$header[self$header$MNEM == item, 3] <- as.factor(value)
                         if(!is.na(units)) self$header[self$header$MNEM == item, 2] <- units
                         if(!is.na(descr)) self$header[self$header$MNEM == item, 4] <- descr
                       } else {

                         temp <- dplyr::tibble(MNEN = item,
                                               UNITS = ifelse(is.na(units), "", units),
                                               VALUE = value,
                                               DESCR = ifelse(is.na(descr), "", descr))

                         self$header <- dplyr::bind_rows(self$header, temp)

                       }
                     }


                     # Add tracks from another las_obj of the same well
                     # add_tracks = function(las_obj2, overwrite = FALSE, join = "left", header_join = "left") {
                     #
                     #   # check wellname
                     #   # check colnames
                     #
                     #   self$data <- left_join(self$data, las_obj2$data, by = c("WELL", "DEPTH"))
                     #   self$tracks$names <- c(self$tracks$names, las_obj2$tracks$names[!c(las_obj2$tracks$names %in% self$tracks$names)])
                     #
                     #   invisible(self)
                     # }

                   ),
                   # -----------------------------------------------------------------------
                   active = list(
                     #' @field tracks active binded field: tracks information
                     tracks = function(value) {

                       if (missing(value)) {
                         # # update header data
                         # self$header[self$header$MNEM == "STRT", 3] <- min(self$data$DEPTH)
                         # self$header[self$header$MNEM == "STOP", 3] <- max(self$data$DEPTH)
                         # self$header[self$header$MNEM == "STEP", 3] <- self$data$DEPTH[2] - self$data$DEPTH[1]
                         # self$header[self$header$MNEM == "NULL", 3] <- "-999.2500"
                         # self$header[self$header$MNEM == "WELL", 3] <- as.character(unique(self$data$WELL))

                         # tracks data frame
                         data.frame(MNEM = colnames(self$data),
                                    UNIT = "UNKNOWN",
                                    APICODE = "UNKNOWN") %>%
                           mutate(CDESCR = paste(MNEM, APICODE, sep = ":"))
                         }
                       else colnames(self$data) <- c("WELL", as.character(private$ptracks$MNEM))
                       }
                     # header = function(value) {
                     #   if (missing(value)) {
                     #     # update header data
                     #     self$header[self$header$MNEM == "STRT", 3] <- min(self$data$DEPTH)
                     #     self$header[self$header$MNEM == "STOP", 3] <- max(self$data$DEPTH)
                     #     self$header[self$header$MNEM == "STEP", 3] <- self$data$DEPTH[2] - self$data$DEPTH[1]
                     #     self$header[self$header$MNEM == "NULL", 3] <- "-999.2500"
                     #     self$header[self$header$MNEM == "WELL", 3] <- as.character(unique(self$data$WELL))
                     #   }
                     #   else value
                     # },
                     # header_strt = function(value) {
                     #   if(self$header[self$header$MNEM == "STRT", 3] != min(self$data$DEPTH)) min(self$data$DEPTH)
                     # },
                     # header_stop = function(value) {
                     #   if(self$header[self$header$MNEM == "STOP", 3] != max(self$data$DEPTH)) max(self$data$DEPTH)
                     # },
                     # header_step = function(value) {
                     #   if(self$header[self$header$MNEM == "STEP", 3] !=  self$data$DEPTH[2] - self$data$DEPTH[1]) {self$data$DEPTH[2] - self$data$DEPTH[1]}
                     # }
                   ),
                   private = list(
                     ptracks =  data.frame(MNEM = NA),

                     # Define behaviour of \code{print} method.
                     print = function(...) {
                       cat("LAS format logs file from R (petroreadr): \n")
                       cat("\n")
                       cat("  Well: ", as.character(self$header[self$header$MNEM == "WELL", 3]), "\n", sep = "")
                       cat("\n")
                       cat("  .las version: ", self$version, "\n", sep = "")
                       cat("\n")
                       cat("  Tracks", paste0("[", length(self$tracks$MNEM), "]:") , as.character(self$tracks$MNEM), "\n", sep = " ")
                       cat("\n")
                       cat("  Data: ", nrow(self$data), "rows x", ncol(self$data), "cols", "\n", sep = " ")
                       # format(utils::head(self$data, n = 3L))
                       invisible(self)
                     },

                     # Customized '$summary()' method
                     summary = function(object, ...) {
                       cat("Summary of las object:")
                       cat("\n")
                       cat("  Well: ", as.character(self$header[self$header$MNEM == "WELL", 3]), "\n", sep = "")
                       cat("  start: ", as.character(self$header[self$header$MNEM == "STRT", 3]), "\n", sep = "")
                       cat("  Stop: ", as.character(self$header[self$header$MNEM == "STOP", 3]), "\n", sep = "")
                       cat("  Step: ", as.character(self$header[self$header$MNEM == "STEP", 3]), "\n", sep = "")
                       cat("  Null: ", as.character(self$header[self$header$MNEM == "NULL", 3]), "\n", sep = "")
                       cat("  Tracks: ","\n")
                       cat("  ", "MNEM", "UNIT", "API CODE", "CURVE DESCRIPTION", "\n", sep = "\t\t")
                       for(i in 1:nrow(self$tracks)) {
                         cat("  ", as.character(self$tracks[i, 1]), as.character(self$tracks[i, 2]),
                             as.character(self$tracks[i, 3]), as.character(self$tracks[i, 4]), "\n", sep = "\t\t")
                       }
                       # format(self$tracks)
                       cat("\n")
                       summary(self$data %>% dplyr::select(-"WELL"))
                     },

                     # Customized '$dim()' method
                     dim = function(x) {
                       dim(self$data)
                     },

                     # Customized '$head()' method
                     head = function(x, n, ...) {
                       utils::head(self$data, n = n)
                     },

                     # Customized '$tail()' method
                     tail = function(x, n, ...) {
                       utils::tail(self$data, n = n)
                     },

                     # Customized '$colnames()' method
                     # colnames = function(x, do.NULL = TRUE, prefix = "col") {
                     #   colnames(self$data)
                     # },

                     # Customized '$cbind()' method
                     cbind = function(..., deparse.level = deparse.level) {
                       stopifnot(is.las(self),
                                 is.las(.))

                       cbind(self$data, .$data)
                     },

                     # Customized '$rbind()' method
                     rbind = function(..., deparse.level = deparse.level) {
                       stopifnot(is.las(self),
                                 is.las(.))

                       rbind(self$data, .$data)
                     },

                     # Customized 'Sscale()' method
                     scale = function(x, center = center, scale = scale) {
                       self$data <- as.data.frame(scale(self$data, center = center, scale = scale))
                       invisible(self)
                     }
                   ))


#### S3 method callers ---------------------------------------------------------------------------------------------

#' Summarizing `las` Objects
#'
#' Summary method for class `las`. The function call the R6 method defined within the class
#' @param object las object
#' @param ... additional arguments
#'
#' @examples
#' \dontrun{
#' summary(obj)
#' obj$summary()
#' }
#' @export
summary.las <- function(object, ...) {
  object$.__enclos_env__$private$summary()
}
#' dim() S3 Method - R6 Method caller
#'
#' @param x las object
#' @export
dim.las <- function(x) {
  x$.__enclos_env__$private$dim()
}
#' colnames() S3 Method - R6 Method caller
#'
#' @param x las object
#' @param do.NULL logical. If \code{FALSE} and names are \code{NULL}, names are created
#' @param prefix for created names.
#' @export
colnames.las <- function(x, do.NULL = TRUE, prefix = "col") {
  x$.__enclos_env__$private$colnames()
}
#' head() S3 Method - R6 Method caller
#'
#' @param x las object
#' @param n integer, number of rows
#' @param ... additonal arguments
#' @export
head.las <- function(x, n = 6L, ...) {
  x$.__enclos_env__$private$head(n = n)
}
#' tail() S3 Method - R6 Method caller
#'
#' @param x las object
#' @param n integer, number of rows
#' @param ... additonal arguments
#' @export
tail.las <- function(x, n = 6L, ...) {
  x$.__enclos_env__$private$tail(n = n)
}
#' cbind() S3 Method - R6 Method caller
#'
#' @param ... N las object
#' @param deparse.level integer controlling the construction of labels in the case of non-matrix-like arguments (for the default method):
#'     deparse.level = 0 constructs no labels; the default,
#'     deparse.level = 1 or 2 constructs labels from the argument names, see the ‘Value’ section below.
#' @export
cbind.las <- function(..., deparse.level = 1) {
  .$.__enclos_env__$private$cbind()
}
#' rbind() S3 Method - R6 Method caller
#'
#' @param ... N las object
#' @param deparse.level integer controlling the construction of labels in the case of non-matrix-like arguments (for the default method):
#'     deparse.level = 0 constructs no labels; the default,
#'     deparse.level = 1 or 2 constructs labels from the argument names, see the ‘Value’ section below.
#' @export
rbind.las <- function(..., deparse.level = 1) {
  .$.__enclos_env__$private$rbind()
}
#' scale() S3 Method - R6 Method caller
#'
#' @param x las object
#' @param center either a logical value or numeric-alike vector of length equal to the number of columns of x,
#'                   here ‘numeric-alike’ means that as.numeric(.) will be applied successfully if is.numeric(.) is not true.
#' @param scale either a logical value or a numeric-alike vector of length equal to the number of columns of x.
#' @export
scale.las <- function(x, center = FALSE, scale = FALSE) {
  x$.__enclos_env__$private$scale()
}
#
#
# #' Minima of Each Track
# #'
# #' @param ... las object
# #' @param na.rm a logical indicating whether missing values should be removed.
# #' @export
# min.las <- function(..., na.rm = FALSE) {
#   .$.__enclos_env__$private$min()
# }
# #' Maxima of Each Track
# #'
# #' @param ... las object
# #' @param na.rm a logical indicating whether missing values should be removed.
# #' @export
# max.las <- function(..., na.rm = FALSE) {
#   .$.__enclos_env__$private$max()
# }
# #' Arithmetic Mean of Each Track
# #'
# #' @param x las object
# #' @param trim he fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed.
# #'     Values of trim outside that range are taken as the nearest endpoint.
# #' @param na.rm a logical indicating whether missing values should be removed.
# #' @param ... further arguments passed to or from other methods.
# #' @export
# mean.las <- function(x, trim = 0, na.rm = FALSE, ...) {
#   x$.__enclos_env__$private$mean()
# }
#
#

#### Definition Functions ---------------------------------------------------------------------------------------------

#' Check If The Argument Belongs To ´las´ R6 Class
#'
#' @param x a R object
#' @return logical value, \code{TRUE} or \code{FALSE}
#' @export
is.las <- function(x) inherits(x, "las")

#' Check If Belongs To ´mlas´ Class
#'
#' @param x a R object
#' @export
is.mlas <- function(x) inherits(x, "mlas")


#' Coherce Data Frame To ´las´ Class
#'
#' The function takes a data frame and try to coherce it to ´las´ R6 Class, building the corresponding
#'    header and tracks information.
#'
#' @param x a data frame
#' @param id character string to be used as ID. If it is any of the columns names of \code{x},
#'            it used the unique of that columen as is. A different string can be passed. Default = "WELL"
#'
#' @importFrom dplyr pull
#'
#' @export
as.las <- function(x, id = "WELL") {

  if(id %in% colnames(x)) {
    id <-  as.character(unique(dplyr::pull(x, id)))
  }

  stopifnot(is.data.frame(x),
            length(id) == 1,
            "DEPTH" %in% colnames(x))

  index <- dplyr::pull(x, DEPTH)

  header <- data.frame(MNEM = c("STRT", "STOP", "STEP", "NULL", "WELL"),
                       UNIT = c(rep("FT", 3), "", ""),
                       VALUE = as.factor(c(min(index), max(index), index[2] - index[1], "-999.2500", id)),
                       DESCR = c("START DEPTH", "STOP DEPTH", "STEP", "NULL VALUE", "WELL"))

  temp <- las$new(header = header,
                  version = c("VERS. 2.0 : CWLS Log ASCII Standard - version 2.0", "WRAP.  NO : One line per depth step"),
                  data = x)

  return(temp)
}


#### Read / Write LAS  ---------------------------------------------------------------------------------------------

#' Extract Info From .las Line
#'
#' @note internal function used in \code{read_las}
#'
#' @param oneline character string containing the info to be extracted from .las file
#' @param key character string defining the keyword of the info to be extracted
#'
#' @return string with the extracted info value
#'
#' @importFrom stringr str_trim
extract_lasinfo <- function(oneline, key) {

  temp <- stringr::str_trim(oneline) # remove initial and final spaces
  temp <- gsub("\\s+", " ", temp) # replace multiple spaces with 1 space
  temp <- sub(paste0(key, " *\\."), "", temp) # remove initial "WELL ." with any number of spaces between WELL and .
  temp <- sub("\\: *\\w* *\\w* *\\w*", "", temp) # remove final ": WELL" with any number of spaces between : and WELL
  temp <- stringr::str_trim(temp) #remove initial and final spaces

  return(temp)
}



#' Read .las logs file - INTERNAL FUNCTION
#'
#' @param filename A character string giving the name or path of the file to be read.
#' @param verbose logical, define if the functions print the name of the file read. Default = FALSE
#'
#' @return an R6 object of class \code{las} or \code{mlas}
#'
#' @importFrom dplyr mutate select %>% as_tibble
#' @importFrom stringr word str_trim str_squish
#' @importFrom utils read.table
#'
read_las_ <- function(filename, verbose = FALSE) {

  # path2data <- "C:/Users/giorgfr/OneDrive - Occidental Petroleum Corporation/R_Projects/rpetophy/Data/lasfiles_test01"
  # # path2data <- "~/OneDrive - Oxy/R_Projects/rpetophy/Data/lasfiles_test01"
  # lasfiles <- list.files(path2data)
  # filename <- file.path(path2data, lasfiles[1])

  if (!file.exists(filename))
    stop("File '", filename, "' does not exist!")

  lasfile <- file(filename, open = "r")
  headerlines <- readLines(con = lasfile, n = 200L, ok = TRUE, skipNul = TRUE)
  close(lasfile)

  # header ---------------------------------------------------------------------------------------------
  pattern_h <- paste("~W", "~Well", "~WELL", sep = "|")
  infoline <- grep(pattern_h, headerlines)
  infoline_end <- grep("~", headerlines)[which(grep("~", headerlines) %in% infoline) + 1]

  header_info <- subset(headerlines[(infoline + 1):(infoline_end - 1)], !grepl("#", headerlines[(infoline + 1):(infoline_end - 1)]))

  # convert header_info to data frame
  for(i in 1:length(header_info)) {
    if(i == 1) {
      x <- data.frame(MNEM = stringr::str_squish(sub("\\..*", "", header_info[1])),
                      UNITS = sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))),
                      VALUE = stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))), "", sub("\\.", "", sub(sub("\\..*", "", header_info[1]), "", header_info[1]))))),
                      DESCR = stringr::str_trim(sub(".*:", "", header_info[1])))
    } else {
      x_temp <- data.frame(MNEM = stringr::str_squish(sub("\\..*", "", header_info[i])),
                           UNITS = sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))),
                           VALUE = stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))), "", sub("\\.", "", sub(sub("\\..*", "", header_info[i]), "", header_info[i]))))),
                           DESCR = stringr::str_trim(sub(".*:", "", header_info[i])))
      colnames(x_temp) <- colnames(x)
      x <- rbind(x, x_temp)
    }
  }

  x$MNEM <- as.character(x$MNEM)
  x$UNITS <- as.character(x$UNITS)
  x$VALUE <- as.character(x$VALUE)
  x$DESCR <- as.character(x$DESCR)

  x <- dplyr::as_tibble(x)

  wellname <- extract_lasinfo(header_info[grep("WELL *\\.", header_info)], "WELL")
  lasnull <- extract_lasinfo(header_info[grep("NULL *\\.", header_info)], "NULL")

  # version --------------------------------------------------------------------------------------------
  pattern_v <- paste("~V", "~Version", "~VERSION", sep = "|")
  versionline <- grep(pattern_v, headerlines)
  versionline_end <- grep("~", headerlines)[which(grep("~", headerlines) %in% versionline) + 1]

  version_info <- subset(headerlines[(versionline + 1):(versionline_end - 1)], !grepl("#", headerlines[(versionline + 1):(versionline_end - 1)]))

  # tracks ---------------------------------------------------------------------------------------------
  pattern_c <- paste("~C", "~Curve", "~CURVE", sep = "|")
  curveline <- grep(pattern_c, headerlines)
  curveline_end <- grep("~", headerlines)[which(grep("~", headerlines) %in% curveline) + 1]
  ncurves <- curveline_end - curveline - 1 - length(grep("#", headerlines[curveline:curveline_end]))

  logname <- seq(0, 0, length.out = ncurves)
  trackslines <- subset(headerlines[(curveline):(curveline_end - 1)], !grepl("#", headerlines[(curveline):(curveline_end - 1)]))

  # generate array with names of each log
  for (i in 1:ncurves) {
    oneline <- trackslines[1 + i]
    logname[i] <- stringr::word(gsub("\\s+", " ", stringr::str_trim(oneline)), 1)[1]
  }

  # convert trackslines to data frame
  trackslines <- trackslines[-1]
  for(i in 1:length(trackslines)) {
    if(i == 1) {
      y <- data.frame(MNEM = stringr::str_squish(sub("\\..*", "", trackslines[1])),
                      UNITS = sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", trackslines[1]), "", trackslines[1]))),
                      APICODE = stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", trackslines[1]), "", trackslines[1]))), "", sub("\\.", "", sub(sub("\\..*", "", trackslines[1]), "", trackslines[1]))))),
                      DESCR = stringr::str_trim(sub(".*:", "", trackslines[1])))
    } else {
      y_temp <- data.frame(MNEM = stringr::str_squish(sub("\\..*", "", trackslines[i])),
                           UNITS = sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", trackslines[i]), "", trackslines[i]))),
                           APICODE = stringr::str_trim(sub(":.*", "", sub(sub("\\s+.*", "", sub("\\.", "", sub(sub("\\..*", "", trackslines[i]), "", trackslines[i]))), "", sub("\\.", "", sub(sub("\\..*", "", trackslines[i]), "", trackslines[i]))))),
                           DESCR = stringr::str_trim(sub(".*:", "", trackslines[i])))
      colnames(y_temp) <- colnames(y)
      y <- rbind(y, y_temp)
    }
  }

  y$MNEM <- as.character(y$MNEM)
  y$UNITS <- as.character(y$UNITS)
  y$APICODE <- as.character(y$APICODE)
  y$DESCR <- as.character(y$DESCR)

  y <- dplyr::as_tibble(y)

  # data -----------------------------------------------------------------------------------------------
  pattern_a <- paste("~A", "~Ascii", "~ASCII", sep = "|")
  dataline <- as.numeric(grep(pattern_a, headerlines) + 1)
  lasfile <- file(filename, open = "r")
  temp <- utils::read.table(lasfile, header = FALSE, na.strings = lasnull, skip = dataline - 1, col.names = logname, stringsAsFactors = FALSE)
  close(lasfile)

  temp <- temp %>%
    dplyr::mutate(WELL = wellname) %>%
    dplyr::select(WELL, 1:ncurves)

  if("DEPT" %in% colnames(temp)) {
    temp <- temp %>%
      rename(DEPTH = DEPT)
  }

  # Construct `las` object -------------------------------------------------------------------------
  object_temp <- las$new(header = x,
                         version = version_info,
                         data = temp,
                         tracks = y)

  # object_temp$tracks <-

  if(verbose) cat("   + ", sub(".*\\/", "", filename), " imported as <las> object", '\n', sep = "")
  return(object_temp)
}


#' Read .las logs file
#'
#' This function read well logs data from .las files and return a `las` or `mlas` objetc.
#'    To obtain just the data.frame with logs data, just add \code{$data} at the end of \code{read_las()}. See examples.
#'    If the \code{filename} you provide does not existe, the function stops and return an error message.
#'
#' @note \code{read_las} has been tested to LAS 2.0 standard, check:
#'     <http://www.cwls.org/wp-content/uploads/2017/02/Las2_Update_Feb2017.pdf>
#'
#' @param filename character string or array of characters string giving the name or path of the file to be read.
#'            If \code{lenght(filename) == 1}, the function returns a `las` object.
#'            If \code{lenght(filename) > 1}, the behaviour is as follows:
#'            \itemize{
#'              \item{all files refers to the same `WELL`:} {data are joined using \code{dplyr::left_join(by = c("WELL", "DEPTH"))},
#'                    retunring a `las` object}
#'              \item{each file refers to different `WELL`s:} {data are row-binded, returning a `mlas` object.
#'                    NOTE: the function uses \code{dplyr::bind_rows}: each track will be added as a column and
#'                    missing/new tracks will be filled with NA where not present }
#'              \item{cambination of files referring to the same well and different wells:} {This case is still not handled}}
#' @param verbose logical, define if the functions print the name of the file read. Default = FALSE
#'
#' @return an R6 object of class \code{las} or \code{mlas}
#'
#' @importFrom dplyr bind_rows left_join
#'
#' @examples
#' \dontrun{
#' setwd(file.path("pathname"))
#' las_obj <- read_las("filename.las")
#'
#' las_obj <- read_las(file.path("pathname", "filename.las")
#'
#' las_obj <- read_las(file.path("pathname", c("filename_1.las", "filename_2.las")))
#' class(las_obj)
#' # "las"
#'
#' las_obj <- read_las(file.path("pathname", c("filename_1.las", "filename_2.las")))$data
#' class(las_obj)
#' # "data.frame"
#'
#' lasfiles <- list.files("pathname")
#' lasfiles <- lasfiles[grepl(".las", lasfiles)]
#' read_las(file.path("pathname", lasfiles), verbose = TRUE)
#' }
#'
#' @export
read_las <- function(filename, verbose = FALSE){

  for(i in 1:length(filename)){
    if(i == 1) {
      lasrawdata <- read_las_(filename[1], verbose = verbose)
    } else {
      temp <- read_las_(filename[i], verbose = verbose)

      if(temp$header[temp$header$MNEM == "WELL", 3] == lasrawdata$header[lasrawdata$header$MNEM == "WELL", 3]) {

        lasrawdata$data <- lasrawdata$data %>%
          dplyr::left_join(temp$data, by = c("WELL", "DEPTH"))

        } else {
        lasrawdata$data <- lasrawdata$data %>%
          dplyr::bind_rows(temp$data)

        # !!!!! assign to `mlas` object !!!!!
      }
    }
  }

  return(lasrawdata)

}
