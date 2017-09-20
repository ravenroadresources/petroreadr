utils::globalVariables(c("WELL", "."))

#' Read .las files
#'
#' This function read well logs data from a .las file and return a data frame with
#'    depth and all log values.
#'    If the \code{filename} you provide does not existe, the function stops and
#'    return an error message.
#'
#' @note \code{read_las} has been tested to LAS 2.0 standard, check:
#'     <http://www.cwls.org/wp-content/uploads/2017/02/Las2_Update_Feb2017.pdf>
#'
#' @param filename A character string giving the name or path of the file to be read.
#' @param lasnull A character string indicating which value is sued as \code{NULL}
#'
#' @return This function returns a dataframe with the logs data read from a .las file.
#'    It include a first column with the wellname.
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom stringr word str_trim
#' @importFrom utils read.table
#'
#' @examples
#' \dontrun{
#' read_las("filename.las")
#' read_las("pathname/filename.las", "-999")
#' }
#'
#' @export
read_las <- function(filename,
                     lasnull="-999.250000") {

  if(!file.exists(filename)) stop("File '", filename, "' does not exist!")

  lasfile <- file(filename, open="r")

  ### 1. read header of the las file (LAS files have all the headers within first 100 lines)
  headerlines <- readLines(con = lasfile, n = 100L, ok = TRUE, skipNul = TRUE)

  # 1.a get the well name
  pattern <- paste("WELL \\.", "WELL\\.", sep = "|")
  oneline <- headerlines[grep(pattern, headerlines)]
  #wellname <- stringr::word(gsub("\\s+"," ", stringr::str_trim(oneline)),2)[1]
  wellname <- extract_wellname(oneline)

  # 1.b get the names of the logs
  pattern_c <- paste("~C" ,"~Curve", "~CURVE", sep = "|")
  aa <- grep(pattern_c, headerlines)
  bb <- grep("~", headerlines)[which(grep("~", headerlines) %in% aa) + 1] # look for the next line with ~ after ~C

  # count number of log curves, discounting comment lines starting with "#"
  ncurves <- bb - aa - 1 - length(grep("#", headerlines[aa:bb]))
  logname <- seq(0, 0, ncurves)
  curveslines <- subset(headerlines[(aa):(bb-1)], !grepl("#", headerlines[(aa):(bb-1)])) # remove comment lines
  for (i in 1:ncurves) {
    oneline <- curveslines[1 + i]
    logname[i] <- stringr::word(gsub("\\s+", " ", stringr::str_trim(oneline)), 1)[1]
  }

  # 1.c get the first line with data
  pattern_a <- paste("~A" ,"~Ascii", "~ASCII", sep = "|")
  dataline <- as.numeric(grep(pattern_a, headerlines) + 1)

  ### 2. read the log data from the line after ~Ascii
  temp <- utils::read.table(lasfile,
                     header = FALSE,
                     na.strings = lasnull,
                     skip = dataline-1,
                     col.names = logname)

  close(lasfile)

  temp <- temp %>%
    dplyr::mutate(WELL = wellname) %>%
    dplyr::select(WELL, 1:ncurves)

  return(temp)
}



#' Read grid properties exported in ascii format
#'
#' This function read 3D grid properties exported in ascii format and returns a
#' data frame with the resepctive properties value, and optionallythe I, J and K
#' indexes.
#'
#' If the \code{filename} you provide does not existe, the function stops and
#' returns an error message.
#'
#' @note Basically what this function do is to convert the ascci format into a
#'   easier to operate gslib-like format.
#'
#' @param filename A character string giving the name or path of the file to be read.
#' @param index A logic flag telling if IJK index should be returned or not.
#'   If set to \code{FALSE} following parameters are ignored.
#'   Default \code{TRUE}.
#' @param dims An array of length = 3,telling the IJK dimension of the cube.
#'   Has no default.
#' @param order A character string defining in which order Petrel exported the grid.
#'   Petrel Export Options:
#'   Cell Origin at (I=0, J=0, K)
#'   Cell Origin at (I=0, J=max(J), K)
#'   Cell Origin at (I=max(I), J=max(J), K)
#'   Cell Origin at (I=max(I), J=0, K)
#'   Should be one of: "IJK","IJmaxK","ImaxJmaxK" or "ImaxJK".
#'   Default \code{"IJK"}.
#' @param along A character string defining in which order traverse the indexes.
#'   Traverse first along I, then along J
#'   Traverse first along J, then along I
#'   Should be either: "I" or "J"
#'   Default \code{"I"}.
#'
#' @return This function returns a dataframe with the grid properties and for
#'   each cell, and if \code{index = TRUE} the I, J and K indexes of the cells.
#'
#' @note At the moment the function only read 1 porperty per file.
#'   Upgrade to read several properties in the same file is under development.
#'
#' @importFrom stringr word
#' @importFrom stringr str_trim
#' @importFrom stringr str_split
#'
#' @examples
#' \dontrun{
#'   read_asciigrid("filename.las", dims = c(12, 12, 25))
#'   read_asciigrid("filename.las", index = FALSE)
#' }
#'
#' @export
read_asciigrid <- function(filename,
                           index = TRUE,
                           dims,
                           order = "IJK",
                           along = "I") {

  if(!file.exists(filename)) stop("File '", filename, "' does not exist!")
  # open file connection
  asciifile <- file(filename, open = "r")

  ### 1. read header
  headerlines <- readLines(con = asciifile, n = 25L, ok = TRUE, skipNul = TRUE)

  # 1.a get the prop name
  oneline <- headerlines[grep("-- Property name in Petrel :", headerlines)]
  propname <- stringr::word(gsub("\\s+", " ", stringr::str_trim(oneline)), 7)[1]

  # 1.b get the first line with data
  dataline <- as.numeric(grep("-- Property name in Petrel :", headerlines)+1)


  ### 2. read properties
  input <- readLines(con = asciifile, ok = TRUE, skipNul = TRUE)
  input <- input[dataline:length(input)]
  # close file connection
  close(asciifile)

  # split lines for each value or pair of values
  raw_values <- stringr::str_split(input, " ", simplify = TRUE)

  # check dimensions
  n <- dim(raw_values)

  # initializize empty array
  prop <- c(NULL)
  # for each value or pair of values
  for (i in 1:n[1]) {
    for (j in 1:n[2]) {
      # if is not empty value (the number of values or pairs may be different in each raw.
      # The str_split function automatically try to reorganize data in in a table, with
      # n_col = max number of values of pairs per row. It fills empty spots with "")
      if (raw_values[i,j]!="") {
        # if is / stop
        if (raw_values[i,j]=="/") break
        # if is a pair (n*v)
        else if (grepl("\\*", raw_values[i,j])) {
          # split the pair
          temp <- as.numeric(stringr::str_split(raw_values[i, j], "\\*", simplify = TRUE))
          # redefine an array repeating n times the value v
          x <- rep(temp[2], temp[1])
          # cocantenate with previous values
          prop <- as.numeric(c(prop,x))
        }
        # if is not a pair (single value), concatenate with previous values
        else {prop <- as.numeric(c(prop,raw_values[i,j]))}
      }
    }
  }

  ### 3. Add I, J and K to the data frame
  if (is.null(dims) & index == TRUE) {
    warning("IJK dimensions must be specified to calcualte the property index. Property output without index.")
  }
  else if (length(prop) != prod(dims)) {
    warning("The provided dimensions does not match the property values. Property output without index.")
  }
  else if (!(order %in% c("IJK", "IJmaxK", "ImaxJmaxK", "ImaxJK"))) {
    warning("The order must be one of: 'IJK','IJmaxK','ImaxJmaxK','ImaxJK'. Property output without index.")
  }
  else if (!(along %in% c("I","J"))) {
    warning("The along parameter must be one of: 'I' or 'J'. Property output without index.")
  }
  else if (along == "I") {
      if (order == "IJK") {
               jj <- NULL
               kk <- NULL
               ii <- rep(1:dims[1],dims[2])
               for (j in 1:dims[2]) jj <- c(jj, rep(j, dims[1]))
               for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

               prop <- as.data.frame(cbind(ii,jj,kk,prop))
               colnames(prop) <- c("I", "J", "K", propname)
      }
      else if (order == "IJmaxK") {
        jj <- NULL
        kk <- NULL
        ii <- rep(1:dims[1],dims[2])
        for (j in dims[2]:1) jj <- c(jj, rep(j, dims[1]))
        for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

        prop <- as.data.frame(cbind(ii,jj,kk,prop))
        colnames(prop) <- c("I", "J", "K", propname)
      }
      else if (order == "ImaxJmaxK") {
        jj <- NULL
        kk <- NULL
        ii <- rep(dims[1]:1,dims[2])
        for (j in dims[2]:1) jj <- c(jj, rep(j, dims[1]))
        for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

        prop <- as.data.frame(cbind(ii,jj,kk,prop))
        colnames(prop) <- c("I", "J", "K", propname)
      }
      else if (order == "ImaxJK") {
        jj <- NULL
        kk <- NULL
        ii <- rep(dims[1]:1,dims[2])
        for (j in 1:dims[2]) jj <- c(jj, rep(j, dims[1]))
        for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

        prop <- as.data.frame(cbind(ii,jj,kk,prop))
        colnames(prop) <- c("I", "J", "K", propname)
      }
    }
  else if (along == "J") {
    if (order == "IJK") {
      ii <- NULL
      kk <- NULL
      for (i in 1:dims[1]) ii <- c(ii, rep(i, dims[2]))
      jj <- rep(1:dims[2],dims[1])
      for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

      prop <- as.data.frame(cbind(ii,jj,kk,prop))
      colnames(prop) <- c("I", "J", "K", propname)
    }
    else if (order == "IJmaxK") {
      ii <- NULL
      kk <- NULL
      for (i in 1:dims[1]) ii <- c(ii, rep(i, dims[2]))
      jj <- rep(dims[2]:1, dims[1])
      for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1] * dims[2]))

      prop <- as.data.frame(cbind(ii, jj, kk, prop))
      colnames(prop) <- c("I", "J", "K", propname)
    }
    else if (order == "ImaxJmaxK") {
      ii <- NULL
      kk <- NULL
      for (i in dims[1]:1) ii <- c(ii, rep(i, dims[2]))
      jj <- rep(dims[2]:1,dims[1])
      for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

      prop <- as.data.frame(cbind(ii,jj,kk,prop))
      colnames(prop) <- c("I", "J", "K", propname)
    }
    else if (order == "ImaxJK") {
      ii <- NULL
      kk <- NULL
      for (i in dims[1]:1) ii <- c(ii, rep(i, dims[2]))
      jj <- rep(1:dims[2],dims[1])
      for (k in 1:dims[3]) kk <- c(kk, rep(k, dims[1]*dims[2]))

      prop <- as.data.frame(cbind(ii, jj, kk, prop))
      colnames(prop) <- c("I", "J", "K", propname)
    }
  }

  return(prop)
}



#' Read .gslib grid format
#'
#' This function read 3D grid properties in .gslib format and returns a data
#' frame.
#'
#' If the \code{filename} you provide does not existe, the function stops and
#' return an error message.
#'
#' @param filename A character string giving the name or path of the file to be read.
#' @param complete A logic flag telling if complete cases only should be returned.
#'   Default \code{TRUE}.
#' @param propnull A character string specifying the null property value.
#'   Default \code{"-99.00"}.
#'
#' @return This function returns a dataframe with lthe logs data read from a .las file.
#'
#' @importFrom stringr word
#' @importFrom stringr str_trim
#' @importFrom dplyr filter %>%
#' @importFrom utils read.table
#' @importFrom stats complete.cases
#'
#' @examples
#' \dontrun{
#'   read_gslib("filename.gslib")
#' }
#'
#' @export
read_gslib <- function(filename, complete = TRUE, propnull="-99.00") {

  if(!file.exists(filename)) stop("File '", filename, "' does not exist!")

  asciifile <- file(filename, open = "r")

  ### 1. read header of the ascii file
  headerlines <- readLines(con = asciifile, n = 100L, ok = TRUE, skipNul = TRUE)

  # 1.a get the number of properties
  oneline <- headerlines[2]
  nprop <- as.numeric(oneline)

  # 1.b get the names of the porperties
  propname <- seq(0,0,nprop)
  for (i in 1:nprop) {
    oneline <- headerlines[2+i]
    propname[i] <- stringr::word(gsub("\\s+", " ", stringr::str_trim(oneline)), 1)[1]
  }

  ### 2. read the log data from the line after ~Ascii
  temp <- utils::read.table(asciifile, header = F, na.strings = propnull,
                     skip = nprop + 2, col.names = propname)

  close(asciifile)

  if (complete) {
    temp <- temp %>%
      dplyr::filter(stats::complete.cases(.))
  }

  return(temp)
}




#' Extract Well Name from .las line
#'
#' @note internal function used in \code{read_las}
#'
#' @param oneline string character containing the well name extracted form .las file
#'
#' @return string with the ecxtacte well name
#'
#' @importFrom stringr str_trim
#'
extract_wellname <- function(oneline) {

  pattern_1 <- paste("WELL \\.", "WELL\\.", sep = "|")
  pattern_2 <- paste("(.*) : WELL", "(.*) :WELL", sep = "|")

  temp <- stringr::str_trim(oneline) # remove initial and final spaces
  temp <- gsub("\\s+", " ", temp) # replace multiple spaces with 1 space
  #temp <- sub("WELL\\.", "", temp) # remove initial "WELL."
  #temp <- sub("WELL \\.", "", temp) # remove initial "WELL ."
  temp <- sub(pattern_1, "", temp) # remove initial "WELL ."
  #temp <- sub(pattern_2, "", temp) # remove initial "WELL ."
  temp <- sub("(.*) :WELL", "\\1", temp) #remove ": WELL" at the end of the string
  temp <- sub("(.*) : WELL", "\\1", temp) #remove ": WELL" at the end of the string
  temp <- sub("(.*) :Well", "\\1", temp) #remove ": WELL" at the end of the string
  temp <- sub("(.*) : Well", "\\1", temp) #remove ": WELL" at the end of the string
  temp <- stringr::str_trim(temp) #remove initial and final spaces


  return(temp)
}


#' Write well log dataframe to las file format
#'
#' @param df a dataframe. The curves names are taken from the column names of the dataframe.
#' @param name variable to be used as part of the .csv file name. Usually in the form \code{name = unique(.$id)},
#'     where \code{id} was used as grouping variable.
#' @param prefix a string with the prefix of the generated .csv files
#' @param verbose logical, define whether the function should print out the names of the writen .csv files.
#'     Dafault = TRUE.
#' @param do logical, define if the output is a data frame. Dafault = FALSE.
#'     This is required if used with function \code{dplyr::do} requires to return a dataframe.
#'     The obtaied dataframe can be dumped with \code{rm(dataframe_name)}.
#'
#' @return write .las file.
#'     Additionally returns a dataframe if \code{do = TRUE} .
#'     As side effect, if \code{verbose = TRUE} prints out the names of the writen .las files
#'
#'@importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' dumpv_df <- df %>%
#'	 dplyr::group_by(id) %>%
#'	 do(write.csv.group(. , name = unique(.$id), prefix = "logperm_", do = TRUE))
#'
#' rm(dump_df)
#' }
#'
#' @export
write_las <- function(df, name, prefix = "", verbose = TRUE, do = FALSE) {
  filename <- paste0(prefix, name, ".las")
  curvenames <- paste(colnames(df), sep = "\n")
  cat(paste("~Version Information",
            "VERS. 2.0  :CWLS Log ASCII Standard - Version 2.00",
            "# ----------------------------------",
            "~Curve Information",
            "", sep = "\n"), file = filename)
  cat(paste(colnames(df), "", sep = "\n"), file = filename, append = TRUE)
  cat(paste("# ----------------------------------",
            "~Ascii",
            "", sep = "\n"), file = filename, append = TRUE)

  utils::write.table(df, filename, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  if (verbose) print(filename)
  if (do) return(df)
}
