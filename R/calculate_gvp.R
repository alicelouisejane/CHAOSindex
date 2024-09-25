#' @title GVP
#'
#' @description This function enables generation Glycemic Variability Percentage (GVP) from CGM data. CGM data must be cleaned and in the format as described in the README also see [CGMprocessing::cleanCGM()].
#' This is based on the logic from Peyser 2018. The length of the CGM temporal trace over a given interval of time depends on the degree of glycemic variability: temporal traces with high glycemic variability
#' have greater lengths than traces with low glycemic variability.
#' Mathematically, this is similar to the problem posed by Mandelbrot regarding the proper measurement of the coastline of Great Britain or other coastal areas with a high degree of tortuosity.
#'
#' @returns Returns a csv files of GVP for each individual.
#'
#' @param inputdirectory path to folder containing cleaned files. If data is pre-aggregated then use the full file path including file name of this file.
#' Preferred csv format but can read in others.
#'
#' @param aggregated TRUE/FALSE dictates if data is pre-aggregated or in individual separate files (usual for clinical trials)
#'
#' @param magedef Defining the threshold used in MAGE calculation.  MAGE is an arithmetic average of either the upward or downward
#' of all glycemic excursions exceeding the threshold (standard deviation of blood glucose obtained from all blood glucose
#' concentrations within 24-hour period). Default is 1 standarddevation ("1sd"), options are 1.5 SD ("1.5sd") , 2 SD ("2sd")
#' or other can be specifed as a numeric
#'
#' @param congan Specificing the n number of hours in CONGA(n). Default is the numeric 1. CONGA(n) represents the SD
#' of all valid differences between a current observation and an observation (n) hours earlier
#'
#' @param outputdirectory path to folder where output files will be stored
#'
#' @param saveplot A TRUE/FALSE to save all associated plots for each patient as in a combined PDF. Default to TRUE. If FALSE no PDFs will be generated.
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate summarise n lead across contains filter select group_by inner_join slice ungroup arrange bind_rows rename
#' @import dplyr
#' @import tidyr
#' @import pracma
#' @import forecast
#' @import anytime
#' @import lubridate
#' @importFrom zoo rollapply zoo
#' @import pastecs
#' @import Hmisc
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @import ggplot2
#' @import hms
#' @importFrom tools file_path_sans_ext
#' @importFrom anytime anytime addFormats
#'
#' @author Alice Carr (University of Alberta)
#'
#' @export
#' @seealso
#' CGMprocessing cleanCGM CHAOSindex

# Function to calculate GVP
calculate_gvp <- function(inputdirectory,
                          outputdirectory,
                          aggregated=F,
                          magedef="1sd",
                          congan = 1,
                          saveplot=T) {

  if(aggregated==F){
    # Read in data: anticipated structure is a file containing clean CGM downloads
    files <- base::list.files(path = inputdirectory, full.names = TRUE)
  }else if(aggregated==T){
    table_test<-rio::import(inputdirectory)
    files<-split(table_test,table_test$id)
  }

  gvpupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(files)))
  base::colnames(gvpupload) <- base::rep("Record", base::length(files))

  # Step 1: read in the CGM data, ensure time gaps imputed
  for (f in 1:base::length(files)) {

    if(aggregated==F){
      #id from filename
      Id <- tools::file_path_sans_ext(basename(files[f]))
      print(Id)
      table <-  base::suppressWarnings(rio::import(files[f], guess_max = 10000000))
    }else if(aggregated==T){
      table <-  files[[f]]
      Id <- unique(table$id)
      table <- unique(table)
      print(Id)
    }

    if(!all(c("id","date","timestamp","sensorglucose") %in% names(table))){
      stop(paste("Unexpected file input. See README for correct file structure."))
    }

    # we must have approx 7 days of data to run - similar logic based on GMI?
    if(length(unique(as.Date(table$timestamp)))<7){
      stop(paste("There must be at least 7 days of data to run the GVP. Check your data input.
            The file that generated this error is: ",Id))
      next
    }

    #change sensor id to be patient id take from filename
    table$id <- Id
    print(Id)

    #order by timestamp
    table <- table[base::order(table$timestamp), ]

    # find what the interval in the data is ie. 5min for dexcom 15 min for libre
    #Sampling Frequency Impact: Larger intervals (e.g., 15 minutes) can smooth out short-term fluctuations in glucose levels, potentially underestimating variability compared to shorter intervals (e.g., 5 minutes).
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp) / 60))

    if(interval>5){
      warning(paste("Sampling Frequency Impact: Larger intervals (e.g., 15 minutes) can smooth out short-term fluctuations in glucose levels, potentially underestimating variability compared to shorter intervals (e.g., 5 minutes)"))
    }


    data <- table %>%
      dplyr::mutate(date = as.Date(timestamp)) %>%
      dplyr::mutate(time = hms::as_hms(timestamp))

    summary_table<-data %>%
      dplyr::mutate(date = as.Date(timestamp)) %>%
      dplyr::mutate(time = hms::as_hms(timestamp)) %>%
      dplyr::mutate(test=hms::round_hms(time, secs = interval*60))

    graph1<- ggplot2::ggplot(data = data, ggplot2::aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = as.numeric(sensorglucose))) +
      ggplot2::geom_path(ggplot2::aes(group = interaction(as.factor(date), id), colour = as.factor(date)), colour = "grey") +
      ggplot2::labs(x = "Time", y = "Glucose", title=paste("Summary of CGM wear over:",length(unique(as.Date(table$timestamp))),"days")) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
      # median hi low and IQR could be the same as each other...
      ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "10-90th Centile"),colour="transparent", fun.data = function(x) {
        y <- stats::quantile(x, c(0.1, 0.9))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", alpha = 0.5, show.legend = T) +
      ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "25-75th Centile"),colour="transparent", fun.data = function(x) {
        y <- stats::quantile(x, c(0.25, 0.75))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", alpha = 0.5, show.legend = T) +
      ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "Median"),colour="transparent", fun.data = function(x) {
        y <- stats::quantile(x, c(0.5, 0.5))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", alpha = 0.5, show.legend = T) +
      ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose)), fun = "median", geom = "line", alpha = 0.5, colour = "orange",linewidth=1, show.legend = F) +
      ggplot2::theme(
        legend.position = c(0.85, 0.9), # Adjust the position of the legend box (top-right corner)
        legend.background = ggplot2::element_rect(fill = "white", color = "black"), # Customize the legend box
        legend.key.size = ggplot2::unit(0.5, "cm"), # Adjust the size of the legend keys
        legend.text = ggplot2::element_text(size = 10), # Adjust the size of the legend text
        legend.title = ggplot2::element_text(size = 12, face = "bold") # Adjust the size and style of the legend title
      ) +
      ggplot2::scale_fill_manual("Key", values = c("lightblue", "darkblue","orange"),aesthetics = c("fill")) +
      ggplot2::scale_y_continuous(limits = c(2, 24), breaks = c(seq(2, 24, 2)))

    data2 <- table %>%
      dplyr::mutate(date = as.Date(timestamp)) %>%
      mutate(start_time =min(table$timestamp)) %>%
      dplyr::mutate(time = as.numeric(difftime(table$timestamp, start_time, units = "min")))

    median_glucose_fordemo=median(data2$sensorglucose)


    graph2<- ggplot2::ggplot(data = data2, ggplot2::aes(x = time, y = as.numeric(sensorglucose))) +
      ggplot2::geom_path(colour = "grey") +
      ggplot2::labs(x = "Time (hours)", y = "Glucose", title=paste("Continuous trace of CGM over:",length(unique(as.Date(table$timestamp))),"days")) +
      ggplot2::theme_minimal() +
      ggplot2::stat_summary(data = data2,ggplot2::aes(x = time, y = as.numeric(sensorglucose),fill = "CGM glucose trace"),colour="transparent", fun.data = function(x) {
        y <- stats::quantile(x, c(0.5, 0.5))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", alpha = 0.5, show.legend = T) +
      ggplot2::stat_summary(data = data2,ggplot2::aes(x = time, y = as.numeric(sensorglucose),fill = "Minimum line length"),colour="transparent", fun.data = function(x) {
        y <- stats::quantile(x, c(0.5, 0.5))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", alpha = 0.5, show.legend = T) +
      ggplot2::scale_x_continuous(breaks = seq(0, max(data2$time), by = 24*60),
                         labels = seq(0, max(data2$time)/60, by = 24)) +
      ggplot2::geom_hline(yintercept = median_glucose_fordemo,colour="red") +
      ggplot2::theme(
        legend.position = c(0.85, 0.9), # Adjust the position of the legend box (top-right corner)
        legend.background = ggplot2::element_rect(fill = "white", color = "black"), # Customize the legend box
        legend.key.size = ggplot2::unit(0.5, "cm"), # Adjust the size of the legend keys
        legend.text = ggplot2::element_text(size = 10), # Adjust the size of the legend text
        legend.title = ggplot2::element_text(size = 12, face = "bold") # Adjust the size and style of the legend title
      ) +
      ggplot2::scale_fill_manual("Key", values = c("grey", "red"),aesthetics = c("fill")) +
      ggplot2::scale_y_continuous(limits = c(2, 24), breaks = c(seq(2, 24, 2)))



    # Ensure the timestamp is in POSIXct format
    table$timestamp <- as.POSIXct(table$timestamp, format = "%Y-%m-%d %H:%M:%S")

    #IMPORTANT: Convert into mg/dl for GVP equation
    #(check first it is not already in mg/dl, test if less than 30. 30 is incredibly low in mg/dl but very high in mmol/l, the sensor limits cant reach these values so this is a robust check)
    table$sensorglucose <- ifelse(table$sensorglucose<30,round(table$sensorglucose * 18, digits = 2),table$sensorglucose)

    # Calculate the differences in time (in minutes)
    time_diff <- as.numeric(diff(table$timestamp, units = "mins"))

    # Calculate the differences in glucose levels
    glucose_diff <- diff(table$sensorglucose)

    # Calculate the length of the actual trace using Pythagorean theorem
    L <- sum(sqrt(time_diff^2 + glucose_diff^2))

    # Adjust ideal line length L0 by subtracting time differences corresponding to gaps
    L0 <- sum(time_diff)


    # Calculate GVP as detailed in Peyser et al 2018.
    # related mathematically to the proposals by Ko- vatchev et al. and Whitelaw et al.
    GVP <- ((L / L0) - 1) * 100

    # Add to data frame
    gvpupload["subject_id", f] <- Id

    # GVP
    gvpupload["gvp", f] <- round(GVP,digits=2)

    # also calculate other measures of variability as comparison based on equatiosn in analyseCGM
    # standard deviation
    gvpupload["standard_deviation", f] <- base::round(stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    # CV is the coeffcient of variarion
    # Note: SD is highly influenced by the mean glucose – someone with a higher mean glucose will have a higher SD.
    # the CV divides the SD/mean x100. This division helps “correct” and normalize glucose variability, allowing us to set a single variability goal that applies to people with different mean glucose levels.
    gvpupload["cv", f] <- base::round((stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))])) / base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    gvpupload["cv (%)", f] <- (base::round((stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))])) / base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2))*100

    # Calculate MAGE
    # https://go.gale.com/ps/i.do?id=GALE%7CA252447314&sid=googleScholar&v=2.1&it=r&linkaccess=abs&issn=15209156&p=AONE&sw=w&userGroupName=loyoland_main
    # Smooth data using an exponentially weighted 9 point moving average, calculate SD of unsmoothed data.
    #require 12 hours of data for this
    #if (base::round(base::length(table$sensorglucose) / (3600 / interval)) > 12 & !is.null(length(table$sensorglucose))) {
    if (!is.null(length(table$sensorglucose))) {
      table$smoothed <- base::as.numeric(zoo::rollapply(zoo::zoo(table$sensorglucose),
                                                        9, function(x) {
                                                          c(1, 2, 4, 8, 16, 8, 4, 2, 1) %*%
                                                            (x / 46)
                                                        },
                                                        fill = NA
      ))
      table$smoothed[1:4] <- base::mean(stats::na.omit(table$sensorglucose[1:4]))
      table$smoothed[(base::length(table$smoothed) - 3):base::length(table$smoothed)] <- base::mean(table$sensorglucose[(base::length(table$sensorglucose) - 3):base::length(table$sensorglucose)])
      # above is because smoothing doesnt work on first and last 4 values so sub in for their mean

      # SD of the
      sd <- stats::sd(table$sensorglucose)



      tryCatch({
        # Identify turning points, peaks, and nadirs.
        tpoints <- pastecs::turnpoints(table$smoothed)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      tpointposition<- pastecs::extract(tpoints, no.tp = 0, peak = 1, pit = -1)
      peaks <- base::which(tpointposition==1)
      pits <- base::which(tpointposition==-1)



      # Calculate the difference between each nadir and its following peak. If the
      # data starts on a peak, remove it. Otherwise remove the final pit to create an even number of pits and peaks.
      if (tpoints[["firstispeak"]] == TRUE && base::length(peaks) != base::length(pits)) {
        peaks <- peaks[2:base::length(peaks)]
      } else if (tpoints[["firstispeak"]] == FALSE && base::length(peaks) != base::length(pits)) {
        pits <- pits[1:(base::length(pits) - 1)]
      }
      differences <- table$sensorglucose[peaks] - table$sensorglucose[pits]
      # if differecen between adjacent turning points are < 1sd (usually 1 but can be others) then it is not a turning point,
      #if there are no turning points then MAGE will be NaN
      #MAGE is Na if there wasnt 12 hours of data for this? maybe


      # Calculate the average of the differences greater than the entire dataset SD, 2SD, etc
      if (magedef == "1sd") {
        gvpupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > sd)])), digits = 2)

      }else if (magedef == "1.5sd") {
        gvpupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > (sd * 1.5))])), digits = 2)
      }else if (magedef == "2sd") {
        gvpupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > (sd * 2))])), digits = 2)
      }else {
        gvpupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > magedef)])), digits = 2)
      }
    }

    # J-index:combination of information from mean and SD of all glucose values
    gvpupload["j_index", f] <- base::round((0.001 * (base::mean(table$sensorglucose, na.rm = T) + stats::sd(table$sensorglucose, na.rm = T))^2), digits = 2)

    # CONGA:continuous overlapping net glycaemic action
    n <- (congan * 3600)
    conga.times <- table$timestamp + n
    conga.times <- conga.times[!is.na(conga.times)]
    conga.times <- conga.times[base::order(conga.times)]
    conga.times <- conga.times[base::which(conga.times %in% table$timestamp)]
    begin.times <- conga.times - n
    suppressWarnings(congas <- table$sensorglucose[base::which(table$timestamp %in% conga.times)] -
                       table$sensorglucose[base::which(table$timestamp %in% begin.times)])
    gvpupload[base::paste0("conga_", congan), f] <- base::round(stats::sd(congas, na.rm = T), digits = 2)



    # MODD: mean of daily difference
    # if there is not a full day of data then the MODD will be NAN e when doing exercise analysis
    table$time <- lubridate::round_date(
      table$timestamp,
      "5 minutes"
    )
    table$time <- base::strftime(table$time,
                                 format = "%H:%M",
                                 tz = "UTC"
    )
    moddtable <- base::data.frame(base::matrix(
      ncol = 2,
      nrow = base::length(unique(table$time))
    ))
    base::colnames(moddtable) <- c("time", "mean_differences")

    moddtable$time <- base::unique(table$time)
    # For each time, calculate differences (absolute values) and average them.
    for (r in 1:nrow(moddtable)) {
      moddtable$mean_differences[r] <- base::mean(base::abs(base::diff(table$sensorglucose[base::which(table$time ==
                                                                                                         moddtable$time[r])])))
    }
    # Average the averages.
    gvpupload["modd", f] <- base::round(base::mean(stats::na.omit(moddtable$mean_differences)), digits = 2)

    # combine all plots for outupt PDF

    graphoutput_title<-cowplot::ggdraw(cowplot::plot_grid(
      NULL,
      graph1,graph2,
      ncol = 1,
      rel_heights = c(0.2,1,1)
    )) +
      cowplot::draw_label(paste0("Patient ID: ",Id, "\n GVP: ",round(GVP,digits=2), "%"), x = 0.5, y = 0.95, hjust = 0.5, fontface = "bold", size = 14)

    if(saveplot==T){
      #save the plot, all patients
      ggplot2::ggsave(paste0(outputdirectory,Id,"_GVP_plot_output.pdf"),graphoutput_title, width=10,height=8)
    }

  }

  # Write file.
  gvpupload <-
    base::cbind("Variable / Field Name" = rownames(gvpupload), gvpupload)
  gvpupload <- base::as.data.frame(base::t(gvpupload))
  gvpupload <- gvpupload[-1, ]
  filename <- base::paste0(outputdirectory,"CGM_variability_upload", ".csv")
  rio::export(gvpupload, file=paste0(filename))

  }



