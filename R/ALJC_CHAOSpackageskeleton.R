#' @title CHAOSindex
#'
#' @description This function Enables generation of the CHAOS index from CGM data. CGM data must be cleaned and in the format as described in the README also see [CGMprocessing::cleanCGM()].
#'This is an concept index in development at the University of Alberta and University of Cardiff being trialed to rapidly and effectively identify and monitor high risk individuals with
#' Type 1 diabetes requiring advanced treatment. This package is in development.
#'
#' @returns Returns PDF of CGM traces and modelling prediction windows as well as csv files of the ARMIA model "accuracy" parameters, the Mean Absolute Percentage Errors (MAPE) for a 30 minute prediction window
#' as well as the chosen maximum prediction horizon (default to 90 min). Also outputs the file of the real values and predicted values from this prediction for 30 mins and the chosen maximum horizon.
#'
#' @param inputdirectory path to folder containing cleaned files. If data is pre-aggregated then use the full file path including file name of this file.
#' Preferred csv format but can read in others.
#'
#' @param aggregated TRUE/FALSE dictates if data is pre-aggregated or in individual separate files (usual for clinical trials)
#'
#' @param outputdirectory path to folder where output files will be stored
#'
#' @param maxhorizon A numeric value in minutes of the maximum horizon time you want to predict up until. Default is 90 (minutes).
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
#' @import forecastML
#' @import imputeTS
#' @import Hmisc
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @import ggplot2
#' @import hms
#' @importFrom tools file_path_sans_ext
#' @importFrom anytime anytime addFormats
#'
#' @author Alice Carr (University of Alberta), Peter Taylor (University of Cardiff), Steph Hanna (University of Cardiff)
#'
#' @export
#' @seealso
#' CGMprocessing cleanCGM

CHAOSindex <- function(inputdirectory,outputdirectory="output",aggregated=F,maxhorizon=90,saveplot=T) {
  #define lists to store outputs
  prediction30_output<-list()
  predictionmaxhorizon_output<-list()
  MAPE_output<-list()
  modelsummaryparms<-list()

  # output directory is created
  base::dir.create(outputdirectory, showWarnings = FALSE)

  if(aggregated==F){
  # Read in data: anticipated structure is a file containing clean CGM downloads
  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  }else if(aggregated==T){
    table_test<-rio::import(inputdirectory)
    files<-split(table_test,table_test$id)
  }

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

    # we must have approx 4 days of data to run the CHAOS index
    if(length(unique(as.Date(table$timestamp)))<4){
      stop(paste("There must be >4 days of data to run the CHAOS index. Check your data input.
            The file that generated this error is: ",Id))
    }

    #change sensor id to be patient id take from filename
    table$id <- Id

    #order by timestamp
    table <- table[base::order(table$timestamp), ]

    # plot a graph of this persons data to get an overview- overlay dates.

    # find what the interval in the data is ie. 5min for dexcom 15 min for libre
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp) / 60))

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


# data is now clean to move onto the next step. If there were gaps >20 mins these gaps will not have been imputed due to too much inaccuracy
    # for generating CHAOS we want to select days from the persons CGM that has complete glucose
    #so it doesnt matter if one day has lots of large time gaps, we wouldnt select this in impuation
    # we need a minimum of 4 **full** days to train and test the data on


# if it is 15min data pseudo code it to be "5 min" data ie. just repeat the rows
    if(interval>5){
      table<-slice(table,rep(1:n(), each = interval/5))
      warning(paste("15 min interval data. Pseudo imputing to 5 min data through repitition."))
    }

# step 2: select days that have no missing glucose after the imputation from cleanCGM ie. remove days that had big gaps
    BDataCGM <- table %>%
    dplyr::mutate(diff=abs(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
    dplyr::mutate(diff=ifelse(is.na(diff),0,diff)) %>%
    dplyr::mutate(biggap=ifelse(diff>20,1,NA))  %>%
    group_by(date) %>%
    tidyr::fill(biggap,.direction = "updown")  %>%
    ungroup() %>%
    dplyr::filter(is.na(biggap)) %>% # we use sensorreadings number to avoid selecting half days at start/end of sensor
    dplyr::select(-c(biggap,diff))

if (nrow(BDataCGM) == 0) {
      stop(paste("No dates in file without gaps in time.
      After cleaning, it was identified that there is not enough useable data.
      It is likely many days have large time gaps that were unable to be imputed.
      Go back and check your data. Exclude ID if neccessary.
      The file that generated this error is: ",Id))
      next
}

    if (length(unique(as.Date(BDataCGM$timestamp))) < 4) {
      stop(paste("Less than 4 days in file without gaps in time. Minimum of 4 days required for CHAOS Index.
      After cleaning, it was identified that there is not enough useable data.
      It is likely many days have large time gaps that were unable to be imputed.
      Go back and check your data. Exclude ID if neccessary.
      The file that generated this error is: ",Id))
      next
    }


#now check for missing dates
    BDataCGM <- BDataCGM %>%
      dplyr::mutate(consecutive = cumsum(c(1, diff(as.Date(date)) > 1)))


# Find the unique consecutive date sequences
consecutive_dates <- rle(BDataCGM$consecutive)

# there are 288 indexes (5mins) in 24hours.
# we train on 3 days = 864
# we want to predict 30min ahead
# we also want to predict up to the maxhorizon, default to 90,
# so need to compare to 18 further indexes = 882

valid_sequences <- as.list(which(consecutive_dates$lengths >= 864 + (maxhorizon/5)))

# Check if there are enough valid sequences
n_valid_sequences <- length(valid_sequences)

if (n_valid_sequences == 0) {
  stop(paste("No valid sequences with at least",864 + (maxhorizon/5),"timeseries values found.\n
       There must be",864 + (maxhorizon/5) ,"consecutive indexes of data to run the CHAOS index ~ 3days + maxhorizon:", maxhorizon,"mins.
       After cleaning, it was identified that there is not enough useable data.
       It is likely many days have large time gaps that were unable to be imputed.
       Go back and check your data. Exclude ID if neccessary.
       The file that generated this error is: ",Id))
  next
}

# if more than one valid sequence to run on Select a random valid sequence
random_sequence_index <- unlist(sample(as.list(valid_sequences), 1, replace = TRUE))

positions<-c(0,cumsum(consecutive_dates$lengths))

if(length(consecutive_dates$values)==1){
start_index<-1
end_index <- consecutive_dates$lengths
}else if(length(consecutive_dates$values)>1){
start_index<-positions[random_sequence_index] + 1
end_index <-positions[random_sequence_index+1]
}

# Extract the window from the time series
window <- BDataCGM[start_index:end_index,]


# Select a random start index within the window for generating prediction
random_start <- sample(1:(nrow(window) - (864 + (maxhorizon/5))), 1)

# Calculate the end index
random_end <- random_start + (864 + (maxhorizon/5)-1)

random_window_final<-window[random_start:random_end,] %>%
  dplyr::select(-consecutive)



#step 3: Generating index
# in development : Some Validations to be made, outputing of 4 files here
#


# separate out prediction and full dataset for graphs later
BDataCGM_predict <- random_window_final[1:864,]

BDataCGM_forcast <- random_window_final[1:(864 + (maxhorizon/5)),]

#UPDATE: use an seasonal arima model to learn on the X days of data
# i think the auto model was doing this anyway? however it important to specify?
# we need to specify seasonal as there may be different patterns in the night and day

sarima_model <- forecast::auto.arima(BDataCGM_predict$sensorglucose, seasonal = TRUE, lambda = "auto", seasonal.test = "ch")

# forcast maxhozizon indexes ahead - default 90min = 18 indexes
# forcast 6 indexes ahead - 30min
# Generate future positions
# Make predictions for the desired time window using the auto.arima model

predictions_maxhorizon <-  forecast::forecast(sarima_model, h = maxhorizon/5)

predictions_30min <-  forecast::forecast(sarima_model, h = 6)

# Extract the predicted values for the desired time window
prediction_window_maxhorizon<-random_window_final[865:(864+(maxhorizon/5)),]$timestamp
real_maxhorizon<-random_window_final[865:(864+(maxhorizon/5)),]$sensorglucose
predicted_values_maxhorizon <- as.numeric(predictions_maxhorizon$mean)
predicted_upper95_maxhorizon<-as.numeric(predictions_maxhorizon$upper[,2])
predicted_lower95_maxhorizon<-as.numeric(predictions_maxhorizon$lower[,2])

prediction_window_30<-random_window_final[865:870,]$timestamp
real_30<-random_window_final[865:870,]$sensorglucose
predicted_values_30 <- as.numeric(predictions_30min$mean)
predicted_upper95_30<-as.numeric(predictions_30min$upper[,2])
predicted_lower95_30<-as.numeric(predictions_30min$lower[,2])


# Create a data frame with the predicted values and the corresponding timestamps
prediction_df_maxhorizon<- data.frame(id=Id,
  timestamp = prediction_window_maxhorizon,
                            real_maxhorizon=real_maxhorizon,
                            predicted_value = predicted_values_maxhorizon,
                            predicted_upper95=predicted_upper95_maxhorizon,
                            predicted_lower95=predicted_lower95_maxhorizon,
                            horizonlength=as.numeric(maxhorizon)) %>%
 dplyr::mutate(index=row_number())

prediction_df_30 <- data.frame(id=Id,
                               timestamp = prediction_window_30,
                               real_30 = real_30,
                               predicted_value = predicted_values_30,
                               predicted_upper95=predicted_upper95_30,
                               predicted_lower95=predicted_lower95_30,
                               horizonlength=30) %>%
  dplyr::mutate(index=row_number())

#calculate the absolute percentage error of prediction for each forcast
absolute_percentage_error_maxhorizon <- abs((prediction_df_maxhorizon$real_maxhorizon - prediction_df_maxhorizon$predicted_value) / prediction_df_maxhorizon$real_maxhorizon) * 100
mape90_maxhorizon <- data.frame(id=Id,
  absolute_percentage_error=mean(absolute_percentage_error_maxhorizon),
  horizon=maxhorizon)

absolute_percentage_error_30 <- abs((prediction_df_30$real_30 - prediction_df_30$predicted_value) / prediction_df_30$real_30) * 100
mape30_maxhorizon <- data.frame(id=Id,
                                absolute_percentage_error=mean(absolute_percentage_error_30),
                                horizon=30)

## Store model accuracy parameters
# validation to be done - these may give better info than the mape
# AIC (Akaike Information Criterion): metric that balances the goodness of fit of the model with its complexity. It penalizes models with more parameters to prevent overfitting. A lower AIC value indicates a better trade-off between model fit and complexity.
#sigma^2: represents the estimated variance of the error term (residuals) in the ARIMA model. It provides a measure of the variability or dispersion of the residuals around the model's fitted values. A lower value indicates that the residuals are tightly clustered around the predicted values, suggesting a better fit of the model to the data.
#Log likelihood: refers to the logarithm of the likelihood function, which measures how well the ARIMA model fits the data. The higher the log likelihood, the better the fit of the model to the data.

modelsummary<-data.frame(id=Id,
                         AIC=sarima_model$aic,
                         `sigma_2`=sarima_model$sigma2,
                         `loglikihood`=sarima_model$loglik)


MAPE_output[[f]]<-merge(mape90_maxhorizon,mape30_maxhorizon, all=T)

predictionmaxhorizon_output[[f]]<-prediction_df_maxhorizon

prediction30_output[[f]]<-prediction_df_30

modelsummaryparms[[f]]<-modelsummary

# plot the training and prediction,
# zoom on prediction

graph2<- ggplot() +
  ggplot2::geom_rect(data = BDataCGM_forcast,aes(xmin=BDataCGM_forcast[865,]$timestamp,xmax=BDataCGM_forcast[(864+(maxhorizon/5)),]$timestamp,ymin=2,ymax=24,fill="Window to predict"),alpha=0.5)+
  ggplot2::geom_path(data=BDataCGM_forcast,aes(x =timestamp, y = sensorglucose)) +
  ggplot2::labs(x = "Time", y = "Glucose",title="Patients real glucose trace used in ARIMA forcast") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
    legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
    legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 10),  # Adjust the size of the legend text
    legend.title = element_text(size = 12, face = "bold")  # Adjust the size and style of the legend title
  ) +
  ggplot2::scale_y_continuous(limits = c(2,24),breaks=c(seq(2,24,2))) +
  ggplot2::scale_fill_manual("Key",values = c("indianred"))

  graph3<- ggplot2::ggplot() +
    ggplot2::geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = real_maxhorizon, colour="real data"), show.legend = T) +
    #geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = predicted_value,colour=paste("maxhorizon:",maxhorizon,"mins"),show.legend = T), colour="lightblue") +
    #geom_ribbon(data=prediction_df_maxhorizon,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="lightblue",alpha=0.3) +
    ggplot2::geom_path(data=prediction_df_30,aes(x =timestamp, y = predicted_value,colour="30mins"),show.legend = T) +
    ggplot2::geom_ribbon(data=prediction_df_30,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="indianred",alpha=0.3) +
    ggplot2::labs(x = "Time", y = "Glucose") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
      legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
      legend.key.size = unit(0.3, "cm"),  # Adjust the size of the legend keys
      legend.text = element_text(size = 6),  # Adjust the size of the legend text
      legend.title = element_text(size = 8, face = "bold")  # Adjust the size and style of the legend title
  )+ ggplot2::scale_color_manual("Key",values = c("indianred","black"))

  graph4<- ggplot2::ggplot() +
    ggplot2::geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = real_maxhorizon, colour="real data"),show.legend = T) +
    ggplot2::geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = predicted_value,colour=paste("maxhorizon:",maxhorizon,"mins")), show.legend = T) +
    ggplot2::geom_ribbon(data=prediction_df_maxhorizon,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="lightblue",alpha=0.3) +
  #geom_path(data=prediction_df_30,aes(x =timestamp, y = predicted_value,colour="30mins"),colour="indianred") +
  #geom_ribbon(data=prediction_df_30,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="indianred",alpha=0.3) +
    ggplot2::labs(x = "Time", y = "Glucose") +
    ggplot2::theme_minimal() +
    ggplot2:: theme(
      legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
      legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
      legend.key.size = unit(0.3, "cm"),  # Adjust the size of the legend keys
      legend.text = element_text(size = 6),  # Adjust the size of the legend text
      legend.title = element_text(size = 8, face = "bold")  # Adjust the size and style of the legend title
  ) + ggplot2::scale_color_manual("Key",values = c("lightblue","black"))

# combine all plots for outupt PDF
  plots <- suppressWarnings(cowplot::align_plots(graph1, graph2, graph3, graph4,align = 'v', axis = 'l'))
  # then build the bottom row
  bottom_row <- cowplot::plot_grid(graph3, graph4)

  # then combine with the top row for final plot
    graphoutput<-suppressWarnings(cowplot::plot_grid(graph1,graph2, bottom_row, ncol = 1,rel_heights = c(4,4,2,2)))

  graphoutput_title<-cowplot::ggdraw(cowplot::plot_grid(
    NULL,
    graphoutput,
    ncol = 1,
    rel_heights = c(0.07, 0.93)
  )) +
    cowplot::draw_label(paste("Patient ID:",Id), x = 0.5, y = 0.95, hjust = 0.5, fontface = "bold", size = 14)

if(saveplot==T){
  #save the plot, all patients
  ggplot2::ggsave(paste(outputdirectory,Id,"combinedplot_output.pdf"),graphoutput_title, width=8,height=10)
}
  }

output_params<-dplyr::bind_rows(modelsummaryparms[!is.null(modelsummaryparms)])
rio::export(output_params,paste0(outputdirectory,"modeloutputparams.csv"))

output_prediction_maximumhorizon<-dplyr::bind_rows(predictionmaxhorizon_output[!is.null(predictionmaxhorizon_output)])
rio::export(output_prediction_maximumhorizon,paste0(outputdirectory,"predictionsmaximumhorizon.csv"))

output_prediction_30mins<-dplyr::bind_rows(prediction30_output[!is.null(prediction30_output)])
rio::export(output_prediction_30mins,paste0(outputdirectory,"predictions30mins.csv"))

output_mape<-dplyr::bind_rows(MAPE_output[!is.null(MAPE_output)])
rio::export(output_mape,paste0(outputdirectory,"mape.csv"))

}
