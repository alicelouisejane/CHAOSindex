#' Generate CHAOS index
#'
#' Enables generation of the CHAOS index from CGM data.
#' This is an concept index in development at the University of Alberta and University of Cardiff
#' being trialed to rapidly and effectively identify and monitor high risk individuals with
#' Type 1 diabetes requiring advanced treatment.
# This package is in development.
# Use this code as directed.
#'
#' @author Alice Carr, Peter Taylor, Steph Hanna
#' @param inputdirectory A string file path to a file of raw CGM downloads.
#' @param outputdirectory A string file path to the output data file, folder will be generated if it doesn't exist. Default folder name: "output".
#' @param maxhorizon A numeric of the number of minutes for the maximum horizon we want to predict to in addition to the 30 minute horizon. Default to 90 minutes.
#' keep as default.
#' @param saveplot A TRUE/FALSE to save all associated plots for each patient as in a combined PDF. Default to TRUE. If FALSE no PDFs will be generated.
#' @return Description of the object returned by the function.
#' @import dplyr
#' @import here
#' @import ggplot2
#' @import tidyr
#' @import forecast
#' @import anytime
#' @import lubridate
#' @import forecastML
#' @import imputeTS
#' @import Hmisc
#' @import cowplot
#' @import rio
#' @import hms
#' @export



# # if package is not built download all files on GitHub and navigate to the file path as below
# cgm_dict<-rio::import("data/cgmvariable_dictionary.xlsx")

#
#inputdirectory<-"/Users/alicecarr/Desktop/C-path/TOMI 2/CHAOS Index/rawdownloads/"
#outputdirectory<-"/Users/alicecarr/Desktop/C-path/TOMI 2/CHAOS Index/testoutput"
#maxhorizon=90
#saveplot=T

# if package is not built you will likely need install packages (only if you havent already done this on your machine)
#and load the library by for example
#install.packages(dplyr)
#library(dplyr)


CHAOSindex <- function(inputdirectory,outputdirectory="output",maxhorizon=90,saveplot=T) {
  #define lists to store outputs
  prediction30_output<-list()
  predictionmaxhorizon_output<-list()
  MAPE_output<-list()
  modelsummaryparms<-list()

  #cgm variable dictionary in documentation data file. Edit source file as necessary if using other CGM
  file_path <- here::here("inst/extdata", "cgmvariable_dictionary.xlsx")
  cgm_dict<-rio::import(file_path)

  #Define all necessary parts

  # Read in data: anticipated structure is a file containing raw CGM downloads
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # output directory is created
  base::dir.create(outputdirectory, showWarnings = FALSE)

# Step 1: clean the CGM data, impute time gaps (if libre impute to 5 mins)
  for (f in 1:base::length(files)) {

    #id from filename
    Id <- tools::file_path_sans_ext(basename(files[f]))

    #handle various file extensions - colud be neccessary hopefully not
    # fileextension<-tools::file_ext(files[f])
    #
    # if(fileextension=="csv"){
    # table <-  base::suppressWarnings(rio::import(files[f], guess_max = 10000000,
    #                                              col_types = "c"))
    # }else if(fileextension=="xlsx"){
    #   table <-  base::suppressWarnings(rio::import(files[f], guess_max = 10000000))
    # }

    table <-  base::suppressWarnings(rio::import(files[f], guess_max = 10000000))


    #indicates what device we are using the data from, possibly important to keep track of
    device_vars<- cgm_dict[cgm_dict$old_vars %in% names(table), ]

    # rename the variables to standardised variables names
    colnames(table) <- dplyr::recode(
    colnames(table),
    !!!setNames(as.character(cgm_dict$new_vars), cgm_dict$old_vars))


# high and low limits from :
    #https://uk.provider.dexcom.com/sites/g/files/rrchkb126/files/document/2021-09/LBL017451%2BUsing%2BYour%2BG6%2C%2BG6%2C%2BUK%2C%2BEN%2C%2Bmmol_0.pdf
    # suggests libre 2 has different limits to libre 1 but this is not the case whan i look at my own data
    #https://www.freestyle.abbott/us-en/support/faq.html?page=device/freestyle-libre-2-system/faq/topic/reader

    if (grepl("dexcom",device_vars$type[1])) {
      #change instances of low/ high to sensor limits
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("low", sensorglucose, ignore.case = TRUE) ~ "2.2",
          grepl("high", sensorglucose, ignore.case = TRUE)  ~ "22.2",
          TRUE ~ table$sensorglucose
        ))
      )
      sensormin=2.2
      sensormax=24
    } else if (grepl("^libre$",device_vars$type[1])) {
      #DO NOT INCLUDE ANY OTHER RECODS OTHER THAN CGM ie. NOT scanglucose
      table <- dplyr::filter(table,recordtype==0)
      #add any scan glucoses to the sensor glucose
      table<-dplyr::select(table,-c(recordtype,scanglucose))
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("lo", sensorglucose, ignore.case = TRUE)  ~ "2.2",
          grepl("hi", sensorglucose, ignore.case = TRUE)  ~ "27.8",
          TRUE ~ table$sensorglucose
        ))
      )
      sensormin=2.2
      sensormax=28
    }

    #this get rid of the first lines in dexcom as all these rows miss a timestamp
    #but also gets rid of any problematic missing rows
    table <- dplyr::filter(table,!is.na(timestamp))

    # keep only variables of interest
    vars_to_keep <- dplyr::intersect(names(table), unique(cgm_dict$new_vars))

    table$device<-device_vars$type[1]

    table<-dplyr::select(table,all_of(vars_to_keep))

    # we must have approx 4 days of data to run the CHAOS index
    if(length(unique(as.Date(table$timestamp)))<4){
      stop(paste("There must be >4 days of data to run the CHAOS index. Check your data input.
            The file that generated this error is: ",Id))
    }

    #change sensor id to be patient id take from filename
    table$id <- Id

  # handling difficult date times
  #  date_parts <- stringr::str_extract(table$timestamp[1], "(?<=-)[^- ]{4}(?= )")


    if(is.character(table$timestamp)){
      table$timestamp <-as.POSIXct(lubridate::parse_date_time(table$timestamp, orders = c("dmy HMS","dmy HM","mdy HMS","mdy HM")),tz="UTC")
      table$timestamp <-anytime::anytime(table$timestamp, tz = "UTC")

    } else if(!is.character(table$timestamp)){
      table$timestamp <-anytime::anytime(table$timestamp, tz = "UTC")
    }

    #make sure glucose is numeric
    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))

    #order by timestamp
    table <- table[base::order(table$timestamp), ]

    # ensure order of the variables
    table <- table[, c("device", "id", "timestamp","sensorglucose")]

    #we round the dates to the nearest 60 seconds for consistencey ie so we dont have to the nearest second
    table$timestamp<-lubridate::round_date(table$timestamp,unit="60 seconds")

    # we first need to make the data on a standardised time scale
    table$timestamp<-lubridate::round_date(table$timestamp,unit="5 minutes")


# gap imputation below: also libre imputation to 5 min data
    if (grepl("libre",device_vars$type[1])) {
      #freestyle libre has 15 min gaps
    #we fill in the time gaps for every 5 min - this shouldn't effect the result of CHAOS index
      table<-forecastML::fill_gaps(table, date_col = 3, frequency="5 min", groups = NULL, static_features = NULL) %>%
      fill(1:2, .direction = "updown")

    # we impute sensor glucose in these gaps. Any gaps that are greater than 6 (6*15=30mins) are not imputed,
      # ie. we leave glucose as missing
      table<-imputeTS::na_kalman(table, model="StructTS",smooth = T,maxgap = 6) %>%
        dplyr::mutate(across(sensorglucose, \(x) round(x, digits = 1)))


    }else if(grepl("dexcom",device_vars$type[1])) {

      #we fill in any gaps where there has been dropout
      table<-forecastML::fill_gaps(table, date_col = 3, frequency="5 min", groups = NULL, static_features = NULL) %>%
        fill(1:2, .direction = "updown")

      # we impute sensor glucose in these gaps. Any gaps that are greater than 25 (5*6=30mins) are not imputed,
      # ie. we leave glucose as missing here
      table<-imputeTS::na_kalman(table, model="StructTS",smooth = T,maxgap = 6) %>%
        dplyr::mutate(across(sensorglucose, \(x) round(x, digits = 1)))

    }


    # plot a graph of this persons data to get an overview- overlay dates.

    graph1<-table %>%
      mutate(date=as.Date(timestamp)) %>%
      mutate(time=hms::as_hms(timestamp)) %>%
      ggplot(aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = sensorglucose)) +
      geom_path(aes(group=as.factor(date),colour=as.factor(date)), colour="grey")+
      labs(x = "Time", y = "Glucose", title=paste("Summary of CGM wear over:",length(unique(as.Date(table$timestamp))),"days")) +
      theme_minimal() +
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
      stat_summary(aes(fill = "Median hilow"),fun.data = median_hilow, geom = "ribbon",alpha = 0.5, colour = "darkblue", show.legend = T)+
      stat_summary(aes(fill = "IQR"),fun.data = function(x) {
        y <- quantile(x, c(0.25, 0.75))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", colour = "lightblue", alpha = 0.5,show.legend = T) +
      theme(
        legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
        legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
        legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend keys
        legend.text = element_text(size = 10),  # Adjust the size of the legend text
        legend.title = element_text(size = 12, face = "bold")  # Adjust the size and style of the legend title
      ) +
      scale_fill_manual("Key",values = c("lightblue","darkblue")) +
      scale_y_continuous(limits = c(2,(sensormax)),breaks=c(seq(2,sensormax,2)))


# data is now clean to move onto the next step. If there were gaps >30 mins these gaps will not have been imputed due to too much inaccuracy
    # for generating CHAOS we want to select days from the persons CGM that has complete glucose
    #so it doesnt matter if one day has lots of large time gaps, we wouldnt select this in impuation
    # we need a minimum of 4 **full** days to train and test the data on

# step 2: select days that have no missing glucose after this imputation ie. remove days that had big gaps
    BDataCGM <- table %>%
    mutate(date=as.Date(timestamp)) %>%
      group_by(date) %>%
      mutate(removeifmissing=mean(sensorglucose,na.rm = F)) %>%
      ungroup() %>%
      filter(!is.na(removeifmissing)) %>% # we use sensorreadings number to avoid selecting half days at start/end of sensor
      select(-c(date,removeifmissing))

if (nrow(BDataCGM) == 0) {
      stop(paste("No dates in file without gaps in time.
      After cleaning, it was identified that there is not enough useable data.
      It is likely many days have large time gaps that were unable to be imputed.
      Go back and check your data. Exclude ID if neccessary.
      The file that generated this error is: ",Id))
      next
    }

# make the date and time separate - it doesnt matter what the date is now for this step
    # put missing placeholders for the missing dates to determine if consecutive
BDataCGM<-BDataCGM %>%
  mutate(DeviceDaysFromEnroll=as.Date(timestamp)) %>%
   mutate(DeviceTm =format(timestamp, format = "%H:%M:%S")) %>%
   forecastML::fill_gaps(date_col = 3, frequency="5 min", groups = NULL, static_features = NULL)

# development? : what if we have months within the same download? a: Split the data frame into consecutive 14-day periods?
#
# split_df <- split(BDataCGM, cut(BDataCGM$DeviceDaysFromEnroll, "14 days"))

# Find the unique consecutive date sequences
consecutive_dates <- rle(!is.na(BDataCGM$sensorglucose))


# there are 288 indexes (5mins) in 24hours.
# we train on 3 days = 864
# we want to predict 30min ahead
# we also want to predict up to the maxhorizon, default to 90,
# so need to compare to 18 further indexes = 882

valid_sequences <- which(consecutive_dates$lengths >= 864 + (maxhorizon/5))

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

# Select a random valid sequence
random_sequence_index <- sample(valid_sequences, 1)

if(random_sequence_index==1){
start_index<-1
end_index <- start_index + consecutive_dates$lengths[random_sequence_index] - 1
}else if(random_sequence_index!=1){
start_index <- sum(consecutive_dates$lengths[1:(random_sequence_index - 1)]) + 1
end_index <- start_index + consecutive_dates$lengths[random_sequence_index] - 1
  }

# Extract the window from the time series
window <- BDataCGM[start_index:end_index,]


# Select a random start index within the window for generating prediction
random_start <- sample(1:(nrow(window) - (864 + (maxhorizon/5))), 1)

# Calculate the end index
random_end <- random_start + (864 + (maxhorizon/5)-1)

random_window_final<-window[random_start:random_end,]


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

predictions_maxhorizon <- forecast(sarima_model, h = maxhorizon/5)

predictions_30min <- forecast(sarima_model, h = 6)

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

# store model accuracy parameters
# validation to be done - these may give better info than the mape
# AIC (Akaike Information Criterion): metric that balances the goodness of fit of the model with its complexity. It penalizes models with more parameters to prevent overfitting. A lower AIC value indicates a better trade-off between model fit and complexity.
#sigma^2: represents the estimated variance of the error term (residuals) in the ARIMA model. It provides a measure of the variability or dispersion of the residuals around the model's fitted values.
#Log likelihood: It refers to the logarithm of the likelihood function, which measures how well the ARIMA model fits the data. The higher the log likelihood, the better the fit of the model to the data.

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
  geom_rect(data = BDataCGM_forcast,aes(xmin=BDataCGM_forcast[865,]$timestamp,xmax=BDataCGM_forcast[(864+(maxhorizon/5)),]$timestamp,ymin=2,ymax=sensormax,fill="Window to predict"),alpha=0.5)+
  geom_path(data=BDataCGM_forcast,aes(x =timestamp, y = sensorglucose)) +
  labs(x = "Time", y = "Glucose",title="Patients real glucose trace up used in ARIMA forcast") +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
    legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
    legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 10),  # Adjust the size of the legend text
    legend.title = element_text(size = 12, face = "bold")  # Adjust the size and style of the legend title
  ) +
  scale_y_continuous(limits = c(2,(sensormax)),breaks=c(seq(2,sensormax,2))) +
  scale_fill_manual("Key",values = c("indianred"))

  graph3<- ggplot() +
    geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = real_maxhorizon, colour="real data"), show.legend = T) +
    #geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = predicted_value,colour=paste("maxhorizon:",maxhorizon,"mins"),show.legend = T), colour="lightblue") +
    #geom_ribbon(data=prediction_df_maxhorizon,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="lightblue",alpha=0.3) +
  geom_path(data=prediction_df_30,aes(x =timestamp, y = predicted_value,colour="30mins"),show.legend = T) +
    geom_ribbon(data=prediction_df_30,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="indianred",alpha=0.3) +
  labs(x = "Time", y = "Glucose") +
    theme_minimal() +
  theme(
    legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
    legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
    legend.key.size = unit(0.3, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 6),  # Adjust the size of the legend text
    legend.title = element_text(size = 8, face = "bold")  # Adjust the size and style of the legend title
  )+ scale_color_manual("Key",values = c("indianred","black"))

  graph4<- ggplot() +
  geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = real_maxhorizon, colour="real data"),show.legend = T) +
  geom_path(data=prediction_df_maxhorizon,aes(x =timestamp, y = predicted_value,colour=paste("maxhorizon:",maxhorizon,"mins")), show.legend = T) +
  geom_ribbon(data=prediction_df_maxhorizon,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="lightblue",alpha=0.3) +
  #geom_path(data=prediction_df_30,aes(x =timestamp, y = predicted_value,colour="30mins"),colour="indianred") +
  #geom_ribbon(data=prediction_df_30,aes(x =timestamp, y=predicted_value,ymin = predicted_lower95,ymax = predicted_upper95),fill="indianred",alpha=0.3) +
  labs(x = "Time", y = "Glucose") +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
    legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
    legend.key.size = unit(0.3, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 6),  # Adjust the size of the legend text
    legend.title = element_text(size = 8, face = "bold")  # Adjust the size and style of the legend title
  ) + scale_color_manual("Key",values = c("lightblue","black"))

# combine all plots for outupt PDF
  plots <- suppressWarnings(cowplot::align_plots(graph1, graph2, graph3, graph4,align = 'v', axis = 'l'))
  # then build the bottom row
  bottom_row <- cowplot::plot_grid(graph3, graph4)

  # then combine with the top row for final plot
  graphoutput<-suppressWarnings(cowplot::plot_grid(graph1,graph2, bottom_row, ncol = 1,rel_heights = c(4,4,2,2)))

  graphoutput_title<-ggdraw(cowplot::plot_grid(
    NULL,
    graphoutput,
    ncol = 1,
    rel_heights = c(0.07, 0.93)
  )) +
    draw_label(paste("Patient ID:",Id), x = 0.5, y = 0.95, hjust = 0.5, fontface = "bold", size = 14)

if(saveplot==T){
  #save the plot, all patients
  ggsave(paste(outputdirectory,Id,"combinedplot_output.pdf"),graphoutput_title, width=8,height=10)
}
  }

output_params<-bind_rows(modelsummaryparms[!is.null(modelsummaryparms)])
rio::export(output_params,paste(outputdirectory,Id,"modeloutputparams.csv"))

output_prediction_maximumhorizon<-bind_rows(predictionmaxhorizon_output[!is.null(predictionmaxhorizon_output)])
rio::export(output_params,paste(outputdirectory,Id,"predictionsmaximumhorizon.csv"))

output_prediction_30mins<-bind_rows(prediction30_output[!is.null(prediction30_output)])
rio::export(output_params,paste(outputdirectory,Id,"predictions30mins.csv"))

output_mape<-bind_rows(MAPE_output[!is.null(MAPE_output)])
rio::export(output_params,paste(outputdirectory,Id,"mape.csv"))

}
