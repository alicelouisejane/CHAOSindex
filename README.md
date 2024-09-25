<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

------------------------------------------------------------------------

# Overview

Package supplying new metrics from non-invasive Continuous Glucose
Monitoring technology to enable quantification of variability/volatility
in an individuals glucose levels. These metrics will aim to more rapidly
and effectively identify and monitor high risk individuals with type 1
diabetes requiring advanced treatment. Data structures should follow
that as detailed in the associated package:
[CGMprocessing](https://github.com/alicelouisejane/CGMprocessing).

------------------------------------------------------------------------

# Installation

You can install the latest version of `CHAOSindex` directly from GitHub
using the `remotes` package. First, make sure you have `remotes`
installed:

    install.packages("remotes")

Then, you can install `CHAOSindex` using the following command:

    remotes::install_github("alicelouisejane/CHAOSindex")

If you encounter any issues during installation, please [open an
issue](https://github.com/alicelouisejane/CHAOSindex/issues) on the
GitHub repository page.

### Functions

See parameters for explanations of the arguments per each function. You
will find this in the man file on this GitHub:

-   [CHAOSindex](#CHAOSindex)

-   [calculate\_gvp](#calculate_gvp)

### Example File Structures

-   **/data-clean**: data ran through *cleanCGM* function or own
    cleaning pipeline to be inline with the format of a [cleanCGM
    output](https://github.com/alicelouisejane/CGMprocessing?tab=readme-ov-file#cleancgm-output)

-   **/data-split**: (optional) cleaned data that may have been windowed
    to matched to comparator like a blood analyte/ time post
    intervention etc

------------------------------------------------------------------------

## <a id="intro"></a> Measures of variability from Continuous Glucose Monitoring Data

------------------------------------------------------------------------

❗**Important**: This package is optimized for clean Dexcom or Libre CGM
data but can work with cleaned pre-aggregated clinical study CGM data.
Read the descriptions of the input parameters in the man file for this
package above or use the help() function within R studio for how to use
aggregated=T.❗

------------------------------------------------------------------------

There are multiple validated measures of variability based on CGM
including:

<table>
<colgroup>
<col style="width: 5%" />
<col style="width: 66%" />
<col style="width: 27%" />
</colgroup>
<thead>
<tr class="header">
<th>Metric</th>
<th>Definition</th>
<th>Suggested in International Consensus for CGM</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Standard deviation</td>
<td>Standard deviation of sensor glucose</td>
<td>Yes</td>
</tr>
<tr class="even">
<td>Coefficient of Variation</td>
<td>The CV divides the SD/mean x100. This division helps “correct” and
normalize glucose variability, allowing us to set a single variability
goal that applies to people with different mean glucose levels as SD is
highly influenced by the mean glucose – someone with a higher mean
glucose will have a higher SD.</td>
<td>Yes</td>
</tr>
<tr class="odd">
<td>Total AUC</td>
<td>Total area under the glucose curve</td>
<td>Yes</td>
</tr>
<tr class="even">
<td>Mean Amplitude Glycemic Excursion</td>
<td>MAGE is an arithmetic average of either the upward or downward of
all glycemic excursions exceeding the threshold (standard deviation of
blood glucose obtained from all blood glucose concentrations within
24-hour period). Option to asses average of the differences greater than
either entire dataset SD, 2SD, etc</td>
<td>Yes</td>
</tr>
<tr class="odd">
<td>J index</td>
<td>J index is a measure of quality of glycemic control based on the
combination of information from the mean and SD calculated as 0.001 ×
(mean + SD) doi: 10.1055/s-2007-979906.</td>
<td>Yes</td>
</tr>
<tr class="even">
<td>Continuous overlapping net glycemic action</td>
<td>CONGA (n), where n indicate number of hours being assessed (ie. 1 =1
hour), CONGA(n) represents the SD of all valid differences between a
current observation and an observation (n) hours earlier. <a
href="doi:10.1089/dia.2005.7.253"
class="uri">doi:10.1089/dia.2005.7.253</a></td>
<td>Yes</td>
</tr>
<tr class="odd">
<td>The mean of daily differences</td>
<td>MODD index provides an estimation of interday glycemic variability.
This parameter is calculated as the mean of absolute differences between
glucose values at corresponding time points of consecutive days. Doi:
10.1007/BF01218495</td>
<td>Yes</td>
</tr>
<tr class="even">
<td>Glycemic Variability Percentage</td>
<td>Defined as ((L / L0) - 1) * 100. Where L is given by the summation
over all n line elements based on decomposition into horizontal (Dx) and
vertical (Dy) components and application of the Py- thagorean theorem.
And L0 is ideal line length L0 for a given temporal duration.  The
length of the CGM temporal trace over a given interval of time depends
on the degree of glycemic variability: temporal traces with high
glycemic variability have greater lengths than traces with low glycemic
variability.</td>
<td><a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5846572">No –
developed after the first consensus, Published and demonstrated its
action by [Peyser
2018](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5846572)</a></td>
</tr>
</tbody>
</table>

There may also be other possible ways to better identify variability of
an individuals glucose, which better relate to the **unpredictability**
of glucose. Unpredictability of glucose is often anecdotally described
by individuals with type 1 diabetes alongside patient reported outcome
measures (PROMS) associated with feelings of distress and lower quality
of life. There is an unmet need for CGM metrics that capture the effects
associated with these PROMs as well as measures of beta cell function.

Continuous Glucose Monitoring data is a time series. Time series data is
very common, and day to day our lives depend on it -meaning that methods
to analyse time series data are very well characterized. For example,
how we forecast the stock market, the weather and even predict problems
with our heart rate- it all starts with time series data. This package
aims to develop new metrics from non-invasive Continuous Glucose
Monitoring technology to enable quantification of unpredictability in an
individuals glucose levels. These metrics will aim to generate CGM
metrics that reflect PROMs and beta cell function and help to more
rapidly and effectively identify and monitor high risk individuals with
type 1 diabetes requiring advanced treatment.

### <a id="inputstructure"></a> Input data structure

Files must be cleaned and in the structure as detailed in
[CGMprocessing::cleanCGM()
output](https://github.com/alicelouisejane/CGMprocessing?tab=readme-ov-file#cleancgm-output).
More information about cleaning CGM data can be found in the
[CGMprocessing
Package](https://github.com/alicelouisejane/CGMprocessing?tab=readme-ov-file#cleancgm)

<table>
<thead>
<tr class="header">
<th>id</th>
<th>date</th>
<th>timestamp</th>
<th>sensorglucose</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:49:36</td>
<td>7.17</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:54:36</td>
<td>7.28</td>
</tr>
<tr class="odd">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 14:59:36</td>
<td>7.61</td>
</tr>
<tr class="even">
<td>1</td>
<td>2000-06-30</td>
<td>2000-06-30 15:04:36</td>
<td>7.83</td>
</tr>
</tbody>
</table>

Files can either be separate cleaned files within one folder or one
pre-aggregated dataset. Each function will loop over the unique
individuals in the file and generate the results per individual in the
output file. For more information about single files or pre-aggreated
data please read [CGMprocessing
Package](https://github.com/alicelouisejane/CGMprocessing).

#### <a id="window"></a> Size of the data window

The amount of data within the window you are analyzing could influence
the CGM metrics calculated. It is important to have a consistent window
size ie. the same or very similar number of days of CGM data that each
metric is calculated on. CGMs can monitor days/months/years of data
continuously so it is likely you will only want to take a sample window
to perform analysis on. If you need to reduce the window size it is very
important you don’t introduce bias in the selection which could also
impact the CGM metrics calculated.

For example:

-   Selecting less than 10 days of data is not advised. Many commercial
    analysis systems require at least 7 days of data before metrics are
    summarized.

-   Manually selecting data can introduce biases ie. Selecting CGM data
    at the end of the CGM lifetime or start of a CGM lifetime could
    introduce bias due to some reported sensor inaccuracies that occur
    in this time periods (although perhaps less common with new sensor
    versions). Selecting random windows of continuous data is important
    to reduce this bias.

-   Type 1 diabetes is an evolving disease, both eitologically and
    psychologically for an individual, windowing data needs to best
    reflect the question you are asking. If the CGM data you enter is
    100 days in total these functions will select a random window of 4
    days (CHAOSindex) and 10 days (GVP). metrics calculated from a
    random window selected at day 1 of a 100 day trace may not be the
    same as ones selected near the end of a 100 day trace. **It is
    important to ensure that the data you supply to these functions
    enable the generation of metrics that reflect the question you are
    asking.**

-   There is some evidence of seasonality in CGM data which impact
    measures of variability, with differencesin CGM metrics depending on
    the month of the year and even the days of the week used. Keep this
    in mind when interpreting results.

------------------------------------------------------------------------

## <a id="CHAOSindex"></a> CHAOSindex

**Functionality:** **CHAOSindex()** is a function written to explore
modelling volatility in glucose levels within an individual using ARIMA
modelling. It is based on exploratory logic, has not been validated and
is under continuous development. Use associated code with caution. CGM
data must be cleaned and in the format as described
[here](#inputstructure) which is the same as [CGMprocessing::cleanCGM()
output](https://github.com/alicelouisejane/CGMprocessing?tab=readme-ov-file#cleancgm-output).
The CHAOSindex assess 3 days of data and predicts into a fourth. The
function will select a random window to perform this on from the data
inputted

------------------------------------------------------------------------

The logic underpinning this function is exploratory. The question begins
with: Using 3 days of data, are we able to generate a forecast
prediction of the glucose data up to some set horizon ie. 30 mins up to
a default maximum horizon 90 mins, using ARIMA modelling? Answering this
question underpins the general aim to generate a measure that captures
how “easy” it is to predict a persons glucose over that of standard
variability measures, which ultimately describes how volatile or
unpredictable a persons glucose is.

The CHAOSindex aims to better capture unpredictability of glucose and
associate this with PROMs as well as beta cell function in order to
rapidly and effectively identify and monitor high risk individuals with
type 1 diabetes requiring advanced treatment.

The CHAOSindex is not yet solidified as a true index and there are
multiple pathways to explore in order to characterize it ie. ARIMA
modelling “goodness of fit” parameters may be helpful surrogates of
this. Comparison between the predicted glucose values and the real
glucose values allow us to generate a Mean Absolute Percentage Error
(MAPE) which expanded to a ratio of the 30 min MAPE:90 min MAPE may
enable a index type measure. These ideas are still exploratory and
characterization and validation of the maximum and minimum limits of
this “index” would be required.

#### <a id="CHAOSindexoutput"></a> CHAOSindex output

<figure>
<img src="man/figures/example%20combinedplot_output.png"
alt="Summary output of CGM traces and CHAOS predictions" />
<figcaption aria-hidden="true">Summary output of CGM traces and CHAOS
predictions</figcaption>
</figure>

CSV files outputted are:

-   **mape.csv** which is the Mean Absolute Percentage Error for 30
    minute and 90 minute prediction.

-   **modeloutputparams.csv** ARIMA modelling standardised “goodness of
    fit” parameters ie. Logliklihood ratio, AIC, sigma^2.

-   **predictions30mins.csv** The full data of ARIMA predictions and
    real values for 30 min trace.

-   **predictionsmaximumhorizon.csv** The full data of ARIMA predictions
    and real values for maximum horizon of prediction, default to 90 min
    trace.

------------------------------------------------------------------------

## <a id="calculate_gvp"></a> calculate\_gvp

**Functionality:** **calculate\_gvp()** This function enables generation
of the Glycemic Variability Percentage (GVP) as well as other
standardized measures of variability from CGM data in the [International
Concensus for continuous glucose
monitoring](https://care.diabetesjournals.org/content/40/12/1631). CGM
data must be cleaned and in the format as described
[here](#inputstructure) which is the same as [CGMprocessing::cleanCGM()
output](https://github.com/alicelouisejane/CGMprocessing?tab=readme-ov-file#cleancgm-output).
GVP is based on the logic described most recently in [Peyser
2018](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5846572/) and is
calculated on a window of 10 days of continuous data. If more than 10
days are in the files, the function will select a random 10 day window.
Random selection of data is important as described [above](#).

------------------------------------------------------------------------

Equations for GVP are outlined in [Peyser
2018](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5846572/). The length
of the CGM temporal trace over a given interval of time depends on the
degree of glycemic variability: temporal traces with high glycemic
variability have greater lengths than traces with low glycemic
variability. This “length of line” is then normalised by the minimum
line length ie. time under the trace, to give a percentage.

Mathematically, this is similar to the problem posed by Mandelbrot
regarding the proper measurement of the coastline of Great Britain or
other coastal areas with a high degree of tortuosity.

------------------------------------------------------------------------

❗**Important**: GVP is dependent on the unit of measure of glucose, and
requires the use of **mg/dl**❗

------------------------------------------------------------------------

#### <a id="gvpoutput"></a> calculate\_gvp output

<figure>
<img src="man/figures/example%20gvp_output.png"
alt="Summary output of CGM traces and GVP calculations" />
<figcaption aria-hidden="true">Summary output of CGM traces and GVP
calculations</figcaption>
</figure>

CSV files outputted are:

-   **CGM\_variability\_upload.csv** which is a table containing each
    individual id and their corresponding Glycemic Variability
    Percentage value, as well as Standard deviation, Coefficient of
    Variation, Total AUC, Mean Amplitude Glycemic Excursion, J index,
    Continuous overlapping net glycemic action and the mean of daily
    differences, whose definitions can be found in the
    [intoduction](#intro).
