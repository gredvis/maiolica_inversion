# Kalman smoother based methane inversion for project MAIOLICA-2

IDL code for Kalman Smoother inversion following Bruhwiler et al. (2005)

*Bruhwiler, L. M. P., Michalak, A. M., Peters, W., Baker, D. F., and Tans, P.: An improved Kalman Smoother for atmospheric inversions, Atmos. Chem. Phys., 5, 2691–2702, https://doi.org/10.5194/acp-5-2691-2005, 2005.*

## Data 

All input and output is located in a directory with the following subdirectories:

OBSINPUT:        Observation data pre-processed to weekly means as input for inversion
MODINPUT:        Model output at observation sites pre-processed to weekly means as input for inversion
OBSMODINPUT:     New version of pre-processed monthly observation and model data in netCDF format.
                 Replaces previous ASCII files in OBSINPUT and MODINPUT.
RESULTS:         Output of inversion
ERRORCOVARIANCE: Model-observation mismatch uncertainties at all stations computed with routine
                 inv_error_diagonal_weekly_brd.pro. Contains the following types of files
                 - inv_errorcovariance_stations_wm_mismatchonly_XX_YYYYMM.dat
                   Centered standard deviations at all stations for a priori simulation and given month
                 - inv_errorcovariance_stations_aposteriori_wm_mismatchonly_XX_YYYYMM.dat
                   Centered standard deviations at all stations for a posteriori simulation and given month
                 - inv_errorcovariance_stations_XX_mean.dat
                   Mean centered standard deviation and bias for a priori simulation at all stations 
                   averaged over whole period
                 - inv_errorcovariance_stations_aposteriori_XX_mean.dat
                   Mean centered standard deviation and bias for a posteriori simulation at all stations
                   averaged over whole period
SOURCESTRENGTHS: A priori emissions in kg/day for the 48 source categories for the given month
                 Contains files of type inv_emissions_daily_YYYYMM.txt.
SENSITIVITIES:   Monthly sensitivities of concentrations at stations to emissions from 48 categories.
                 Contains three files for each month:
                 - inv_names_weekly_XX_YYYYMM.txt: names of the stations in this month
                 - inv_dates_weekly_XX_YYYYMM.txt: dates for which sensitivities are valid
                 - inv_sensitivity_weekly_XX_YYYYMM.txt: the sensitivities per station, category and date
CHOICE_MODELLEV: Selection of model levels (m above ground) at which FLEXPART is evaluated at the
                 individual stations. For the simulation with 62 flask sites, for example, the 
                 corresponding file is called choice_modellevel_station_62.dat.
FIGURES:         Figures of growth rates, station time series, and emissions
log:             Log files produced by run_inversion_final, e.g. contains
                 number and percentage of observations discarded in each year

## Running an inversion

The main routine is run_inversion_final.pro


