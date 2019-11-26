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
                 * inv_errorcovariance_stations_wm_mismatchonly_XX_YYYYMM.dat
                   Centered standard deviations at all stations for a priori simulation and given month
                 * inv_errorcovariance_stations_aposteriori_wm_mismatchonly_XX_YYYYMM.dat
                   Centered standard deviations at all stations for a posteriori simulation and given month
                 * inv_errorcovariance_stations_XX_mean.dat
                   Mean centered standard deviation and bias for a priori simulation at all stations 
                   averaged over whole period
                 * inv_errorcovariance_stations_aposteriori_XX_mean.dat
                   Mean centered standard deviation and bias for a posteriori simulation at all stations
                   averaged over whole period
    SOURCESTRENGTHS: A priori emissions in kg/day for the 48 source categories for the given month
                 Contains files of type inv_emissions_daily_YYYYMM.txt.
    SENSITIVITIES:   Monthly sensitivities of concentrations at stations to emissions from 48 categories.
                 Contains three files for each month:
                 * inv_names_weekly_XX_YYYYMM.txt: names of the stations in this month
                 * inv_dates_weekly_XX_YYYYMM.txt: dates for which sensitivities are valid
                 * inv_sensitivity_weekly_XX_YYYYMM.txt: the sensitivities per station, category and date
    CHOICE_MODELLEV: Selection of model levels (m above ground) at which FLEXPART is evaluated at the
                 individual stations. For the simulation with 62 flask sites, for example, the 
                 corresponding file is called choice_modellevel_station_62.dat.
    FIGURES:         Figures of growth rates, station time series, and emissions
    log:             Log files produced by run_inversion_final, e.g. contains
                 number and percentage of observations discarded in each year

## Running an inversion

**Main routine**

    run_inversion_final, sim=sim, dlr=dlr
  
    Keyword input parameter:
       /dlr: set this keyword to perform inversion on DLR's EMAC output instead of FLEXPART
    Output:
       sim : simulation structure
       
The inversion settings are defined through the following variables defined (hardcoded) at the beginning:

    run (string):     Inversion configuration name. The latest configuations used by Florian were 'NEW_DLR' and '22.4'
                      The configuration defines the a priori uncertainties per emission category
    sconfig (string): the station configuration. Current options are 'flask', 'all', 'special', 'brd', 'flask_DLR2'

The following processing steps can be activated (1) or deactivated (0)

    step1 = 0   ; create monthly files of weekly mean observation and model data
                ; needs to be called only once for FLEXPART or EMACs, since the output is
                ; generated for all available sites irrespective of the simulation settings
    step2 = 1   ; compute model-data mismatch first time
    step3 = 1   ; run preliminary inversion to compute aposteriori model-data mismatch
    step4 = 1   ; compute model-data mismatch second time using aposteriori model data
    step5 = 1   ; run final inversion
    step6 = 0   ; run plot programs


**Inversion settings**

Define directories and inversion settings depending on variables *run* and *sconfig* provided by main routine.

    sim = inv_configurations_brd(run=run,sconfig=sconfig,dlr=dlr,ok=ok)
    
    Inputs:
       run:     inversion configuration name (see above)
       sconfig: station configuration (e.g. 'flask' or 'all')
       dlr:     set dlr=1 to conduct inversion for DLR model output 
    Output:
       ok :     =1 if configuration was successful, i.e. if values of run and sconfig are know
       sim:     Simulation configuration structure
       
         sim = {name:name,$                ; simulation name, e.g. 'final_sim01'
         sconfig:sconfig,$                 ; station configuration, e.g. 'flask'
         sn,$                              ; string 'NNstats' with NN number of stations 
                                           ; used for file names
         qunc:qunc,$                       ; string 'optUU.U' with total a priori uncertainty
                                           ; used for file names 
         dlr:dlr,$                         ; =0 for FLEXPART, =1 for DLR output processing
         obsdir:obsdir,$                   ; directory of pre-processed obs data
         modeldir:modeldir,$               ; base directory of FLEXPART model output
                                           ; model output will be in modeldir/name
         outdir:outdir,$                   ; directory of inversion output
         hdir:hdir,$                       ; directory of station sensitivities
         syyyymm:syyyymm,eyyyymm:eyyyymm,$ ; start and end month of inversion
         scaleq:scaleq,$                   ; uncertainty scaling factors per category
         ntrace:ntrace,nage:nage,$         ; number of tracer (48) and age classes (5)
         stats:stats,ufact:ufact,$         ; list of stations and uncertainy scaling factors
         stat_levs:levs,$                  ; altitude levels at which FLEXPART is evaluated
         flask:flask,$                     ; =1 if only flask sites are included
         filter:filter,$                   ; =1 to enforce filtering for non-background values
         statfilt:statfilt,$               ; list of sites to be filtered if /filter is set
         weekly:weekly,$                   ; =1 to aggregate to weekly observation
         keeppos:keeppos,$                 ; =1 to use log of emissions for positive solutions
         startcf:startcf}                  ; list of initial scaling factors
