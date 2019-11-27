;+
; NAME:
;
;   run_inversion_final
;
; PURPOSE:
;
;   Runs all necessary inversion programs to carry out an inversion and plot the results
;   using a predefined set of observations and parameters.
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  run_inversion
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;             
; COMMON BLOCKS:
;        none
;
; SIDE EFFECTS:
;        none
;
; RESTRICTIONS:
;        none
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; FA 25 November 2015
; DB 05 January 2017: structure simplified. Inversion configuration and
;          station settings moved to separate routines
;          inv_configuration.pro and inv_station_settings.
;-

@inv_tools.pro

PRO run_inversion_final,sim=sim,dlr=dlr

  ;; basic simulation configuration and directory settings
  ;; run = 'NEW_DLR'
  run = '22.4'
  run = '25.1'

  ;; larger uncertainties, especially for wetland emissions
  ;; Dominik with uncert = 0.4 for anthrop, 0.8 for bb, rice, wetl, 0.5 for rest
  ;run = '32.8'
  ;run = '65.6'      

  ;;sconfig = 'flask'             ; options are 'flask', 'all', 'special'
  ;;sconfig = 'flask_DLR2'        ; options are 'flask', 'all', 'special', 'flask_DLR2'
  sconfig = 'brd'               ; options are 'flask', 'all', 'special', 'brd', 'flask_DLR2'

  sim = inv_configurations(run=run,sconfig=sconfig,dlr=dlr,ok=ok)
  IF NOT ok THEN RETURN

  ;; activate steps
  step1 = 0   ; step1: create monthly files of weekly mean observation and model data
              ; needs to be called only once for FLEXPART or EMACs, since the output is
              ; generated for all available sites irrespective of the simulation settings
  step2 = 1   ; step2: compute model-data mismatch first time
  step3 = 1   ; step3: run preliminary inversion to compute aposteriori model-data mismatch
  step4 = 1   ; step4: compute model-data mismatch second time using aposteriori model data
  step5 = 1   ; step5: run final inversion
  step6 = 1   ; step6: run plot programs      
    
  ;************************************************************************
  ; start program chain:
  ;************************************************************************

  ;; 1. compute weekly mean observation data and write to monthly files
  IF keyword_set(step1) THEN BEGIN
     print, '1. Compute and write out weekly mean obs and model data'
     inv_create_monthly_obs_mod_data,obslist=obslist,dlr=dlr
  ENDIF ELSE BEGIN
     print, 'Skipped step1: computing weekly mean observation and model data'
  ENDELSE
  
  ;; 2. compute and write out model-data mismatch uncertainties first time
  IF keyword_set(step2) THEN BEGIN
     print, '2. Compute and write out (prior) model-data mismatch first time'
     inv_create_monthly_station_output,sim,/prelim,/prior
     inv_create_model_data_mismatch,sim,/prelim ;,plot=plot
  ENDIF ELSE BEGIN
     print, 'Skipped step2: computing model-data mismatch for preliminary inversion'    
  ENDELSE
  
  ;; 3. run preliminary inversion that yields first prior model estimates with which,
  ;;    in the following, improved observational errors can be calculated
  IF keyword_set(step3) THEN BEGIN
     print, '3. Run preliminary inversion'
     inv_run,sim,/prelim
     ;; add posteriori fields to station output
     inv_create_monthly_station_output,sim,/prelim,/append
  ENDIF ELSE BEGIN
     print, 'Skipped step3: running preliminary inversion to compute aposteriori model-data mismatch'      
  ENDELSE
  
  ; 4. compute new observational errors that go into the final inversion
  IF keyword_set(step4) THEN BEGIN
     print, '4. Compute error covariance values again for final inversion'
     inv_create_model_data_mismatch,sim ;,plot=plot
  ENDIF ELSE BEGIN
     print, 'Skipped step4: computing model-data mismatch for final inversion'
  ENDELSE  
  
  ;; 5. run inversion second time to obtain final estimates
  IF keyword_set(step5) THEN BEGIN
     print, '5. Run final inversion'
     inv_run,sim
     ;; write out monthly mean station output (obs and model prior and posterior)
     inv_create_monthly_station_output,sim
  ENDIF ELSE BEGIN
     print, 'Skipped step5: running final inversion'        
  ENDELSE
  
  print, 'End of process chain'
  
  ;************************ END PROCESSING *************************************************
  
  IF keyword_set(step6) THEN BEGIN
  
     print, 'Plotting now ...'

     ;; plot a priori, a posteriori and observed station time series
     plot_inv_timeseries,sim,/map,/eps

     ;; plot growth rates
     plot_growth_rates,sim,/eps
     
     ;; plot xhisquare statistics
     plot_chisquare_statistics,sim,/eps

     ;; 8. plot anomalies of emissions
     ;plot_inv_emissions_anomalies_paper_maiolica_final,sim,rel=0
     ;plot_inv_emissions_anomalies_paper_maiolica_final,sim,rel=1
  
     ;; unsich = 0
     ;;  plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats  
  
     ;;unsich = 1
     ;;  plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats

  ENDIF
       
END
