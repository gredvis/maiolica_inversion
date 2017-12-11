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

@inv_tools_brd.pro

PRO run_inversion_final,sim=sim,dlr=dlr

  ;; basic simulation configuration and directory settings
;  run = 'NEW_DLR'               ; or '22.4'
  run = '22.4'

  run = '32.8'

  ;;sconfig = 'flask'             ; options are 'flask', 'all', 'special'
  sconfig = 'flask_DLR2'        ; options are 'flask', 'all', 'special', 'flask_DLR2'

  sim = inv_configurations_brd(run=run,sconfig=sconfig,dlr=dlr,ok=ok)
  IF NOT ok THEN RETURN
  
  ;; activate steps
  step1 = 1   ; step1: read in original obs data and write to monthly files of weekly means
  step2 = 1   ; step2: read in model receptor output and write to monthly files of weekly means
  step3 = 1   ; step3: compute model-data mismatch first time
  step4 = 1   ; step4: run preliminary inversion to compute aposteriori model-data mismatch
  step5 = 1   ; step5: compute model-data mismatch second time using aposteriori model data
  step6 = 1   ; step6: run final inversion
  step7 = 0   ; step7: run inv_emissions_ratio to determine model estimate separated into categories
  step8 = 0   ; step8: run plot programs      
  step9 = 0   ; step9: run plot programs 2

  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
    
  ;************************************************************************
  ; start program chain:
  ;************************************************************************

  ;; 1. compute weekly mean observation data and write to monthly files
  IF keyword_set(step1) THEN BEGIN
     inv_obsvector_mon_weekly_brd,sim
   ENDIF ELSE BEGIN
     print, 'Skipped step1: computing weekly mean observational data'
  ENDELSE

  ;; 2. compute weekly mean model data and write to monthly files
  IF keyword_set(step2) THEN BEGIN
     inv_modvector_mon_weekly_brd,sim,dlr=dlr,/plot
  ENDIF ELSE BEGIN
     print, 'Skipped step2: computing weekly mean model data'  
  ENDELSE

  ;; 3. compute observational errors
  IF keyword_set(step3) THEN BEGIN
     print, '3. Run inv_error_diagonal_weekly'
     inv_error_diagonal_weekly_brd,sim
  ENDIF ELSE BEGIN
     print, 'Skipped step3: computing model-data mismatch first time'    
  ENDELSE
  
  ;; 4. run preliminary inversion that yields first prior model estimates with which,
  ;;    in the following, improved observational errors can be calculated
  IF keyword_set(step4) THEN BEGIN
     print, '4. Run preliminary inversion'
     hdump    = 1
     rapriori = 1
     inv_run_brd,sim,hdump=hdump,serdllh=serdllh,sumdllh=sumdllh,$
                 serzlen=serzlen,sumzlen=sumzlen,rapriori=rapriori,$
                 sernobse=sernobse,dlr=dlr
  ENDIF ELSE BEGIN
     print, 'Skipped step4: running preliminary inversion to compute aposteriori model-data mismatch'      
  ENDELSE
 
  ; 5. compute new observational errors that go into the final inversion
  IF keyword_set(step5) THEN BEGIN
    print, '5. Compute error covariance values again for final inversion'
    inv_error_diagonal_weekly_brd,sim,/apost
    ;inv_error_diagonal_weekly_aposteriori_brd,sim,ufact=ufact
  ENDIF ELSE BEGIN
     print, 'Skipped step5: computing error covariance values again for final inversion'
  ENDELSE  
  
  ;; 6. run inversion second time to obtain final estimates
  IF keyword_set(step6) THEN BEGIN
     print, '6. Run final inversion'
     hdump     = 1
     rapriori  = 0              ;1
     inv_run_brd,sim,hdump=hdump,serdllh=serdllh,sumdllh=sumdllh,$
                 serzlen=serzlen,sumzlen=sumzlen,rapriori=rapriori,$
                 sernobse=sernobse
  ENDIF ELSE BEGIN
     print, 'Skipped step6: running final inversion'        
  ENDELSE        
  
  ;; 7. compute model estimates from final inversion separated in categories
  IF keyword_set(step7) THEN BEGIN
     print, '7. compute a posteriori model estimates divided into categories'
     inv_emissions_ratio_brd,sim
  ENDIF ELSE BEGIN
     print, 'Skipped step7: running inv_emissions_ratio to determine model estimate separated into categories'        
  ENDELSE
  
  print, 'End of process chain'
  
  ;************************ END PROCESSING *************************************************
  
  IF keyword_set(step8) THEN BEGIN
  
     print, 'Plotting now ...'
     
     ;; 8. plot anomalies of emissions
     ;;  flask = 1
     rel   = 0
     plot_inv_emissions_anomalies_paper_maiolica_final,sim=sim,stats=stats,flask=flask,rel=rel,nobg=nobg,special=special
     
     ;;  rel   = 1
     ;;  plot_inv_emissions_anomalies_paper,sim=sim,stats=stats,flask=flask,rel=rel,nobg=nobg,special=special
  
     ;; unsich = 0
     ;;  plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats  
  
     ;;unsich = 1
     ;;  plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats    
  ENDIF
 
 IF keyword_set(step9) THEN BEGIN
    ;; 9. growth rates  
    plot_station_methane_categories_MAIOLICA,lat=3
    ;; 9. plot emissions errors
    ;; plot_inv_q,sim=sim,stats=stats
  
    ;; 10. plot growth rates
    ;; 10a: NH
    ;; lat = 0
    ;; compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg
  
    ;; 10b: tropics
    ;; lat = 1
    ;; compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg  
  
    ;; 10c: SH
    ;; lat = 2
    ;; compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg  
  
    ;; 10d: global    
    ;; lat = 3
    ;; compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg
  ENDIF
      
END
