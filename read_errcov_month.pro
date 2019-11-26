;+
; NAME:
;
;   read_errcov_month
;
; PURPOSE:
;
;   Read in monthly mean model mismatches at the stations stored in files
;      inv_errorcovariance_stations_wm_mismatchonly_xx_YYYYMM.dat
;   for the uncertainties calculated based on the a priori data or 
;      inv_errorcovariance_stations_wm_mismatchonly_aposteriori_flask_xx_YYYYMM.dat
;   for the uncertainties calculated after the first inversion
; 
;   The squared values of these uncertainties are returned for use as 
;   diagonal elements of the observation error covariance matrix R
;
; CATEGORY:
;
;   MAIOLICA-2, inverse modelling
;
; CALLING SEQUENCE:
;
;   read_errcov_month,sim,yyyymm,errcov=errcov,stats=stats,prelim=prelim
;
; INPUTS:
;
;  sim           (structure)     : the model simulation information structure
;                                  (see inv_configurations.pro for details)
;  yyyymm        (string)        : the month for which to get the data
;
; KEYWORD PARAMETERS:
;
;  /prelim                     : set this keyword to read in mismatches of a priori
;                                  model simulation
;
; OUTPUTS:
;
;  errcov   (FltArr)             : the monthly mean errors per station
;  stats    (Strarr)             : the station names
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 23 Nov 2017: first implementation
;-

PRO read_errcov_month,sim,yyyymm,errcov=errcov,stats=stats,prelim=prelim
 
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  IF n_elements(yyyymm) EQ 0 THEN BEGIN
     print,'parameter yyyymm missing in call'
     RETURN
  ENDIF
  
  ;;*********************************************************
  ;; read error covariance matrix (vector)
  ;; of year and month (only diagonal elements)
  ;; Unit: ppb^2
  ;;*********************************************************
  sstr = sim_filename_str(sim,prelim=prelim)

  sstr = 'filter_89stats_final_sim01_'

  errorfile = sim.errcovdir+'inv_errorcovariance_stations_wm_mismatchonly_'+$
              sstr+yyyymm+'.dat'

  IF file_test(errorfile) EQ 0 THEN stop
  
  header    = '' & dummy = header
  n         = FILE_LINES(errorfile)-1 ; number of data lines by subtracting the header line
  stats     = StrArr(n)
  errcov    = DblArr(n)
  rfact     = 1d
  openr,lun,errorfile,/get_lun
  readf,lun,header
  FOR i=0,n-1 DO BEGIN
     readf,lun,dummy
     result    = STRSPLIT(dummy,/EXTR)
     stats[i]  = result[0]
     errcov[i] = (result[1]*rfact)^2
  ENDFOR
  free_lun,lun

END  
