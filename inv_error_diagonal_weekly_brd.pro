;+
; NAME:
;
;   inv_error_diagnoal_weekly_brd
;
; PURPOSE:
;
;   Calculate monthly diagonal error covariance matrix representing
;   the sum of measurement and transport errors.
;
;   One way of estimating the uncertainty would be:
;    sigma =
;      sigma(meas)               ( = sqrt(inst precision^2 )
;    + sigma(transport error)    ( = sqrt(mean of the 3-day SDs) )
;    + sigma(sampling frequency) ( = sqrt(var(CH4mod)/nmod) )
;    + sigma(intercalibration)   ( = 0, because all datahave been scaled to NOAA04 scale)
;
;   However, the method chosen here estimates the uncertainty from the differences
;   between observations and model simulations per year, with the mean annual bias
;   being subtracted from the simulations. The routine, therefore, reads the weekly 
;   observational data (flask and continuous) and weekly mean a priori model data for 
;   the years of the inversion and computes the uncertainty statistics.
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  inv_error_diagonal_weekly_brd,sim
;
; INPUTS:
;
;       sim :  the simulation structure
;
; KEYWORD PARAMETERS:
;
;       /apost : set this keyword if a posteriori simulation results
;                should be used instead of a priori simulation
;
; OUTPUTS:
;
;       weekly station data in predefined order
;             
; COMMON BLOCKS:
;
;        none
;
; SIDE EFFECTS:
;
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;     Read all available data from continuous/flask stations and model
;     data per month
;     Output weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 22 September 2011
; 
; Dominik Brunner, 7 Feb 2017: deleted all subroutines, because they
;       are idential to other sn
; DB, 11 Feb 2017: all subroutines deleted and routine restructured
;                  and simplified in a major way
;-

;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_error_diagonal_weekly_brd,sim,apost=apost

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  
  from = fix(STRMID(sim.syyyymm,0,4))
  to   = fix(STRMID(sim.eyyyymm,0,4))

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
                                    
  ; number of stations
  nst      = n_elements(sim.stats)
  nmonths  = 12
  mon      = STRING(indgen(12)+1,format='(i2.2)')
  nyears   = long(to-from+1)
  nall     = nyears*nmonths*31L
  syear    = STRCOMPRESS(string(from+indgen(nyears)),/REM)
  ndata    = IntArr(nmonths,nyears)

  ;;sigma_meas  = FltArr(nst)+!values.f_nan
  ;;sigma_trans = FltArr(nst,nyears)+!values.f_nan
  ;;sigma_freq  = FltArr(nst,nyears)+!values.f_nan
  ;;sigma_stan  = FltArr(nst,nyears)+!values.f_nan

  sigma_all   = FltArr(nst,nyears)+!values.f_nan
  sigma_mean  = FltArr(nst)+!VALUES.f_nan
  sigmasquare = FltArr(nst)+!VALUES.f_nan  
  bias        = FltArr(nst,nyears)+!values.f_nan
  bias_mean   = FltArr(nst)+!VALUES.F_NAN
  mismatch    = FltArr(nst,nyears)+!values.f_nan
  
  ;;***************************************
  ;;* MAIN LOOP: YEARS
  ;;***************************************
  FOR ij=0,nyears-1 DO BEGIN

     print, 'process year ', syear[ij]
    
     ;;********************************
     ;;* 1. read in observational data 
     ;;********************************
     read_processed_obs_data_year,sim,syear[ij],ch4obs=ch4obs

     ;;************************************************
     ;;* read in corresponding model data
     ;;************************************************
     read_processed_model_data_year,sim,syear[ij],ch4recs=ch4mod
     
     IF keyword_set(apost) THEN BEGIN
        FOR i=0,nmonths-1 DO BEGIN
           yyyymm=syear[ij]+mon[i]
           index = WHERE(STRMID(ch4obs.dtg,0,6) EQ yyyymm,cnt)
           IF cnt EQ 0 THEN stop
           inv_calculate_posterior_tseries,sim,yyyymm,ch4obs=ch4obs[index],$
                                           ch4apri=ch4mod[index],/prelim,$
                                           sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4tmp
           IF i EQ 0 THEN ch4post=ch4tmp ELSE ch4post=[ch4post,ch4tmp]
        ENDFOR
        IF n_elements(ch4post) NE n_elements(ch4mod) THEN stop
        ;; replace prior by posterior time series
        ch4mod = ch4post
     ENDIF

     ;; get names of stations available in this year
     isort = sort(ch4obs.name)
     iuniq = uniq(ch4obs[isort].name)
     nmax = n_elements(iuniq)
     snames = ch4obs[isort[iuniq]].name
                                       
     ;; loop over these stations and compute statistics
     FOR i=0,nmax-1 DO BEGIN
        
        istat = (WHERE(sim.stats EQ snames[i],cnt))[0]
        IF cnt EQ 0 THEN stop ; should not happen

        ;; compute statistics
        index = WHERE(ch4obs.name EQ snames[i] AND finite(ch4obs.ch4),cnt)
        IF cnt GT 2 THEN BEGIN
           diff = ch4obs[index].ch4-ch4mod[index].ch4
           ;; annual median bias
           bias[istat,ij] = median(diff)
           ;; model data corrected for bias
           modcorr = ch4mod[index].ch4-bias[istat,ij]
           ;; centered mean square error
           mismatch[istat,ij] = 1./(float(cnt)-1.)*$
                                total((ch4obs[index].ch4-modcorr)^2)
           ;;************************************************************************
           ;;* 2. sampling frequency error: use station data for hourly measurements,
           ;;*    and model data for event data
           ;;************************************************************************
           sigma_all[istat,ij] = sqrt(mismatch[istat,ij])*sim.ufact[istat]
        ENDIF
     ENDFOR                     ; end loop over stations
  ENDFOR                        ; end loop over years
  
  ;; mean per station over all years
  FOR i=0,nst-1 DO sigma_mean[i]  = mean(sigma_all[i,*],/nan)
  FOR i=0,nst-1 DO sigmasquare[i] = mean(sigma_all[i,*]^2,/nan)
  FOR i=0,nst-1 DO bias_mean[i]  = mean(bias[i,*],/nan)
  
  print
  print,'Centered RMSE for all stations:'

  FOR i=0,nst-1 DO print, sim.stats[i],' ',sigma_mean[i]
  print
  print, 'Mean square error averaged over all stations (preliminary): ', mean(sigmasquare) 
  
  suffix = ''
  IF keyword_set(apost) THEN suffix='aposteriori_'
  IF sim.flask THEN suffix = suffix + 'flask_'
  IF sim.nobg THEN suffix = suffix + 'nobg_'

  filemean = sim.errcovdir+'inv_errorcovariance_stations_'+suffix+$
             sim.sn+'_'+sim.name+'_mean.dat' 

  ;; write out overall statistics
  openw,lun,filemean,/get_lun
  FOR i=0,nst-1 DO printf,lun,sim.stats[i],sigma_mean[i],bias_mean[i],format='(a3,1x,f9.3,1x,f9.3)'
  free_lun,lun

  ;************************************************************
  ;* write out monthly error files
  ;************************************************************
  FOR ij=0,nyears-1 DO BEGIN

     FOR im=0,nmonths-1 DO BEGIN
        
        yyyymm = syear[ij]+mon[im]
        
        read_processed_obs_data_month,sim,yyyymm,ch4obs=ch4obs
        
        IF syear[ij] eq '2006' and im eq 1 THEN BEGIN
           ;; due to switch in vertical model levels, first days in Feb 2006 are missing
           ind = WHERE(dtg2gvtime(ch4obs.dtg) GE dtg2gvtime('200602030000'),c)
           namec    = ch4obs[ind].name
        ENDIF ELSE BEGIN
           namec    = ch4obs.name
        ENDELSE
        nc       = n_elements(namec)
        
        monfile = sim.errcovdir+'inv_errorcovariance_stations_wm_mismatchonly_'+$
                  suffix+sim.sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'

        openw,lun,monfile,/get_lun
        printf,lun,'STN    SIG_ALL'
        FOR i=0,nc-1 DO BEGIN
           ind = WHERE(namec[i] eq sim.stats,c1)
           IF c1 NE 1 THEN stop
           IF finite(sigma_all[ind,ij]) EQ 0 THEN BEGIN
              printf,lun,namec[i],sigma_mean[ind],format='(a3,1x,f9.3)'
           ENDIF ELSE BEGIN
              printf,lun,namec[i],sigma_all[ind,ij],format='(a3,1x,f9.3)'
           ENDELSE
        ENDFOR
        free_lun,lun
     ENDFOR
  ENDFOR
  
END

