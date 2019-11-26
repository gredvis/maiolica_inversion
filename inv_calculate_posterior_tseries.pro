;+
; NAME:
;
;  inv_calculate_posterior_tseries
;
; PURPOSE:
;
;  For a given inversion simulation compute the a posteriori concentrations at all 
;  assimilated observation stations for a given month yyyymm.
;  The posterior concentrations are calculated from the prior concentrations by
;  scaling the contributions from the different age classes by the ratio between
;  posterior and prior emissions and applying the correction factors to the oldest
;  age class.
;  The routine reads in the observations and the prior simulations for the 
;  current month as well as the prior (sa) and posterior emissions (sp) and correction 
;  factors (fcorr) for the whole simulation.If the arrays sa, sp and fcorr are not
;  supplied, they are read from the inversion output and returned for a next call
;  of this routine, which avoids reading them for each month anew.
;
; CATEGORY:
;
;  inverse modelling, MAIOLICA2 project
;
; CALLING SEQUENCE:
;
;  inv_calculate_posterior_tseries,sim,yyyymm,ch4obs=ch4obs,ch4apri=ch4apri,$
;                            sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4post,prelim=prelim
;
; INPUTS:
;
;  sim            (STRUCTURE) : the simulation configuration structure, see
;                               inv_configurations.pro
;  yyyymm
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;   /prelim                  : set this keyword to read results from preliminary inversion
;
;   The following three fields need to be present to allow looping over all months:
;   sa     (DblArr,n,ntrace) : the a priori emissions of the ntrace tracers
;                              and n months of simulation
;   sp     (DblArr,n,ntrace) : the corresponding a posteriori emissions
;   fcorr  (DblArr,n,ntrace) : the monthly scaling factors for all ntrace tracers
;
;
; OUTPUTS:
;
;   ch4obs       (STRUCTURE) : Structure array of all weekly observations
;                              at stations available during this month
;   ch4apri      (STRUCTURE) : Structure array of all weekly a prior simulated
;                              values at stations available during this month
;   ch4post      (STRUCTURE) : Structure array of all weekly a posteriori simulated
;                              values at stations available during this month
;   
;   sa     (DblArr,n,ntrace) : the a priori emissions of the ntrace tracers
;                              and n months of simulation
;   sp     (DblArr,n,ntrace) : the corresponding a posteriori emissions
;   fcorr  (DblArr,n,ntrace) : the monthly scaling factors for all ntrace tracers
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;  Dominik Brunner (DB)
;  Empa, Swiss Federal Laboratories for Materials Science and Technology
;  dominik.brunner@empa.ch
;  DB, 26 Nov 2017: first implementation
;  DB, 06 Jan 2018: adjusted to new netcdf output
;-
PRO inv_calculate_posterior_tseries,sim,yyyymm,prelim=prelim,ch4obs=ch4obs,ch4apri=ch4apri,$
                                    sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4post,ok=ok

  ok = 0
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  IF n_elements(yyyymm) EQ 0 THEN BEGIN
     print,'parameter yyyymm missing in call'
     RETURN
  ENDIF

  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1 ; number months in inversion

  ;; copy structure from a priori
  ch4post = ch4apri

  ;; get month index of the simulation
  yyyy = fix(STRMID(yyyymm,0,4)) & mm = fix(STRMID(yyyymm,4,2))
  im = (yyyy - syyyy)*12 + mm - smm
  IF im LT 0 OR im GT (n-1) THEN RETURN

  ;; read in monthly a priori and a posteriori emissions and scaling factors
  IF n_elements(sa) EQ 0 OR n_elements(sp) EQ 0 OR n_elements(fcorr) EQ 0 THEN BEGIN
     result = read_inv_results_netcdf(sim,prelim=prelim,/emisonly)
     IF size(result,/type) EQ 2 THEN RETURN
     sa = result.emis_apri
     sp = result.emis_apost
     fcorr = result.scalef
  ENDIF
  
  FOR i=0,n_elements(ch4obs)-1 DO BEGIN

     FOR j=0,sim.ntrace-1 DO BEGIN
        IF sa[im,j] GT 0. THEN $
           ch4post[i].ch4trace[j]=ch4apri[i].ch4trace[j]*sp[im,j]/sa[im,j]
        IF im GT 0 THEN BEGIN
           ik = im-1 ;; month-1
           IF sa[ik,j] GT 0. THEN $
              ch4post[i].ch4trace[sim.ntrace+j]=$
              ch4apri[i].ch4trace[sim.ntrace+j]*sp[ik,j]/sa[ik,j]
        ENDIF
        IF im GT 1 THEN BEGIN
           ik = im-2 ;; month-2
           IF sa[ik,j] GT 0. THEN $
              ch4post[i].ch4trace[2*sim.ntrace+j]=$
              ch4apri[i].ch4trace[2*sim.ntrace+j]*sp[ik,j]/sa[ik,j]
        ENDIF
        IF im GT 2 THEN BEGIN
           ik = im-3 ;; month-3
           IF sa[ik,j] GT 0. THEN $
              ch4post[i].ch4trace[3*sim.ntrace+j]=$
              ch4apri[i].ch4trace[3*sim.ntrace+j]*sp[ik,j]/sa[ik,j]
        ENDIF
        IF im GT 3 THEN BEGIN
           ik = im-4 ;; month-4, oldest age class, apply correction factor
           ch4post[i].ch4trace[4*sim.ntrace+j]=$
              ch4apri[i].ch4trace[4*sim.ntrace+j]*fcorr[ik,j]
        ENDIF
        ch4post[i].ch4=total(ch4post[i].ch4trace)
     ENDFOR
  ENDFOR

  ok = 1
END
