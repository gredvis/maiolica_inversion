;+
; NAME:
;
;   read_apri_apost_fcorr
;
; PURPOSE:
;
;   Read in output of an inversion simulation containing the monthly 
;   a priori and a posteriori emissions and correction factors.
;   These data are stored at the end of an inversion run in files
;   inv_output_weekly_xxx.txt
;   where xxx is an additional string describing the type of stations,
;   the simulation name, the simulation period, and the total a priori uncertainty.
;   Example:
;   inv_output_weekly_flask_prelim_62stats_final_sim01_198902-201212_opt16.4000.txt
;
; CATEGORY:
;
;   inverse modelling, MAIOLICA2 project
;
; CALLING SEQUENCE:
;
;    read_apri_apost_fcorr,sim,sa=sa,sp=sp,fcorr=fcorr,prelim=prelim,qpdiag=qpdiag,$
;                          q1diag=q1diag,q2diag=q2diag,q3diag=q3diag,uncert=uncert,$
;                          yyyymm=yyyymm,categories=categories,labels=labels
;
; INPUTS:
;
;   sim         (STRUCTURE) : the simulation configuratiokn structure, see
;                             inv_configurations.pro
;
; KEYWORD PARAMETERS:
;
;   /prelim                 : set this keyword to read in results of preliminary
;                             inversion
;
; OUTPUTS:
;
;   sa    (DblArr,n,ntrace) : the a priori emissions of the ntrace tracers
;                             and n months of simulation
;   sp    (DblArr,n,ntrace) : the corresponding a posteriori emissions
;   fcorr (DblArr,n,ntrace) : the monthls scaling factors for all ntrace tracers
;   qpdiag(DblArr,n,ntrace) : uncertainties (sqrt of diagonal elements) of final
;                             emission estimates (after fourth step)
;   q1diag(DblArr,n,ntrace) : uncertainties of emissions after first step
;   q2diag(DblArr,n,ntrace) : uncertainties of emissions after second step
;   q3diag(DblArr,n,ntrace) : uncertainties of emissions after third step
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
;-
PRO read_apri_apost_fcorr,sim,sa=sa,sp=sp,fcorr=fcorr,prelim=prelim,qpdiag=qpdiag,$
                          q1diag=q1diag,q2diag=q2diag,q3diag=q3diag,uncert=uncert,$
                          yyyymm=yyyymm,categories=categories,labels=labels

  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing in call',/continue
     RETURN
  ENDIF

  ;; read a priori, a posteriori and fcorr fields for whole simulation
  sstr = ''
  IF keyword_set(prelim) THEN sstr = 'prelim_'
  IF keyword_set(sim.flask) THEN sstr = sstr + 'flask_'
  IF keyword_set(sim.filter) THEN sstr = sstr + 'filter_'
  testfile = sim.outdir+'inv_output_weekly_'+sstr+sim.sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
             sim.eyyyymm+'_'+sim.qunc+'.txt'

  ;; get simulation dates (months) and emission categories
  yyyymm = get_sim_dates(sim)
  n = n_elements(yyyymm)
  categories = emiss_categories(labels=labels)

  fcorr    = DblArr(n,sim.ntrace)
  sa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)
  
  Qpdiag   = DblArr(n,sim.ntrace)
  Q1diag   = DblArr(n,sim.ntrace)
  Q2diag   = DblArr(n,sim.ntrace)
  Q3diag   = DblArr(n,sim.ntrace)

  uncert   = DblArr(n,sim.ntrace)

  openr,lun,testfile,/get_lun
  readf,lun,fcorr
  readf,lun,sa
  readf,lun,sp
  readf,lun,qpdiag
  readf,lun,q1diag
  readf,lun,q2diag
  readf,lun,q3diag
  readf,lun,uncert
  free_lun,lun

END
