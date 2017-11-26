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
;   read_apri_apost_fcorr,sim,sa=sa,sp=sp,fcorr=fcorr
;
; INPUTS:
;
;   sim         (STRUCTURE) : the simulation configuratiokn structure, see
;                             inv_configurations.pro
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;   sa    (DblArr,n,ntrace) : the a priori emissions of the ntrace tracers
;                             and n months of simulation
;   sp    (DblArr,n,ntrace) : the corresponding a posteriori emissions
;   fcorr (DblArr,n,ntrace) : the monthls scaling factors for all ntrace tracers
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
PRO read_apri_apost_fcorr,sim,sa=sa,sp=sp,fcorr=fcorr

  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing in call',/continue
     RETURN
  ENDIF

  ;; read a priori, a posteriori and fcorr fields for whole simulation
  sn   = STRCOMPRESS(string(fix(n_elements(sim.stats))),/REM)+'stats'
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  IF keyword_set(sim.flask) THEN BEGIN
     testfile = sim.outdir+'inv_output_weekly_flask_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'.txt'
  ENDIF ELSE BEGIN
     testfile = sim.outdir+'inv_output_weekly_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'.txt'  
     IF keyword_set(sim.special) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_prelim_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'.txt'
     ENDIF
  ENDELSE
  
  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1 ; number months in inversion
  
  fcorr    = DblArr(n,sim.ntrace)
  sa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)
  
  openr,lun,testfile,/get_lun
  readf,lun,fcorr
  readf,lun,sa
  readf,lun,sp
  free_lun,lun

END
