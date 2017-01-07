;+
; NAME:
;
;  READ_DATA_SINGLE
;
; PURPOSE:
;
;  read CH4 measurement data for a given month yyyymm
;
; CATEGORY:
;
;  URMEL, CH4 inversion
;
; CALLING SEQUENCE:
;
;  read_data_single,sim=sim,yyyymm=yyyymm,ch4obs=ch4,dtgobs=dtg,$
;                     lonobs=lon,latobs=lat,nameobs=name,nobs=nobs
;
; INPUTS:
;
;  sim                (structure) : the model simulation information structure
;                                  (see inv_run.pro for details)
;  yyyymm             (string)    : the year and month for which to get the data
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;  ch4obs          (fltarr(nobs)) : the nobs CH4 measurements
;  dtgobs          (strarr(nobs)) : the corresponding dates/times
;  lonobs          (fltarr(nobs)) : the corresponding longitudes
;  latobs          (fltarr(nobs)) : the corresponding latitudes
;  nambeobs        (strarr(nobs)) : the corresponding station names
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
;   (c) Tina Schnadt Proberai
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   March 2012: first implementation
;
;-

PRO read_data_single_final,sim=sim,yyyymm=yyyymm,ch4obs=ch4,dtgobs=dtg,$
                     lonobs=lon,latobs=lat,nameobs=name,nobs=nobs,weekly=weekly,$
                     stats=stats,flask=flask,brw=brw,nobg=nobg,special=special

  sn = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'
;  IF keyword_set(special) THEN sn = '88stats'   
;  IF keyword_set(brw) THEN sn = '91stats'
;  IF keyword_set(flask) THEN sn = '69stats'
;  IF keyword_set(flask) THEN sn = '11stats' ;FLO MAYBE TO REMOVE
;IF keyword_set(flask) THEN sn = '10stats'

  ;*******************************
  ;* read data
  ;*******************************     
  IF KEYWORD_SET(weekly) THEN BEGIN
    IF keyword_set(flask) THEN BEGIN
      infile = sim.obsdir+'z_allweekly_flask_'+sn+'_'+yyyymm+'.dat'    
      IF keyword_set(nobg) THEN infile = sim.obsdir+'z_allweekly_flask_nobg_'+sn+'_'+yyyymm+'.dat'     
    ENDIF ELSE BEGIN
      infile = sim.obsdir+'z_allweekly_'+sn+'_'+yyyymm+'.dat'    
      IF keyword_set(brw)  THEN    infile = sim.obsdir+'z_allweekly_'+sn+'_'+yyyymm+'.dat'        
      IF keyword_set(nobg) THEN    infile = sim.obsdir+'z_allweekly_nobg_'+sn+'_'+yyyymm+'.dat'        
      IF keyword_set(special) THEN infile = sim.obsdir+'z_allweekly_special_'+sn+'_'+yyyymm+'.dat' 
    ENDELSE 
  ENDIF ELSE BEGIN
    infile   = sim.obsdir+'z_'+yyyymm+'.dat'
  ENDELSE
  nobs   = FILE_LINES(infile)

  dtg  = StrArr(nobs)
  name = StrArr(nobs)
  lat  = FltArr(nobs)
  lon  = FltArr(nobs)
  ch4  = DblArr(nobs)

  a0=''
  openr,lun,infile,/get_lun
  FOR i=0L,nobs-1 DO BEGIN
    readf,lun,a0
    result  = STRSPLIT(a0,/EXTRACT)
    dtg[i]  = result[0]
    name[i] = result[1]
    lon[i]  = float(result[2])
    lat[i]  = float(result[3])
    ch4[i]  = double(result[4])
  ENDFOR
  free_lun,lun

END
