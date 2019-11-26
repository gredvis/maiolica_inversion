;+
; NAME:
;
;   read_processed_model_data_month
;
; PURPOSE:
;
;   Read in all weekly mean model data pre-processed for the inversion for a given month
;
; CATEGORY:
;
;   MAIOLICA-2, inverse modelling
;
; CALLING SEQUENCE:
;
;   read_processed_model_data_month,sim,yyyymm,ch4recs=ch4recs
;
; INPUTS:
;
;  sim           (structure)     : the model simulation information structure
;                                  (see inv_configurations.pro for details)
;  yyyymm        (string)        : the month for which to get the data
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;  ch4recs : array of structures of form
;             {statname:'',
;              dtg:'',$
;              ch4:0D,$
;              ch4trace:DblArr(sim.ntrace*sim.nage)}
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
;   DB, 11 Feb 2017: first implementation
;-

PRO read_processed_model_data_month,sim,yyyymm,ch4recs=ch4recs
 
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  ;; maximum number of data records in a given month
  nmax = 400L

  trace =  DblArr(sim.ntrace*sim.nage)
  rec = {statname:'',dtg:'',ch4:0D,ch4trace:trace}
  ch4recs = replicate(rec,nmax)
  line    = ''

  k=0L

  yyyy = STRMID(yyyymm,0,4)
  mm = STRMID(yyyymm,4,2)

  IF sim.flask THEN BEGIN
     file = sim.moddir+'m_allweekly_flask_'+sim.sn+'_'+sim.name+'_'+yyyy+mm+'.dat'
  ENDIF ELSE BEGIN
     file = sim.moddir+'m_allweekly_'+sim.sn+'_'+sim.name+'_'+yyyy+mm+'.dat'     
  ENDELSE

  openr,lun,file,/get_lun
  WHILE NOT eof(lun) DO BEGIN
     readf,lun,line
     result  = STRSPLIT(line,/EXTRACT)
     dtg = result[0]
     statname = result[1]
     ch4 = double(result[4])
     readf,lun,trace
     ch4recs[k] = {statname:statname,dtg:dtg,ch4:ch4,ch4trace:trace}
     k++
  ENDWHILE
  free_lun,lun
  
  ;; cut array to number of records found in the year
  ch4recs = ch4recs[0:k-1]

END  
