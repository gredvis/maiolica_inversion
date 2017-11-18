;+
; NAME:
;
;   read_processed_model_data_year
;
; PURPOSE:
;
;   Read in all weekly mean model data pre-processed for the inversion for a complete year
;
; CATEGORY:
;
;   MAIOLICA-2, inverse modelling
;
; CALLING SEQUENCE:
;
;   read_processed_model_data_year,sim,yyyy,ch4recs=ch4recs
;
; INPUTS:
;
;  sim           (structure)    : the model simulation information structure
;                                 (see inv_configurations.pro for details)
;  yyyy         (string)        : the year for which to get the data
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
;   DB, 10 Nov 2007: first implementation
;   DB, 05 Feb 2017: small adjustments, improved doc header
;-

PRO read_processed_model_data_year,sim,yyyy,ch4recs=ch4recs
 
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  nmonths = 12
  mm      = STRING(indgen(nmonths)+1,format='(i2.2)')

  ;; loop over months
  FOR i = 0,nmonths-1 DO BEGIN
     read_processed_model_data_month,sim,yyyy+mm[i],ch4recs=ch4tmp
     IF i EQ 0 THEN ch4recs=temporary(ch4tmp) ELSE ch4recs=[ch4recs,ch4tmp]
  ENDFOR

END
