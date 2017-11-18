;+
; NAME:
;
;   read_processed_obs_data_year
;
; PURPOSE:
;
;   Read in all observation data pre-processed for the inversion for a complete year
;
; CATEGORY:
;
;   MAIOLICA-2, inverse modelling
;
; CALLING SEQUENCE:
;
;   read_processed_obs_data_year,sim,yyyy,ch4obs=ch4obs
;
; INPUTS:
;
;  sim           (structure)     : the model simulation information structure
;                                  (see inv_configurations.pro for details)
;  yyyy         (string)        : the year for which to get the data
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;  ch4obs     Array of structures of the form
;              {dtg:'',name:'',lon:0.,lat:0.,ch4:0.,numobs:0,type:''}
;               where name is the station name, type the station type
;               and numobs the number of individual data points contributing
;               to the average
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
;   DB, 11 Feb 2017: changed to return an array of structures instead of
;                    multiple arrays
;-

PRO read_processed_obs_data_year,sim,yyyy,ch4obs=ch4obs
 
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  nmonths = 12
  mm      = STRING(indgen(nmonths)+1,format='(i2.2)')

  ;; loop over months
  FOR i = 0,nmonths-1 DO BEGIN
     read_processed_obs_data_month,sim,yyyy+mm[i],ch4obs=ch4tmp
     IF i EQ 0 THEN ch4obs=temporary(ch4tmp) ELSE ch4obs=[ch4obs,ch4tmp]
  ENDFOR
  
END
