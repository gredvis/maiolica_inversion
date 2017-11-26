;+
; NAME:
;
;  READ_PROCESSED_OBS_DATA_MONTH
;
; PURPOSE:
;
;  Read CH4 measurement data for a given month yyyymm which have been
;  processed with inv_obsvector_mon_weekly.pro from individual station
;  data into monthly files with weekly averages
;
; CATEGORY:
;
;  URMEL, CH4 inversion
;
; CALLING SEQUENCE:
;
;  read_processed_obs_data_month,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs
;
; INPUTS:
;
;  sim                (structure) : the model simulation information structure
;                                  (see inv_run.pro for details)
;  yyyymm             (string)    : the year and month for which to get the data
;
; KEYWORD PARAMETERS:
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
;   (c) Tina Schnadt Proberai
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   March 2012: first implementation
;
;   Dominik Brunner, 8 Jan 2017: adjusted to make use of simulation structure sim
;   original name was read_data_single_final.pro
;   Changed to return a structure array rather than multiple arrays.
;-
PRO read_processed_obs_data_month,sim,yyyymm,ch4obs=ch4obs

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  sn = STRCOMPRESS(string(fix(n_elements(sim.stats))),/REM)+'stats'  
  flask = sim.sconfig EQ 'flask'

  suffix = ''
  IF sim.flask THEN suffix = 'flask_'
  IF sim.special THEN suffix = 'special_'
  IF sim.brw THEN suffix = ''

  ;*******************************
  ;* read data
  ;******************************* 
  infile = sim.obsdir + 'z_allweekly_' + suffix + sn + '_' + yyyymm + '.dat'
  nobs   = FILE_LINES(infile)

  rec = {dtg:'',name:'',lon:0.,lat:0.,ch4:0.,numobs:0,type:''}
  ch4obs = replicate(rec,nobs)
 
  openr,lun,infile,/get_lun
  readf,lun,ch4obs,format='(a12,1x,a3,1x,f8.3,1x,f8.3,1x,f7.2,1x,i4,1x,a2)'
  free_lun,lun

END
