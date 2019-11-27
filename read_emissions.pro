;*************************************************************
; READ MONTHLY EMISSIONS OF ACTUAL MONTH OR 4 MONTHS BEFORE
; UNITS ARE kg/day
;*************************************************************
PRO read_emissions,sim,yyyymm,m3=m3,sa=sa

  trdir      = sim.basedir + 'SOURCESTRENGTHS/'

  IF keyword_set(m3) THEN BEGIN
     ;; get month for time i-4 by subtracting 100 days 
     hyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-100),0,6)
  ENDIF ELSE hyyyymm=yyyymm

  file  = trdir+'inv_emissions_daily_'+hyyyymm+'.txt'

  ntrace  = 48
  sa      = FltArr(ntrace)
  help    = FltArr(ntrace)
  
  openr,lun,file,/get_lun
  readf,lun,help
  sa[*]= help[*]
  free_lun,lun

END
