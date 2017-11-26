;*************************************************************
;READ MONTHLY EMISSIONS OF 3 MONTHS BEFORE ACTUAL MONTH
;*************************************************************
PRO read_emissions_final,yyyymm=yyyymm,m3=m3,sa=sa

  trdir      = '/nas/arf/INVERSION/SOURCESTRENGTHS/'

  IF keyword_set(m3) THEN BEGIN
;     ;; get month for time i-3 by subtracting 70 days
;     hyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-70),0,6)

;; get month for time i-4 by subtracting 100 days ;FLO
     hyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-100),0,6)
     ;hyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-70),0,6) ;flo NEW 04/05/2015

  ENDIF ELSE hyyyymm=yyyymm

  file  = trdir+'inv_emissions_daily_'+hyyyymm+'.txt'

  ntrace  = 48
  sa      = FltArr(ntrace)
  help    = FltArr(ntrace)
  
  openr,lun,file,/get_lun
  readf,lun,help
  sa[*]= help[*]
  free_lun,lun
;sa[where(sa eq 0.0000000)] = 1.100000 ;flo NEW
END
