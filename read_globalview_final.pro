;+
; NAME:
;
;   read_globalview
;
; PURPOSE:
;
;   Reads GLOBALVIEW reference boundary layer methane matrix
;   that is stored under /home/spc134/DATA/GLOBALVIEW/
;   and is named ref_mbl_mtx.ch4
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;   read_globalview,latglob=lat,timeglob=time,ch4glob=ch4
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;       monthly reference methane values, latitudinal dependence
;       for the period 1984-2008
;             
; COMMON BLOCKS:
;
;        none
;
; SIDE EFFECTS:
;
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   inv_obsvector_mon
;
;     Read all available data from continuous and flask stations in given year
;     Output daily or weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 19 September 2011
;-

PRO read_globalview_final,latglob=lat,timeglob=uyyyymm,ch4glob=ch4

  days    = [31,28,31,30,31,30,31,31,30,31,30,31]
  schalt  = ['1984','1988','1992','1996','2000','2004','2008']
  mm      = ['01','02','03','04','05','06','07','08','09','10','11','12']
  nmonths = n_elements(days)
  
  ndat = (2008-1984+1)*12L ; number of output dates
  nlat = 41                ; number of latitudes

  file = '/project/arf/remote7/ref_mbl_mtx.ch4'
  n    = FILE_LINES(file)

  form      = ''
  line      = ''
  time      = FltArr(n-1)
  ch4weekly = FltArr(n-1,nlat)
  ch4       = FltArr(ndat,nlat)
  
  openr,lun,file,/get_lun
  readf,lun,form
  FOR i=0,n-2 DO BEGIN
    readf,lun,line
    result = STRSPLIT(line,/EXTRACT)
    time[i] = float(result[0])
    FOR iy=0,nlat-1 DO ch4weekly[i,iy] = float(result[1+iy])
  ENDFOR
  free_lun,lun

  ; compute latitudes
  sinlat = -1.+0.05*findgen(nlat)
  lat    = asin(sinlat)*180./!pi

  ;compute monthly methane values
  jahr   = floor(time)
  sjahr  = STRCOMPRESS(string(jahr),/REM)
  frac   = time-float(jahr)
  sjahr  = sjahr(sort(sjahr))
  ujahr  = sjahr(uniq(sjahr))
  nyears = n_elements(ujahr)
  yyyymm = StrArr(n-1)
  yr     = jahr[0]+indgen(nyears)

  a = 0
  e = 0
  FOR ij=0,nyears-1 DO BEGIN
    inds  = WHERE(ujahr[ij] eq schalt,cs)
    IF cs eq 1 THEN days[1] = 29
    daysa    = IntArr(nmonths)
    dayse    = IntArr(nmonths)
    daysa[0] = 1
    FOR im=0,nmonths-1 DO dayse[im] = total(days[0:im])
    FOR im=1,nmonths-1 DO daysa[im] = total(days[0:im-1])+1

    indyr = WHERE(yr[ij] eq jahr,cyr)
    IF cyr gt 0L THEN BEGIN
      tag    = IntArr(cyr)
      tag[*] = ceil(frac[indyr]*dayse[nmonths-1])
      ind    = WHERE(tag eq 0,cnull)
      IF cnull eq 1 THEN tag[ind] = 1

      FOR im=0,nmonths-1 DO BEGIN
        indm = WHERE(tag ge daysa[im] and tag le dayse[im],cm)
        e += cm
        yyyymm[a:e-1] = ujahr[ij]+mm[im]      
        a += cm
      ENDFOR
    ENDIF
  ENDFOR

  hip     = dtg2hiptime(yyyymm)
  syyyymm = hip(sort(hip))
  uyyyymm = STRMID(hiptime2dtg(syyyymm(uniq(syyyymm))),0,6)

  std = FltArr(ndat,nlat)
  FOR id=0,ndat-1 DO BEGIN
     indd = WHERE(yyyymm eq uyyyymm[id],cd)
     FOR iy=0,nlat-1 DO ch4[id,iy] = mean(ch4weekly[indd,iy])
     FOR iy=0,nlat-1 DO std[id,iy] = stddev(ch4weekly[indd,iy])
  ENDFOR
  
  ;; expand data set by repeating last year 2008
  i2008 = WHERE(STRMID(uyyyymm,0,4) EQ '2008')
  FOR i=0,3 DO BEGIN
     ch4 = [ch4,ch4[i2008,*]]
     year = STRING(2009+i,format='(i4)')
     uyyyymm = [uyyyymm,year+mm]
  ENDFOR

  print, 'Processing of read_globalview completed.'

END
