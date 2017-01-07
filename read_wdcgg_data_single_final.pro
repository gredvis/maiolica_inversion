;+
; NAME:
;    read_wdcgg_data_single
;
; PURPOSE:
;     Reads data from WDCGG files at /nas/input/WDCGG/...
;
; CALLING SEQUENCE:
;     read_wdcgg_data_single,file=file,station=station,contri=contri,lat,lon,cal,$
;                                 jahr=jahr,month=month,day=day,ch4=ch4,$
;                                 flag=flag,ndata=ndata,characteristics=characteristics,$
;                                 brw=brw,special=special,statfilt=statfilt
;
; KEYWORD INPUTS:
;     file       (STRING) : path of WDCGG file
;     station    (STRING) : name of corresponding station (may be extracted from filename)
;     contri     (STRING) : acronym of contributing network
;     /brw                : set keyword to filter all non-background values at BRW
;     /special            : set keyword to additionally filter all stations listed in
;                           statfilt for non-background values
;     statfilt            : list of stations to which more stringent filtering is applied
;                           (there seems to be some redundancy with /brw)
;     
; OUTPUTS:
;     lat        (FLOAT)  : station latitude
;     lon        (FLOAT)  : station longitude
;     jahr  IntArr(ndata) : Year of each measurement
;     month IntArr(ndata) : Month of each measurement
;     day   IntArr(ndata) : Day of each measurement
;     ch4   FltArr(ndata) : The CH4 measurements
;     flag  StrArr(ndata) : Flag for each measurement
;     ndata     (INTEGER) : Number of data points
;     characteristics     : strarr(5) with some site characteristics
; 
; LIMITATIONS:
;
; EXAMPLE:
;
; file = '/nas/input/WDCGG/CH4/hourly/y2004/brw471n00.noaa.as.cn.ch4.nl.hr2004.dat'
; contri = 'noaa'
; station  = 'brw'
; special=1
; statfilt=['brw','bkt','cgo']
;
;
; MODIFICATION HISTORY:
; Tina Schnadt,    13/04/2011: first implementation
; Dominik Brunner, 05/01/2017: improved file doc header
;                              small changes to parameter passing
;-

PRO read_wdcgg_data_single_final,file=file,station=station,contri=contri,lat,lon,cal,$
                                 jahr=jahr,month=month,day=day,ch4=ch4,$
                                 flag=flag,ndata=ndata,characteristics=characteristics,$
                                 brw=brw,special=special,statfilt=statfilt,$
                                 gvlat=gvlat,gvtime=gvtime,gvch4=gvch4

  datei = file_search(file)
  print, 'Read WDCGG data : '+datei[0]
  
  ;; intercalibration factors to convert to NOAA04 scale for
  ;; both continuous and flask providers
  CASE contri OF
     'aemet': conv = 1.
     'agage': conv = 1.0003
     'csiro': conv = 1.
     'ec': conv = 1.
     'enea': conv = 1.
     'empa': conv = 1.
     'ipen': conv = 1.
     'jma': conv = 1.
     'kma': conv = 1.013        ; Anmyeon-do has KRISS scale, scale up by a factor of 1.3%
     'gerc': conv = 1.          ; Gosan has NOAA04 scale
     'lsce': conv = 1.0124      ; CMDL83 scale
     'mgo': conv = 1.
     'mpij': conv = 1.
     'mri': conv = 0.9973       ; NIES Gravimetric
     'nier': conv = 1.
     'nies': conv = 0.9973      ; NIES Gravimetric
     'niwa': conv = 1.
     'noaa': conv = 1.
     'rivm': conv = 0.9973
     'rse': conv = 1.
     'saws': conv = 1.
     'ubag': conv = 1.
     'cmdl': conv = 1.0124
  ENDCASE
  
  ;; USA stands for USA standard scale (NIST)  
  calstandard = ['NOAA2004','NOAA04','NOAA-04','WMO','NOAA','KRISS',   'USA','Tohoku','NOAA/CMDL','NIES','NONE',$
                 'indirect', 'MRI', '1999~2006:']
  convfactor  = [      1.,    1.,       1.,      1., 1.0124, 1.013,  0.9973,  1.0003,   1.0124,   0.9973,   1., $
                       1.,0.9973, 1.013]
  
  ;;flo: data provider specific flagging
  
  provider    = ['csiro','ec','jma','mgo','noaa','cmdl','rse','ubag','lsce']
  ;; AEMET, EMPA, ENEA, IPEN, ISAC, KMA,
  ;; LSCE, NIER, NIWA (baseline/no-baseline provided only), 
  ;; RIVM, SAWS do not provide flags, not considered in provider
  nprov          = n_elements(provider)
  
  ;; prepare to remove flagged data
  remove         = StrArr(nprov,15)
  nrem           = IntArr(nprov)
  remove[0,0:12] = ['A..','B..','C..','D..','E..','N..','*..','H..','I..','J..','.M.','.O.','NF.'] ; CSIRO
  remove[1,0:0]  = ['0']                                                                           ; EC
  remove[2,0:1]  = ['2','9']                                                                       ; JMA
  ;; MGO available flags: ['.X','.Z','.N','SX','SZ','SN','MX','MZ','MN','+X','S.X']
  remove[3,0:7]  = ['.X','.N','SX','SN','MX','MN','+X','S.X']                    ; MGO
  remove[4,0:9]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP'] ; NOAA/GMD
  remove[5,0:9]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP'] ; NOAA/CMDL (older NOAA data) 
  remove[6,0:0]  = ['2']                                                         ; RSE
  remove[7,0:2]  = ['3','6','9']                                                 ; UBA
  remove[8,0:3]  = ['N..','+..','III','..S']                                     ; LSCE 
  nrem           = [13,1,2,8,10,10,1,3,4]
  
  ;; additional filtering for non-background values for subset of sites
  IF keyword_set(special) AND n_elements(statfilt) NE 0 THEN BEGIN
     ind = WHERE(station eq statfilt,cfilt)
     IF cfilt eq 1L THEN BEGIN
        print, 'Filtering data for station ', STRMID(file,27,3)
        ;; prepare to remove flagged data
        remove          = StrArr(nprov,17)
        nrem            = IntArr(nprov)
        remove[0,0:16]  = ['A..','B..','C..','D..','E..','N..','*..','H..','I..','J..','.M.',$
                           '.O.','NF.','.F.','.G.','.K.','.L.']                               ; CSIRO
        remove[1,0:0]   = ['0']                                                               ; EC
        remove[2,0:7]   = ['0','1','2','3','4','5','6','9']                                   ; JMA
        ;; MGO available flags: ['.X','.Z','.N','SX','SZ','SN','MX','MZ','MN','+X','S.X']
        remove[3,0:8]   = ['.X','.N','SX','SN','MX','MN','+X','S.X','.Z.']                    ; MGO
        remove[4,0:10]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP','.C.'] ; NOAA/GMD
        remove[5,0:10]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP','.C.'] ; NOAA/CMDL (older NOAA data) 
        remove[6,0:0]   = ['2']                                                               ; RSE
        remove[7,0:2]   = ['3','6','9']                                                       ; UBA
        remove[8,0:3]   = ['N..','+..','III','..S']                                           ; LSCE 
        nrem            = [17,1,8,9,11,11,1,3,4]
        print, 'filtering out non-bg values for station ', STRMID(file,27,3)
     ENDIF
  ENDIF 
  
  lat     = 0.
  lon     = 0.
  cal     = ''
  characteristics    = StrArr(5)
  characteristics[*] = 'xxx'
  
  sline = ''
  
  ;******************************************
  ;*** NOW READ FILE
  ;******************************************

;******************************************
  ;*** 1. READ HEADER
  ;******************************************

  nlines = FILE_LINES(datei[0])  ; number of lines in file i
  openr,lun,datei,/get_lun

  ;; first check number of comment lines
  smatch = ''
  nheader = 0L
  WHILE smatch NE 'DATE  TIME' AND NOT eof(lun) DO BEGIN
     readf,lun,sline
     smatch = STRMID(sline,6,10)
     nheader = nheader + 1L
  ENDWHILE
  IF eof(lun) THEN nheader = 32

  point_lun,lun,0               ; reset to file start

  ndata  = long(nlines)-nheader ; number of data lines in file i
  ;; output arrays
  jahr = IntArr(ndata)
  month = IntArr(ndata)
  day = IntArr(ndata)
  ch4 = FltArr(ndata)
  flag = StrArr(ndata)
  test = FltArr(ndata)

  readf,lun,sline                            ; first line C1
  param = (strsplit((strsplit(sline,':',/extract))[1],' ',/extract))[0]

  readf,lun,sline                            ; second line C2
  result = STRSPLIT(sline,/EXTRACT)          ; from second line extract filename
  characteristics[0] = STRMID(result[3],0,3) ; filename => characteristics[0], station acronym
  FOR k=2,8 DO readf,lun,sline               ; read lines C3 to C9
  result = STRSPLIT(sline,/EXTRACT)          ; from 9th line extract whether platform is stationary of mobile
  characteristics[1] = result[8]             ; extract type of platform => characteristics[1]
  
  IF result[8] ne 'stationary' AND result[8] NE 'vertical' THEN BEGIN ; read data only from stationary platforms
     print,'sorry, station is not a stationary platform: ',result[8]
     free_lun,lun
     RETURN
  ENDIF

  FOR k=9,11 DO readf,lun,sline ; read lines C10 to C12
  result         = STRSPLIT(sline,/EXTRACT)  
  lat            = result[2]    ; extract latitude information => lat
  readf,lun,sline               ; read line C13
  result         = STRSPLIT(sline,/EXTRACT)
  lon            = result[2]       ; extract longitude information => lon
  FOR k=13,18 DO readf,lun,sline   ; read lines C14 to C19
  result         = STRSPLIT(sline,/EXTRACT)
  characteristics[2] = STRMID(result[3],0,4) ; extract start year of measurements
  readf,lun,sline                            ; read line C20
  result         = STRSPLIT(sline,/EXTRACT)
  characteristics[3] = result[3]   ; extract whether measurements are 'event' or 'hourly'
  FOR k=20,22 DO readf,lun,sline   ; read lines C21 to C23
  result       = STRSPLIT(sline,/EXTRACT)
  characteristics[4] = result[3] ; extract whether measurements are 'flask' or 'continuous'

  IF result[3] NE 'flask' AND result[3] NE 'continuous' THEN BEGIN
     print,'sorry, station does not provide continuous or flask measurements'
     free_lun,lun
     RETURN
  ENDIF
  
  readf,lun,sline               ; line C24, time zone, offset from UTC
  result  = STRSPLIT(sline,':',/EXTRACT)
  IF STRMID(result[0],4,9) NE 'TIME ZONE' THEN stop
  utcoffset = 0D                ; default is UTC
  pos = strpos(result[1],'UTC')
  IF pos NE -1 THEN BEGIN
     utcoffset = double(STRMID(result[1],pos+3,10))
  ENDIF
  ;print,'UTC offset = ',utcoffset

  readf,lun,sline               ; line C25, calibration standard
  result  = STRSPLIT(sline,/EXTRACT)
  IF n_elements(result) LT 4 THEN BEGIN
     IF station EQ 'tkb' THEN cal='MRI' ELSE stop
  ENDIF ELSE BEGIN
     cal     = result[3]                ; check calibration standard
  ENDELSE
  FOR k=25,nheader-1 DO readf,lun,sline ; overread rest of header lines

  ;; *******************************************
  ;; * 2. READ DATA
  ;; *******************************************
  FOR k=0L,ndata-1L DO BEGIN
     readf,lun,sline
     output = STRSPLIT(sline,/EXTRACT)
     jahr[k]   = fix(STRMID(output[0],0,4))
     month[k]  = fix(STRMID(output[0],5,2))
     day[k]    = fix(STRMID(output[0],8,2))
     ch4[k]    = float(output[4])
     flag[k]   = output[7]
     IF ch4[k] lt 0. THEN ch4[k] = !VALUES.F_NAN
     IF keyword_set(brw) THEN IF characteristics[0] eq 'brw' and flag[k] eq '.C.' THEN $
        ch4[k] = !VALUES.F_NAN        
     ;;*****************************************
     ;;* intercalibrate data to NOAA04 standard
     ;;*****************************************
     indc      = WHERE(cal eq calstandard,cc)
     IF conv ne convfactor[indc[0]] THEN stop
     IF finite(ch4[k]) eq 1 THEN ch4[k] = ch4[k]*convfactor[indc[0]]
     
     ;;********************************************* 
     ;;* remove flagged data from those
     ;;* providers who indicate flags in their data
     ;;*********************************************
     indp = WHERE(contri eq provider,cp)
     test[k] = ch4[k]
     IF cp eq 1 THEN BEGIN
        indrem = WHERE(flag[k] eq remove[indp,*],crem)
        IF crem gt 0L THEN ch4[k] = !VALUES.F_NAN
     ENDIF                
     
  ENDFOR                        ; end loop over data
  
  free_lun,lun

  ;;********************************************
  ;;* filter out suspiciously high and low data
  ;;********************************************
  ;; high data
  ch4weiter  = ch4
  dayweiter  = day
  monweiter  = month
  jahrweiter = jahr
  flagweiter = flag
  indcheck1 = WHERE(finite(ch4weiter) eq 1,ccheck1)
  IF ccheck1 gt 0L THEN BEGIN        
     miin  = mean(ch4weiter[indcheck1])
     ober  = miin + 4.*stddev(ch4weiter[indcheck1])
     indo  = WHERE(ch4weiter[indcheck1] gt ober,c,complement=include)
     
     ch4on  = ch4weiter[indcheck1[include]]
     dayon  = dayweiter[indcheck1[include]]
     monon  = monweiter[indcheck1[include]]
     jahron = jahrweiter[indcheck1[include]]
     flagon = flagweiter[indcheck1[include]]
     nweiter= n_elements(jahron)
  ENDIF 
  
  IF n_elements(gvlat) NE 0 AND n_elements(gvtime) NE 0 and n_elements(gvch4) NE 0 THEN BEGIN
     ;; identify outliers with too low values using GLOBALVIEW-CH4
     range = 100.
     date         = StrArr(nweiter) ; year and month info
     smon         = StrArr(nweiter)
     indklein     = WHERE(monon lt 10,cklein,complement=indgross)
     smon[indklein] = '0'+STRCOMPRESS(string(monon[indklein]),/REM)
     smon[indgross] = STRCOMPRESS(string(monon[indgross]),/REM)
     date[*]        = STRCOMPRESS(string(jahron),/REM)+smon
     
     indcheck2      = WHERE(abs(gvlat-lat) eq min(abs(gvlat-lat)),cch)
     cminus = 0
     inderr = IntArr(nweiter)  & inderr[*] = 0
     FOR k=0,nweiter-1 DO BEGIN
        indt   = WHERE(date[k] eq gvtime,ct)
        IF ch4on[k] lt gvch4[indt[0],indcheck2[0]]-range THEN BEGIN
           cminus += 1
           inderr[k] = 1
        ENDIF
     ENDFOR
     
     IF cminus gt 0 THEN print, 'Data at station ', characteristics[0], ': ', cminus, $
                                ' suspiciously low measurements'
     cvalid   = nweiter-cminus
     jahr     = IntArr(cvalid) & month = IntArr(cvalid)  & day = IntArr(cvalid)
     ch4      = FltArr(cvalid) & flag  = StrArr(cvalid)        
     dtgvalid = StrArr(cvalid)
     indval   = WHERE(inderr eq 0,cval)
     IF cval gt 0L THEN BEGIN
        dtgvalid[*] = date[indval]
        ch4[*]      = ch4on[indval]
        jahr[*]     = fix(STRMID(date[indval],0,4))
        month[*]    = fix(STRMID(date[indval],4,2))
        day[*]      = dayon[indval]
        flag[*]     = flagon[indval]
        ndata       = cvalid
     ENDIF                
  ENDIF ELSE BEGIN
     ch4      = ch4on
     jahr     = jahron
     month    = monon
     day      = dayon
     flag     = flagon
     ndata    = nweiter
  ENDELSE                       ; check against GlobalView 
  
END
