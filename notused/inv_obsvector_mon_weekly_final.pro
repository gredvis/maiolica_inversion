;+
; NAME:
;
;   inv_obsvector_mon_weekly
;
; PURPOSE:
;
;   Read observational data (flask and continuous) for given month
;   into weekly tables
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  inv_obsvector_mon
;
; INPUTS:
;
;       year
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;       flask daily and weekly mean station data (continuous) in predefined order
;       output to /nas/spc134/URMEL/INVERSION/OBSINPUT
;       monthly files, filename: z_yyyymm.dat
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
;   inv_obsvector_mon_weekly
;
;     Read all available data from continuous and flask stations in given year
;     Output daily or weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 19 September 2011
;-

;--------------------------------------------------------------------
PRO determine_dtg,selyear,j,m,d,dtg=dtg

  indy     = WHERE(STRCOMPRESS(string(j),/REM) eq selyear,cy)
  IF cy gt 0L THEN BEGIN
    yyyy     = StrArr(cy)
    mm       = StrArr(cy)
    dd       = StrArr(cy)

    yyyy[*]  = STRCOMPRESS(string(j[indy]),/REM)
    mm[*]    = STRCOMPRESS(string(m[indy]),/REM)
    dd[*]    = STRCOMPRESS(string(d[indy]),/REM)

    indm     = WHERE(m[indy] le 9,cm)
    indd     = WHERE(d[indy] le 9,cd)
    IF cm gt 0L THEN FOR i=0,cm-1 DO mm[indm[i]] = '0'+STRCOMPRESS(string(m[indy[indm[i]]]),/REM)
    IF cd gt 0L THEN FOR i=0,cd-1 DO dd[indd[i]] = '0'+STRCOMPRESS(string(d[indy[indd[i]]]),/REM)     
    dtg      = yyyy+mm+dd+'0000'
  ENDIF

END

;--------------------------------------------------------------------
;*******************************************
; compute daily means of observational data
;*******************************************
PRO daily_means,dtgdat=dtgdat,ch4=ch4,out=ch4daily,timestamp=timestamp,obstime=obstime,ndays=ndays

  k       = 0
  ndays   = 0
  mmonth  = STRMID(dtgdat,4,2) ; which months
  s       = sort(fix(mmonth))
  smonth  = mmonth(s)
  u       = uniq(smonth)
  umonth  = smonth(u)
  nmonths = n_elements(umonth) ; number of months with data

  ch4work   = FltArr(366)
  stampwork = StrArr(366)
  indexwork = IntArr(366)

  FOR im = 0,nmonths-1 DO BEGIN

    indm   = WHERE(mmonth eq umonth[im],cm)
    IF cm gt 0L THEN BEGIN             ; which days in month im
      mdays = STRMID(dtgdat[indm],6,2) ; which days     
      s     = sort(mdays)
      sdays  = mdays(s)
      u      = uniq(sdays)
      udays  = sdays(u)                 ; unique days
      nd     = n_elements(udays)        ; number of unique days

      FOR iu=0,nd-1 DO BEGIN            ; loop over number of unique days
        indday = WHERE(udays[iu] eq mdays,cday)
        IF cday gt 0L THEN BEGIN
          ch4work[k]   = mean(ch4[indm[indday]],/NAN)
          stampwork[k] = dtgdat[indm[indday[0]]]
          IF finite(ch4work[k]) eq 1 THEN ndays += 1
          k += 1
        ENDIF ELSE BEGIN
          ch4work[k] = !VALUES.F_NAN
        ENDELSE 
      ENDFOR
    ENDIF                               ; endif data available in month im
  ENDFOR                                ; end loop over months

  IF ndays gt 0L THEN BEGIN
    ch4daily    = FltArr(ndays)
    timestamp   = StrArr(ndays)
    obstime     = IntArr(ndays)

    ind = WHERE(finite(ch4work[0:ndays-1]) eq 1,cf)
    ch4daily[*]  = ch4work[ind]
    timestamp[*] = stampwork[ind]
    obstime[*]   = indexwork[ind]
  ENDIF ELSE BEGIN
    ch4daily[*] = !VALUES.F_NAN
  ENDELSE

END


;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_obsvector_mon_weekly_final,sim

  print, 'flask= ', sim.flask

  ;; sites with continuous measurements
  statscn  = [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'ngl',   'chm',$
                 'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',$
                 'coi',   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',$
                 'mlo',   'cpt']
  
  ;; contributing network (relevant for scaling)
  contrcn  = [    'ec',  'noaa',   'ec',    'ec',    'ubag',   'ec',    'rivm', 'ubag',  'ec',$
                  'ubag', 'ec',    'ubag',  'empa',  'ubag',   'rse',   'ec',   'ec',$
                  'nies', 'jma',   'kma',   'mri',   'aemet',  'nies',  'jma',  'jma',$
                  'noaa', 'saws']
  
  IF NOT keyword_set(sim.flask) THEN BEGIN
     ;; reduced set of flask sites with event data excluding those that also have continuous data
     statsev  = [   'zep',   'sum',    'ter',  'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',$
                    'lpo',   'esp',    'hpb',  'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',$
                    'pdm',$
                    'bgu',    'nwr',  'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',$
                    'bmw',   'bme',    'wkt',  'wis',   'key',   'ask',   'lln',   'kum',   'cri',$
                    'gmi',   'abp',    'chr',  'bkt',   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',$
                    'ams',   'maa',    'arh',  'bhd',   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',$
                    'hba']
     
     contrev  = [  'noaa',  'noaa',    'mgo', 'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',  'noaa',$
                   'lsce',    'ec',   'noaa', 'noaa',  'lsce',  'cmdl',  'noaa',  'noaa',  'noaa',  'noaa',$
                   'lsce',$
                   'lsce',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'noaa',  'noaa',  'enea',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'noaa', 'csiro',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',$
                   'lsce', 'csiro',   'niwa', 'niwa',  'noaa', 'csiro',  'noaa',  'noaa', 'csiro',  'noaa',$
                   'noaa']
     
  ENDIF ELSE BEGIN
     ;; flask sites with event data
     statsev  = [  'alt',   'brw',    'mhd',  'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',    'ice',  'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',    'puy',  'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',    'azr',  'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',    'key',  'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',    'asc',  'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',    'mqa',  'tdf',   'psa',   'cya',   'syo',   'hba']
     
     contrev  = [  'ec',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',    'ec',  'noaa',  'noaa',$
                   'noaa',  'noaa',$
                   'mgo',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',    'ec',$
                   'noaa',  'noaa',   'lsce', 'cmdl',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',  'lsce',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'enea',  'noaa',  'noaa',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',$
                   'noaa',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'lsce', 'csiro',  'niwa',  'niwa',$
                   'noaa',  'csiro', 'noaa',  'noaa', 'csiro',  'noaa',  'noaa']            
     
  ENDELSE                  

  statsagage   = [ 'mhd', 'thd', 'rpb','smo','cgo']
  
                                ; Lampedusa lmp: NOAA: 2006-12-10 to 2009-12-31
                                ;                ENEA: 1995-01-06 to 2008-12-26
                                ; => take ENEA until end of 2007 and NOAA for 2008/2009
                                ; => check consistency of data sets

                                ; Tina: took out cmn (Monte Cimone), because it's not
                                ; NOAA scale or anything related, 03.11.2011
                                ; took out gsn (Gosan), because it has
                                ; a considerable number of suspicious
                                ; values between 1000 and 1600 ppbv, 19.03.2012
  
                                ; number of stations
  ncn     = n_elements(statscn)
  nev     = n_elements(statsev)  
  nagage  = n_elements(statsagage)
  
  ncont   = 0
  FOR i=0,ncn-1 DO BEGIN
     result = WHERE(statscn[i] eq sim.stats,cn)
     IF cn eq 1L THEN ncont  += cn
     IF cn gt 1L THEN stop
  ENDFOR
  
  nflask  = 0
  FOR i=0,nev-1 DO BEGIN
     result = WHERE(statsev[i] eq sim.stats,cev)
     IF cev eq 1L THEN nflask += 1
     IF cev gt 1L THEN stop
  ENDFOR
  
  nhf     = 0
  FOR i=0,nagage-1 DO BEGIN
     result = WHERE(statsagage[i] eq sim.stats,cagage)
     IF cagage eq 1L THEN nhf += 1
     IF cagage gt 1L THEN stop
  ENDFOR
  
  IF ncont gt 0L  THEN BEGIN
     statscont  = StrArr(ncont)
     contrcont  = StrArr(ncont)
  ENDIF
  IF nflask gt 0L THEN BEGIN
     statsflask = StrArr(nflask)
     contrflask = StrArr(nflask)
  ENDIF    
  IF nhf gt 0L    THEN statshf    = StrArr(nhf)

  IF ncont gt 0L THEN BEGIN  
     j = 0
     FOR i=0,ncn-1 DO BEGIN
        result = WHERE(statscn[i] eq sim.stats,cn)
        IF cn eq 1L THEN BEGIN
           statscont[j] = statscn[i]
           contrcont[j] = contrcn[i]
           j += 1
        ENDIF
     ENDFOR
  ENDIF
  IF nflask gt 0L THEN BEGIN
     j = 0
     FOR i=0,nev-1 DO BEGIN
        result = WHERE(statsev[i] eq sim.stats,cev)
        IF cev eq 1L THEN BEGIN
           statsflask[j] = statsev[i]
           contrflask[j] = contrev[i]
           j += 1
        ENDIF
     ENDFOR
  ENDIF
  IF nhf gt 0L THEN BEGIN
     j = 0
     FOR i=0,nagage-1 DO BEGIN
        result = WHERE(statsagage[i] eq sim.stats,cagage)  
        IF cagage eq 1L THEN BEGIN
           statshf[j] = statsagage[i]
           j += 1
        ENDIF
     ENDFOR
  ENDIF   
  IF keyword_set(sim.flask) THEN ncont = 0
  IF keyword_set(sim.flask) THEN nhf   = 0 
  
  ;; loop over the years
  syear = fix(STRMID(sim.syyyymm,0,4))
  eyear = fix(STRMID(sim.eyyyymm,0,4))
  years = syear + indgen(eyear-syear+1)
  nyear = n_elements(years)
  
  FOR yy = 0,nyear-1 DO BEGIN
     year = years[yy]

     ;; number of weekly data (AGAGE and continuous)
     nweekly = 12L*4L

     nall    = (2L*nweekly+366L)*100L

     dtgcollect  = StrArr(nall)
     ch4collect  = DblArr(nall)
     namecollect = StrArr(nall)
     latcollect  = FltArr(nall)
     loncollect  = FltArr(nall)
     numcollect  = IntArr(nall)
     typecollect = StrArr(nall)

     selyear = STRCOMPRESS(string(year),/REM)
     yrdays  = 365
     IF year eq 1992 or year eq 1996 or year eq 2000 or year eq 2004 or year eq 2008 THEN yrdays = 366
     ppbfact = 1.e9

     days    = [31,28,31,30,31,30,31,31,30,31,30,31]
     IF year eq 1992 or year eq 1996 or year eq 2000 or year eq 2004 or year eq 2008 THEN days[1] = 29

     ;; read GLOBALVIEW reference data to identify clear outliers
     read_globalview_final,latglob=latglob,timeglob=timeglob,ch4glob=ch4glob
     range  = 100.              ; values more than 100 ppb below GLOBALVIEW are invalidated

     ;; indices to collect all data in the collect arrays
     a = 0
     e = 0

     nout = 0L
     nevent = 0L
     nhighf = 0L

     fileober = sim.outdir+'/log/percentage_data_dismissed_upperthreshold_'+selyear+'.txt'
     openw,lun1,fileober,/get_lun

     ;; ***************************************************
     ;; * READ STATION DATA
     ;; ***************************************************
     ;; * First event data
     ;; ***************************************************
     ;;direv = '/nas/input/WDCGG/CH4/event/' 
     ;;direv = '/home/arf/DATA/GAW_WDCGG/ch4/event/' ;flo nov15

     direv = sim.wdcggdir+'CH4/event/'
     FOR i=0,nflask-1 DO BEGIN
        result   = FINDFILE(direv+statsflask[i]+'*.'+contrflask[i]+'*.dat')
        IF n_elements(result) gt 1L THEN stop
        IF result eq '' THEN stop
        datei    = result
        contri   = contrflask[i]
        station  = statsflask[i]
        read_wdcgg_data_single_final,file=datei,station=station,contri=contri,lat,lon,cal,$
                                     jahr=jahr,month=month,day=day,ch4=ch4,flag=flag,ndata=ndata,$
                                     characteristics=characteristics,brw=sim.brw,special=sim.special,$
                                     statfilt=sim.statfilt,gvlat=latglob,gvtime=timeglob,gvch4=ch4glob
        jahrin = IntArr(ndata) & monin  = IntArr(ndata) & dayin  = IntArr(ndata) & ch4in = DblArr(ndata)
        jahrin[*] = jahr[0:ndata-1]
        monin[*]  = month[0:ndata-1]
        dayin[*]  = day[0:ndata-1]
        ch4in[*]  = ch4[0:ndata-1]
        
        indcheck = WHERE(finite(ch4in) eq 1,ccheck)
        nevent   += ccheck    
        IF ccheck gt 0L THEN BEGIN        
           miin  = mean(ch4in[indcheck])
           ober  = miin + 3.*stddev(ch4in[indcheck])
           indo  = WHERE(ch4in[indcheck] gt ober,c,complement=include)
           nout  += c
           
           ch4weiter  = ch4in[indcheck[include]]
           dayweiter  = dayin[indcheck[include]]
           monweiter  = monin[indcheck[include]]
           jahrweiter = jahrin[indcheck[include]]
           ch4weiter  = ch4in[indcheck]
           dayweiter  = dayin[indcheck]
           monweiter  = monin[indcheck]
           jahrweiter = jahrin[indcheck]
        ENDIF ELSE BEGIN
           stop
        ENDELSE   
        
        determine_dtg,selyear,jahrweiter,monweiter,dayweiter,dtg=dtg

        indy     = WHERE(STRCOMPRESS(string(jahrweiter),/REM) eq selyear,cy)

        ;; **************************************************
        ;; only continue if data available in selected year
        IF cy gt 0L THEN BEGIN
           
           index = WHERE(finite(ch4weiter[indy]) eq 1,cex)
           
           IF cex gt 0L THEN BEGIN
              dtgev = StrArr(cex)
              ch4ev = DblArr(cex)
              dtgev[*] = dtg[index]
              ch4ev[*] = ch4weiter[indy[index]]
              
              ;; **************************************************   
              ;; check if there are suspicious values
              indch  = WHERE(abs(latglob-lat) eq min(abs(latglob-lat)),cch)
              date   = STRMID(dtgev,0,6) ; year and month information to compare with GLOBALVIEW monthly data
              cminus = 0
              inderr = IntArr(cy)  & inderr[*] = 0
              FOR k=0,cex-1 DO BEGIN
                 indt   = WHERE(date[k] eq timeglob,ct)
                 IF ch4ev[k] lt ch4glob[indt[0],indch[0]]-range THEN BEGIN
                    cminus += 1
                    inderr[k] = 1
                 ENDIF
              ENDFOR
              IF cminus gt 0 THEN print, 'Event data: at station ', statsflask[i], ': ', cminus, ' suspicious measurements'
              cvalid   = cy-cminus
              dtgvalid = StrArr(cvalid)
              ch4valid = DblArr(cvalid)
              indval   = WHERE(inderr eq 0,cval)
              IF cval gt 0L THEN BEGIN
                 dtgvalid[*] = dtgev[indval]
                 ch4valid[*] = ch4ev[indval]
              ENDIF

              ;; calculate daily means
              daily_means,dtgdat=dtgev,ch4=ch4ev,out=out,timestamp=timestamp,obstime=obstime,ndays=ndays

              ;;***********************************************
              ;; calculate weekly means from daily means  
              ;; final resulting number of analysis
              ;; points/yr are 12 months * 4 weekly means = 48
              ;; and assign dates to these means
              
              mm  = STRMID(timestamp,4,2)
              s   = sort(dtg2hiptime(mm))
              smm = STRMID(timestamp(s),4,2)
              u   = uniq(smm)
              umm = smm(u)
              nmonths = n_elements(umm)
              
              ;; calculate weekly means from daily means
              ch4weekev = DblArr(nweekly)
              dtgweekev = StrArr(nweekly)
              numweekev = IntArr(nweekly)
              nweekev   = 0
              k         = 0     ; data counter
              FOR im = 0,nmonths-1 DO BEGIN
                 index = WHERE(mm EQ umm[im],cnt)
                 ntage = days[fix(umm[im])-1]
                 CASE ntage OF
                    31: BEGIN
                       wmin = ['01','09','17','25'] & wmax = ['08','16','24','31'] & dates = ['04','12','20','28']
                    END
                    30: BEGIN
                       wmin = ['01','09','17','25'] & wmax = ['08','16','24','30'] & dates = ['04','12','20','27']
                    END
                    28: BEGIN
                       wmin = ['01','08','15','23'] & wmax = ['07','14','22','28'] & dates = ['04','11','18','26']
                    END
                    29: BEGIN
                       wmin = ['01','08','15','23'] & wmax = ['07','14','22','29'] & dates = ['04','11','18','26']
                    END
                    ELSE: stop
                 ENDCASE        
                 FOR j = 0,3 DO BEGIN
                    datmwa = STRCOMPRESS(string(year),/REM)+umm[im]+wmin[j] ; beginning date week j in month
                    datmwe = STRCOMPRESS(string(year),/REM)+umm[im]+wmax[j] ; end date of week j in month
                    ind = WHERE(dtg2hiptime(timestamp) ge dtg2hiptime(datmwa) and $
                                dtg2hiptime(timestamp) le dtg2hiptime(datmwe),cweek)
                    IF cweek gt 0L THEN BEGIN
                       ch4weekev[k] = mean(out[ind],/NAN) 
                       nweekev     += 1
                       dtgweekev[k] = STRCOMPRESS(string(year),/REM)+umm[im]+dates[j]+'0000'
                       numweekev[k] = fix(cweek)      
                       k += 1
                    ENDIF
                 ENDFOR         ; endloop over weeks in month im
              ENDFOR            ; endloop over months in year
              
              e        += nweekev
              ch4collect[a:e-1]  = ch4weekev[0:nweekev-1]
              dtgcollect[a:e-1]  = dtgweekev[0:nweekev-1]
              namecollect[a:e-1] = statsflask[i]
              latcollect[a:e-1]  = lat
              loncollect[a:e-1]  = lon
              numcollect[a:e-1]  = numweekev[0:nweekev-1]
              typecollect[a:e-1] = 'ev'
              a        += nweekev
              
           ENDIF                ; end if data in year are not NaN values       
        ENDIF                   ; end if data are available in selected year
     ENDFOR                     ; end loop over event data
     
     print, 'Number of filtered out high event data: ', nout, ', = ', float(nout)/float(nevent)*100., ' % of raw data'
     printf,lun1,selyear,' event: ', nevent, ' ', nout   
     
     IF NOT keyword_set(sim.flask) THEN BEGIN
        ;;**********************************************************
        ;;* READ AGAGE DATA
        ;;* AGAGE listest as event stations, but are high frequency
        ;;**********************************************************
        nag  = 0L
        nout = 0L
        FOR i=0,nhf-1 DO BEGIN
           result   = FINDFILE(direv+statshf[i]+'*.agage.*md.ev.dat')
           datei    = result
           contri   = 'agage'
           station  = statshf[i]

           read_wdcgg_data_single_final,file=datei,station=station,contri=contri,lat,lon,cal,$
                                        jahr=jahr,month=month,day=day,ch4=ch4,flag=flag,ndata=ndata,$
                                        characteristics=characteristics,brw=sim.brw,special=sim.special,$
                                        statfilt=sim.statfilt,gvlat=latglob,gvtime=timeglob,gvch4=ch4glob

           jahrin = IntArr(ndata) & monin  = IntArr(ndata) & dayin  = IntArr(ndata) & ch4in = DblArr(ndata)
           jahrin[*] = jahr[0:ndata-1]
           monin[*]  = month[0:ndata-1]
           dayin[*]  = day[0:ndata-1]
           ch4in[*]  = ch4[0:ndata-1]
           
           indcheck = WHERE(finite(ch4in) eq 1,ccheck)
           nag     += ccheck
           IF ccheck gt 0L THEN BEGIN    
              miin  = mean(ch4in[indcheck])
              ober  = miin + 3.*stddev(ch4in[indcheck])
              indo  = WHERE(ch4in[indcheck] gt ober,c,complement=include)
              nout  += c
              
              ch4weiter  = ch4in[indcheck[include]]
              dayweiter  = dayin[indcheck[include]]
              monweiter  = monin[indcheck[include]]
              jahrweiter = jahrin[indcheck[include]]
              
              ch4weiter  = ch4in[indcheck]
              dayweiter  = dayin[indcheck]
              monweiter  = monin[indcheck]
              jahrweiter = jahrin[indcheck]      
           ENDIF ELSE BEGIN
              stop    
           ENDELSE   
           
           determine_dtg,selyear,jahrweiter,monweiter,dayweiter,dtg=dtg
           
           indy     = WHERE(STRCOMPRESS(string(jahrweiter),/REM) eq selyear,cy)
           ;; **************************************************
           ;; only continue if data available in selected year
           IF cy gt 0L THEN BEGIN
              
              index = WHERE(finite(ch4weiter[indy]) eq 1,cex)
              IF cex gt 0L THEN BEGIN
                 dtgagage = StrArr(cex)
                 ch4agage = DblArr(cex)
                 dtgagage[*] = dtg[index]
                 ch4agage[*] = ch4weiter[indy[index]]
                 
                 ;; **************************************************   
                 ;; check if there are suspicious values
                 indch  = WHERE(abs(latglob-lat) eq min(abs(latglob-lat)),cch)
                 date   = STRMID(dtgagage,0,6) ; year and month information to compare with GLOBALVIEW monthly data
                 cminus = 0
                 inderr = IntArr(cy)  & inderr[*] = 0
                 FOR k=0,cex-1 DO BEGIN
                    indt   = WHERE(date[k] eq timeglob,ct)
                    IF ch4agage[k] lt ch4glob[indt[0],indch[0]]-range THEN BEGIN
                       cminus += 1
                       inderr[k] = 1
                    ENDIF
                 ENDFOR
                 IF cminus gt 0 THEN print, 'At station ', statshf[i], ': ', cminus, ' suspicious measurements'
                 cvalid   = cy-cminus
                 dtgvalid = StrArr(cvalid)
                 ch4valid = DblArr(cvalid)
                 indval   = WHERE(inderr eq 0,cval)
                 IF cval gt 0L THEN BEGIN
                    dtgvalid[*] = dtgagage[indval]
                    ch4valid[*] = ch4agage[indval]
                 ENDIF
                 
                 ;; calculate daily means
                 daily_means,dtgdat=dtgagage,ch4=ch4agage,out=out,timestamp=timestamp,obstime=obstime,ndays=ndays
                 
                 ;;***********************************************
                 ;; calculate weekly means from daily means  
                 ;; final resulting number of analysis
                 ;; points/yr are 12 months * 4 weekly means = 48
                 ;; and assign dates to these means
                 
                 mm  = STRMID(timestamp,4,2)
                 s   = sort(dtg2hiptime(mm))
                 smm = STRMID(timestamp(s),4,2)
                 u   = uniq(smm)
                 umm = smm(u)
                 nmonths = n_elements(umm)
                 
                 ;; calculate weekly means from daily means
                 ch4weekagage = DblArr(nweekly)
                 dtgweekagage = StrArr(nweekly)
                 numweekagage = IntArr(nweekly)
                 nweekagage   = 0
                 k      = 0     ; data counter
                 FOR im = 0,nmonths-1 DO BEGIN
                    index = WHERE(mm EQ umm[im],cnt)
                    ntage = days[fix(umm[im])-1]
                    CASE ntage OF
                       31: BEGIN
                          wmin = ['01','09','17','25'] & wmax = ['08','16','24','31'] & dates = ['04','12','20','28']
                       END
                       30: BEGIN
                          wmin = ['01','09','17','25'] & wmax = ['08','16','24','30'] & dates = ['04','12','20','27']
                       END
                       28: BEGIN
                          wmin = ['01','08','15','23'] & wmax = ['07','14','22','28'] & dates = ['04','11','18','26']
                       END
                       29: BEGIN
                          wmin = ['01','08','15','23'] & wmax = ['07','14','22','29'] & dates = ['04','11','18','26']
                       END
                       ELSE: stop
                    ENDCASE        
                    FOR j = 0,3 DO BEGIN
                       datmwa = STRCOMPRESS(string(year),/REM)+umm[im]+wmin[j] ; beginning date week j in month
                       datmwe = STRCOMPRESS(string(year),/REM)+umm[im]+wmax[j] ; end date of week j in month
                       ind = WHERE(dtg2hiptime(timestamp) ge dtg2hiptime(datmwa) and $
                                   dtg2hiptime(timestamp) le dtg2hiptime(datmwe),cweek)
                       IF cweek gt 0L THEN BEGIN
                          ch4weekagage[k] = mean(out[ind],/NAN) 
                          nweekagage     += 1
                          dtgweekagage[k] = STRCOMPRESS(string(year),/REM)+umm[im]+dates[j]+'0000'
                          numweekagage[k] = fix(cweek)      
                          k += 1
                       ENDIF
                    ENDFOR      ; endloop over weeks in month im
                 ENDFOR         ; endloop over months in year   
                 
                 e         += nweekagage
                 ch4collect[a:e-1]  = ch4weekagage[0:nweekagage-1]
                 dtgcollect[a:e-1]  = dtgweekagage[0:nweekagage-1]
                 namecollect[a:e-1] = statshf[i]
                 latcollect[a:e-1]  = lat
                 loncollect[a:e-1]  = lon
                 numcollect[a:e-1]  = numweekagage[0:nweekagage-1]
                 typecollect[a:e-1] = 'cn'
                 a        += nweekagage
                 
              ENDIF             ; end if available data not NaN values
           ENDIF                ; end if data are available in selected year
        ENDFOR                  ; end loop over event data
        
        print, 'nag= ', nag
        print, 'n agage high data filtered out: ', nout, ', relative fraction: ', float(nout)/float(nag)*100.
        printf,lun1,selyear,' agage: ', nag, ' ', nout 
        
        ;; ***************************************************
        ;; * READ CONTINUOUS DATA
        ;; ***************************************************
        ;;dircn = '/nas/input/WDCGG/CH4/hourly/y'+selyear+'/'
        ;;dircn = '/home/arf/DATA/GAW_WDCGG/ch4/hourly/y'+selyear+'/' ;flo nov15
        dircn = sim.wdcggdir+'CH4/hourly/y'+selyear+'/'
        nout  = 0L
        nhighf = 0L
        
        FOR i=0,ncont-1 DO BEGIN
           
           result = FINDFILE(dircn+statscont[i]+'*.'+contrcont[i]+'*.dat',count=count)
           IF count GT 1 THEN BEGIN
              ;; more than one file available, select the one with filtered data
              pos = strpos(result,'filtered')
              index = WHERE(pos NE -1,count)
              IF count GT 0 THEN result = result[index[0]]
           ENDIF

           IF count gt 0L THEN BEGIN
              datei = result
              station  = statscont[i]
              contri=contrcont[i]

              read_wdcgg_data_single_final,file=datei,station=station,contri=contri,lat,lon,cal,$
                                           jahr=jahr,month=month,day=day,ch4=ch4,flag=flag,ndata=ndata,$
                                           characteristics=characteristics,brw=sim.brw,special=sim.special,$
                                           statfilt=sim.statfilt,gvlat=latglob,gvtime=timeglob,gvch4=ch4glob

              jahrin = IntArr(ndata)
              monin  = IntArr(ndata)
              dayin  = IntArr(ndata)
              ch4in  = DblArr(ndata)
              jahrin[*] = jahr[0:ndata-1]
              monin[*]  = month[0:ndata-1]
              dayin[*]  = day[0:ndata-1]
              ch4in[*]  = ch4[0:ndata-1]
              
              indcheck = WHERE(finite(ch4in) eq 1,ccheck)
              nhighf   += ccheck
              IF ccheck gt 0L THEN BEGIN
                 miin  = mean(ch4in[indcheck])
                 ober  = miin + 3.*stddev(ch4in[indcheck])
                 indo  = WHERE(ch4in[indcheck] gt ober,c,complement=include)
                 nout += c
                 
                 ch4weiter  = ch4in[indcheck[include]]
                 dayweiter  = dayin[indcheck[include]]
                 monweiter  = monin[indcheck[include]]
                 jahrweiter = jahrin[indcheck[include]]
                 
                 ch4weiter  = ch4in[indcheck]
                 dayweiter  = dayin[indcheck]
                 monweiter  = monin[indcheck]
                 jahrweiter = jahrin[indcheck]              
              ENDIF ELSE BEGIN
                 stop
              ENDELSE
              
              determine_dtg,selyear,jahrweiter,monweiter,dayweiter,dtg=dtg
              
              indy      = WHERE(STRCOMPRESS(string(jahrweiter),/REM) eq selyear,cy)
              ;; **************************************************
              ;; only continue if data available in selected year
              IF cy gt 0L THEN BEGIN
                 
                 index = WHERE(finite(ch4weiter[indy]) eq 1,cex)
                 IF cex gt 0L THEN BEGIN
                    dtgcn = StrArr(cex)
                    ch4cn = DblArr(cex)
                    dtgcn[*] = dtg[index]
                    ch4cn[*] = ch4weiter[indy[index]]
                    
                    ;; **************************************************   
                    ;; check if there are suspicious values
                    indch  = WHERE(abs(latglob-lat) eq min(abs(latglob-lat)),cch)
                    date   = STRMID(dtgcn,0,6) ; year and month information to compare with GLOBALVIEW monthly data
                    cminus = 0
                    inderr = IntArr(cy)  & inderr[*] = 0
                    FOR k=0,cex-1 DO BEGIN
                       indt   = WHERE(date[k] eq timeglob,ct)
                       IF ch4cn[k] lt ch4glob[indt[0],indch[0]]-range THEN BEGIN
                          cminus += 1
                          inderr[k] = 1
                       ENDIF
                    ENDFOR
                    IF cminus gt 0 THEN print, 'At station ', statscont[i], ': ', cminus, ' suspicious measurements'
                    cvalid   = cy-cminus
                    dtgvalid = StrArr(cvalid)
                    ch4valid = DblArr(cvalid)
                    indval   = WHERE(inderr eq 0,cval)
                    IF cval gt 0L THEN BEGIN
                       dtgvalid[*] = dtgcn[indval]
                       ch4valid[*] = ch4cn[indval]
                    ENDIF
                    
                    ;; calculate daily means
                    daily_means,dtgdat=dtgcn,ch4=ch4cn,out=out,timestamp=timestamp,obstime=obstime,ndays=ndays
                    
                    ;;***********************************************
                    ;; calculate weekly means from daily means  
                    ;; final resulting number of analysis
                    ;; points/yr are 12 months * 4 weekly means = 48
                    ;; and assign dates to these means
                    
                    mm  = STRMID(timestamp,4,2)
                    s       = sort(dtg2hiptime(mm))
                    smm     = STRMID(timestamp(s),4,2)
                    u       = uniq(smm)
                    umm     = smm(u)
                    nmonths = n_elements(umm)
                    
                    ;; calculate weekly means from daily means
                    ch4weekcn = DblArr(nweekly)
                    dtgweekcn = StrArr(nweekly)
                    numweekcn = IntArr(nweekly)
                    nweekcn   = 0
                    k         = 0 ; data counter
                    FOR im = 0,nmonths-1 DO BEGIN
                       index = WHERE(mm EQ umm[im],cnt)
                       ntage = days[fix(umm[im])-1]
                       print, umm[im], ' ', ntage
                       CASE ntage OF
                          31: BEGIN
                             wmin = ['01','09','17','25'] & wmax = ['08','16','24','31'] & dates = ['04','12','20','28']
                          END
                          30: BEGIN
                             wmin = ['01','09','17','25'] & wmax = ['08','16','24','30'] & dates = ['04','12','20','27']
                          END
                          28: BEGIN
                             wmin = ['01','08','15','23'] & wmax = ['07','14','22','28'] & dates = ['04','11','18','26']
                          END
                          29: BEGIN
                             wmin = ['01','08','15','23'] & wmax = ['07','14','22','29'] & dates = ['04','11','18','26']
                          END
                          ELSE: stop
                       ENDCASE        
                       FOR j = 0,3 DO BEGIN
                          datmwa = STRCOMPRESS(string(year),/REM)+umm[im]+wmin[j] ; beginning date week j in month
                          datmwe = STRCOMPRESS(string(year),/REM)+umm[im]+wmax[j] ; end date of week j in month
                          ind = WHERE(dtg2hiptime(timestamp) ge dtg2hiptime(datmwa) and $
                                      dtg2hiptime(timestamp) le dtg2hiptime(datmwe),cweek)
                          IF cweek gt 0L THEN BEGIN
                             ch4weekcn[k] = mean(out[ind],/NAN)
                             nweekcn     += 1
                             dtgweekcn[k] = STRCOMPRESS(string(year),/REM)+umm[im]+dates[j]+'0000'  
                             numweekcn[k] = fix(cweek)    
                             k += 1
                          ENDIF
                       ENDFOR   ; endloop over weeks in month im
                    ENDFOR      ; endloop over months in year 
                    
                    e        += nweekcn
                    ch4collect[a:e-1]  = ch4weekcn[0:nweekcn-1]
                    dtgcollect[a:e-1]  = dtgweekcn[0:nweekcn-1]
                    namecollect[a:e-1] = statscont[i]
                    latcollect[a:e-1]  = lat
                    loncollect[a:e-1]  = lon
                    numcollect[a:e-1]  = numweekcn[0:nweekcn-1]
                    typecollect[a:e-1] ='cn'
                    a        += nweekcn 
                    
                 ENDIF          ; end if data availabale in year are not NaN
              ENDIF             ; end if data are available in selected year
           ENDIF
           
        ENDFOR                  ; end loop over continuous data
        
        print, 'number of high continuous data filtered out: ', nout, ', relative fraction: ', float(nout)/float(nhighf)*100.
        printf,lun1,selyear,' continuous: ', nhighf, ' ', nout 
        
     ENDIF                      ; IF NOT keyword_set(flask)
     
     ;; ***************************************************
     ;; * SORT DATA
     ;; ***************************************************
     dirout     = sim.obsdir
     n          = e
     sn         = STRCOMPRESS(string(fix(total(ncont+nflask+nhf))),/REM)+'stats'
     dtgall     = StrArr(n)
     ch4all     = DblArr(n)
     nameall    = StrArr(n)
     lonall     = FltArr(n)
     latall     = FltArr(n)
     monall     = StrArr(n)
     numall     = IntArr(n)
     typeall    = StrArr(n)
     dtgall[*]  = dtgcollect[0:n-1] 
     ch4all[*]  = ch4collect[0:n-1]
     nameall[*] = namecollect[0:n-1]
     lonall[*]  = loncollect[0:n-1]
     latall[*]  = latcollect[0:n-1]
     numall[*]  = numcollect[0:n-1]
     monall[*]  = STRMID(dtgall,4,2)
     typeall[*] = typecollect[0:n-1]
     
     s         = sort(dtg2hiptime(dtgall))
     sdtg      = dtgall[s]  & stype = typeall[s] 
     slon      = lonall[s]  & slat  = latall[s] & snum = numall[s]
     sname     = nameall[s] & smon  = monall[s] & sch4 = ch4all[s]  
     
     umm       = ['01','02','03','04','05','06','07','08','09','10','11','12']
     nfiles    = n_elements(umm)
     
     print, 'nfiles = ', nfiles

     FOR i=0,nfiles-1 DO BEGIN

        indm = WHERE(smon eq umm[i],cm)
        print, umm[i], ' ', cm
        IF cm gt 0L THEN BEGIN
           latmon = FltArr(cm) & lonmon = FltArr(cm) & namemon = StrArr(cm) & ch4mon = DblArr(cm) & dtgmon = StrArr(cm)
           nummon = IntArr(cm) & typemon = StrArr(cm)
           latmon[*]  = slon[indm]  & lonmon[*] = slat[indm]
           namemon[*] = sname[indm] & dtgmon[*] = sdtg[indm]
           ch4mon[*]  = sch4[indm]  & nummon[*] = snum[indm] 
           typemon[*] = stype[indm]

           IF keyword_set(flask) THEN BEGIN
              fileout = dirout+'z_allweekly_flask_'+sn+'_'+selyear+umm[i]+'.dat'
           ENDIF ELSE BEGIN
              fileout = dirout+'z_allweekly_'+sn+'_'+selyear+umm[i]+'.dat'             
              IF keyword_set(special) THEN fileout = dirout+'z_allweekly_special_'+sn+'_'+selyear+umm[i]+'.dat'
              IF keyword_set(brw) THEN     fileout = dirout+'z_allweekly_brwnobg_'+sn+'_'+selyear+umm[i]+'.dat' 
           ENDELSE
           print, fileout
           openw,lun,fileout,/get_lun
           FOR j=0,cm-1 DO BEGIN
              IF namemon[j] eq 'bkt' THEN BEGIN
                 dontconsider = ['200610200000','200611040000','200611120000']
                 index = WHERE(dtgmon[j] eq dontconsider,cdont)
                 IF cdont eq 0L THEN BEGIN
                    printf,lun,dtgmon[j],namemon[j],latmon[j],lonmon[j],ch4mon[j],nummon[j],typemon[j],$
                           format='(a12,1x,a3,1x,f8.3,1x,f8.3,1x,f7.2,1x,i4,1x,a2)'
                 ENDIF
              ENDIF ELSE BEGIN
                 printf,lun,dtgmon[j],namemon[j],latmon[j],lonmon[j],ch4mon[j],nummon[j],typemon[j],$
                        format='(a12,1x,a3,1x,f8.3,1x,f8.3,1x,f7.2,1x,i4,1x,a2)'
              ENDELSE        
           ENDFOR
           free_lun,lun
        ENDIF
     ENDFOR

     free_lun,lun1
     print, 'inv_obsvector_mon for year ', selyear, ' finished.'

  ENDFOR                        ; loop over the years

END

