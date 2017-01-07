;+
; NAME:
;
;   inv_obsvector_mon_weekly_brd
;
; PURPOSE:
;
;   Produce weekly observation data tables for a given inversion simulation.
;
;   The routine loops over all years of the simulations, reads in the
;   observation data corresponding to the specified list of stations, 
;   and writes out weekly averages into monthly files.
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  inv_obsvector_mon_weekly_brd,sim
;
; INPUTS:
;
;       sim:  The simulation structure (see inv_configuration_brd.pro)
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

;; *******************************************************************************
;;  Read station data and process into weekly means
;; *******************************************************************************
;+
; INPUTS:
;    sim:    simulation structure
;    stats:  list of stations
;    contri: contributing network
;    type:   one of 'flask', 'cont', or 'agage'
;
; INPUTS AND OUTPUTS:
;    dtgcollect   : 
;    ch4collect   :
;    namecollect  :
;    latcollect   :
;    loncollect   :
;    numcollect   :
;    typecollect  :
;    a            : start index
;    e            : end index
;-
PRO collect_data,sim,stats,contri,type,year,$
                 dtgcollect=dtgcollect,ch4collect=ch4collect,namecollect=namecollect,$
                 latcollect=latcollect,loncollect=loncollect,numcollect=numcollect,$
                 typecollect=typecollect,a=a,e=e

  IF n_params() NE 5 THEN RETURN

  nstat = n_elements(stats)
  
  nextyear = STRING(year+1,format='(i4)')
  yrdays = round(dtg2gvtime(nextyear+'0101')-dtg2gvtime(year+'0101'))
  days = [31,28,31,30,31,30,31,31,30,31,30,31]
  IF yrdays EQ 366 THEN days[1] = 29

  ;; number of weeks per year (note that here a "week" can be more than 7 days)
  nweekly = 12L*4L

  ;; ***************************************************
  ;; * READ STATION DATA
  ;; ***************************************************
  CASE type OF
     'flask': basedir = sim.wdcggdir+'CH4/event/'
     'cont':  basedir = sim.wdcggdir+'CH4/hourly/y'+year+'/'
     'agage': basedir = sim.wdcggdir+'CH4/event/'
     ELSE: stop
  ENDCASE

  ;; read GLOBALVIEW reference data to identify clear outliers
  ;; Problem: data set only extends to 2009 and is no longer updated by NOAA
  read_globalview_final,latglob=latglob,timeglob=timeglob,ch4glob=ch4glob
  range  = 100.                 ; invalidate values more than 100 ppb below GLOBALVIEW

  nout = 0L
  ndat = 0L
  nhighf = 0L

  FOR i=0,nstat-1 DO BEGIN

     station =stats[i]
        
     CASE type OF
        'flask': BEGIN
           contribution = contri[i]
           file = file_search(basedir+station+'*.'+contribution+'*.dat')
        END
        'cont': BEGIN
           contribution = contri[i]
           file = file_search(basedir+station+'*.'+contribution+'*.dat')
        END
        'agage': BEGIN
           contribution = 'agage'
           file = file_search(basedir+station+'*.agage.*md.ev.dat')
        END
     END
 
     IF n_elements(file) gt 1L THEN BEGIN
        ;; more than one file available, select the one with filtered data
        pos = strpos(file,'filtered')
        index = WHERE(pos NE -1,count)
        IF count GT 0 THEN file = file[index[0]]
     ENDIF

     IF file eq '' THEN BEGIN
        print,'no data for station ',station,' for year ',year
        GOTO,nextstat
     ENDIF

     read_wdcgg_data_single_final,file=file,station=station,contri=contribution,lat,lon,cal,$
                                  jahr=jahr,month=month,day=day,ch4=ch4,flag=flag,ndata=ndata,$
                                  characteristics=characteristics,brw=sim.brw,special=sim.special,$
                                  statfilt=sim.statfilt,gvlat=latglob,gvtime=timeglob,gvch4=ch4glob
     
     jahrin = IntArr(ndata) & monin  = IntArr(ndata) & dayin  = IntArr(ndata) & ch4in = DblArr(ndata)
     jahrin[*] = jahr[0:ndata-1]
     monin[*]  = month[0:ndata-1]
     dayin[*]  = day[0:ndata-1]
     ch4in[*]  = ch4[0:ndata-1]
     
     indcheck = WHERE(finite(ch4in) eq 1,ccheck)
     ndat   += ccheck    
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
     
     determine_dtg,year,jahrweiter,monweiter,dayweiter,dtg=dtg
     
     indy     = WHERE(STRCOMPRESS(string(jahrweiter),/REM) eq year,cy)
     
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
           IF cminus gt 0 THEN print,'At station ',stats[i],' of type',type,': ',cminus,' suspicious measurements'
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
           s   = sort(dtg2gvtime(mm))
           smm = STRMID(timestamp(s),4,2)
           u   = uniq(smm)
           umm = smm(u)
           nmonths = n_elements(umm)
           
           ;; calculate weekly means from daily means
           ch4weekev = DblArr(nweekly)
           dtgweekev = StrArr(nweekly)
           numweekev = IntArr(nweekly)
           nweekev   = 0
           k         = 0        ; data counter
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
                 datmwa = STRCOMPRESS(string(year),/REM)+umm[im]+wmin[j]    ; beginning date week j in month
                 datmwe = STRCOMPRESS(string(year),/REM)+umm[im]+wmax[j]    ; end date of week j in month
                 ind = WHERE(dtg2gvtime(timestamp) ge dtg2gvtime(datmwa) and $
                             dtg2gvtime(timestamp) le dtg2gvtime(datmwe),cweek)
                 IF cweek gt 0L THEN BEGIN
                    ch4weekev[k] = mean(out[ind],/NAN) 
                    nweekev     += 1
                    dtgweekev[k] = STRCOMPRESS(string(year),/REM)+umm[im]+dates[j]+'0000'
                    numweekev[k] = fix(cweek)      
                    k += 1
                 ENDIF
              ENDFOR            ; endloop over weeks in month im
           ENDFOR               ; endloop over months in year
           
           e        += nweekev
           ch4collect[a:e-1]  = ch4weekev[0:nweekev-1]
           dtgcollect[a:e-1]  = dtgweekev[0:nweekev-1]
           namecollect[a:e-1] = stats[i]
           latcollect[a:e-1]  = lat
           loncollect[a:e-1]  = lon
           numcollect[a:e-1]  = numweekev[0:nweekev-1]
           typecollect[a:e-1] = 'ev'
           a        += nweekev
           
        ENDIF                   ; end if data in year are not NaN values       
     ENDIF                      ; end if data are available in selected year
     nextstat:
  ENDFOR                        ; end loop over dat data
  
  ;; store percentage of data invalidated to a log-file
  fileober = sim.outdir+'/log/percentage_data_dismissed_upperthreshold_'+year+'_'+type+'.txt'
  openw,lun1,fileober,/get_lun

  print, 'Number of filtered out high data: ', nout, ', = ', float(nout)/float(ndat)*100., ' % of raw data'
  printf,lun1,year,':',string(nout,format='(i6)'),' invalid points of ',string(ndat,format='(i6)'),$
         ' (',strcompress(float(nout)/float(ndat)*100.,/rem),'%)'   

  free_lun,lun1

END


;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_obsvector_mon_weekly_brd,sim

  ;; get list of stations for this simulation
  inv_stat_definitions_brd,sim,scont=scont,ccont=ccont,sflask=sflask,cflask=cflask,sagage=sagage
  
  nflask = n_elements(sflask)
  ncont = n_elements(scont)
  nhf = n_elements(sagage)

  ;; loop over the years of the inversion
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

     year = STRING(year,format='(i4)')

     ;; indices to collect all data in the collect arrays
     a = 0
     e = 0

     ;; collect flask data
     collect_data,sim,sflask,cflask,'flask',year,$
                  dtgcollect=dtgcollect,ch4collect=ch4collect,namecollect=namecollect,$
                  latcollect=latcollect,loncollect=loncollect,numcollect=numcollect,$
                  typecollect=typecollect,a=a,e=e

     
     IF NOT keyword_set(sim.flask) THEN BEGIN
        ;; collect agage data
        collect_data,sim,sagage,'agage','agage',year,$
                     dtgcollect=dtgcollect,ch4collect=ch4collect,namecollect=namecollect,$
                     latcollect=latcollect,loncollect=loncollect,numcollect=numcollect,$
                     typecollect=typecollect,a=a,e=e

        ;; collect continuous data
        collect_data,sim,scont,ccont,'cont',year,$
                     dtgcollect=dtgcollect,ch4collect=ch4collect,namecollect=namecollect,$
                     latcollect=latcollect,loncollect=loncollect,numcollect=numcollect,$
                     typecollect=typecollect,a=a,e=e
     ENDIF
     
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
     
     s         = sort(dtg2gvtime(dtgall))
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
              fileout = dirout+'z_allweekly_flask_'+sn+'_'+year+umm[i]+'.dat'
           ENDIF ELSE BEGIN
              fileout = dirout+'z_allweekly_'+sn+'_'+year+umm[i]+'.dat'             
              IF keyword_set(special) THEN fileout = dirout+'z_allweekly_special_'+sn+'_'+year+umm[i]+'.dat'
              IF keyword_set(brw) THEN     fileout = dirout+'z_allweekly_brwnobg_'+sn+'_'+year+umm[i]+'.dat' 
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

     print, 'inv_obsvector_mon for year ', year, ' finished.'

  ENDFOR                        ; loop over the years

END

