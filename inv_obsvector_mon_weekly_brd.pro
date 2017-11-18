;+
; NAME:
;
;   inv_obsvector_mon_weekly_brd
;
; PURPOSE:
;
;   Produce monthly files with weekly averaged observation data for a 
;   given inversion simulation. Note that a "week" is not necessarily
;   7 days, but each month of the year is split into 4 "weeks".
;
;   The routine loops over all years of the simulation, reads in the
;   observation data corresponding to the specified list of stations, 
;   and writes out weekly averages into monthly files.
;
; CATEGORY:
;
;   inverse modelling, URMEL, MAIOLICA-2, preparation for inversion
;
; CALLING SEQUENCE:
;
;   inv_obsvector_mon_weekly_brd,sim
;
; INPUTS:
;
;   sim:  The simulation structure (see inv_configuration_brd.pro)
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;    None.
;    The program writes out monthly text files with names 'z_allweekly_NNstats_yyyymm.dat'
;    into the directory sim.obdir
;
; COMMON BLOCKS:
;
;    none
;
; SIDE EFFECTS:
;
;    none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   daily_means: 
;        compute daily means for 1 year of observational data
;        Called by routine collect_data
;
;   weekly_means:
;        compute weekly means after computing the daily_means
;
;   collect_data
;        Read station data and process into weekly means
;        Called by main program for each of the three station types
;        ('flask','agage','continuous')
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   CSP 19 September 2011
;   Dominik Brunner, 8 Jan 2017: major overhaul, routines shortened and better
;                organized
;-

;---------------------------- daily means ----------------------------------

;+
; NAME:
;   daily_means
;
; PURPOSE:
;   compute daily means for 1 year of observational data
;
; CALLING SEQUENCE:
;   daily_means,dtgin=dtgin,ch4in=ch4in,ch4out=ch4out,dtgout=dtgout,ndays=ndays
;
; INPUTS:
;    dtgin  StrArr(nin)  : date/time of CH4 data in format YYYYMMDDhhmm
;    ch4in  LonArr(nin)  : CH4 data
;
; OUTPUTS:
;    ch4out FltArr(ndays): daily mean CH4 data
;    dtgout StrArr(ndays): corresponding dates (YYYYMMDD)
;-
PRO daily_means,dtgin=dtgin,ch4in=ch4in,ch4out=ch4out,dtgout=dtgout,ndays=ndays

  ;; sort in ascending order (although should already be sorted)
  ndat = n_elements(dtgin)

  yyyymmdd = STRMID(dtgin,0,8)  ; clip hours
  s = sort(yyyymmdd)
  syyyymmdd=yyyymmdd[s]
  sch4=ch4in[s]

  u = uniq(syyyymmdd)

  ndays = n_elements(u)
  ch4out = FltArr(ndays)
  dtgout = syyyymmdd[u]
  
  ci = 0L
  FOR i = 0L,ndays-1 DO BEGIN
     cnt = 0L
     mval = 0.
     WHILE syyyymmdd[ci] EQ dtgout[i] DO BEGIN
        mval += sch4[ci]
        cnt  += 1L
        ci += 1L
        IF ci EQ ndat THEN BREAK
     ENDWHILE
     ch4out[i] = mval/float(cnt)
  ENDFOR

END

;---------------------------- weekly means ----------------------------------

;+
; NAME:
;   weekly_mean
;
; PURPOSE:
;   compute weekly means for one year of daily mean observation data
;
; CALLING SEQUENCE:
;   weekly_means,dtgin=dtgin,ch4in=ch4in,ch4out=ch4out,dtgout=dtgout,numweek=numweek
;
; INPUTS:
;    dtgin  StrArr(nin)   : date/time of daily mean CH4 data in format YYYYMMDD
;    ch4in  LonArr(nin)   : daily mean CH4 data
;
; OUTPUTS:
;    ch4week FltArr(nweek): weekly mean CH4 data
;    dtgweek StrArr(nweek): corresponding central dates (YYYYMMDD)
;    numweek IntArr(nweek): number of daily measurement data points contributing
;    nweek   Integer      : total number of weeks for which data was found
;-
;---------------------------------------------------------------------------------
PRO weekly_means,dtgin=dtgin,ch4in=ch4in,ch4week=ch4week,dtgweek=dtgweek,$
                 numweek=numweek,nweek=nweek

  ;;***********************************************
  ;; calculate weekly means from daily means  
  ;; final resulting number of analysis
  ;; points/yr are 12 months * 4 weekly means = 48
  ;; and assign dates to these means

  ;; number of weeks per year (note that here a week can be longer than 7 days)
  nweekly = 12L*4L

  ;; unique months
  mm  = STRMID(dtgin,4,2)
  u   = uniq(mm)
  umm = mm[u]
  nmonths = n_elements(umm)
  
  yyyy = STRMID(dtgin[0],0,4)   ; all data should be from same year
  nyyyy = STRING(fix(yyyy)+1,format='(i4)')
  yrdays = round(dtg2gvtime(nyyyy+'0101')-dtg2gvtime(yyyy+'0101'))
  days = [31,28,31,30,31,30,31,31,30,31,30,31]
  IF yrdays EQ 366 THEN days[1] = 29

  ;; calculate weekly means from daily means
  ch4week = DblArr(nweekly)
  dtgweek = StrArr(nweekly)
  numweek = IntArr(nweekly)
  nweek   = 0L
  k       = 0L                  ; data counter

  ;; continuous times
  gvtime = dtg2gvtime(dtgin)
 
  FOR im = 0,nmonths-1 DO BEGIN     
     ;; index of all points belonging to this week
     index = WHERE(mm EQ umm[im],cnt)
     ntage = days[fix(umm[im])-1]

     ;; start and end days of each "week"
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
        datmwa = yyyy+umm[im]+wmin[j]    ; beginning date week j in month
        datmwe = yyyy+umm[im]+wmax[j]    ; end date of week j in month
        ind = WHERE(gvtime GE dtg2gvtime(datmwa) AND gvtime LE dtg2gvtime(datmwe),cweek)
        IF cweek GT 0L THEN BEGIN
           ch4week[k] = mean(ch4in[ind]) 
           nweek     += 1L
           dtgweek[k] = yyyy+umm[im]+dates[j]+'0000'
           numweek[k] = cweek
           k += 1L
        ENDIF
     ENDFOR                     ; endloop over weeks in month im
  ENDFOR                        ; endloop over months in year
  
END

;----------------------------- collect data -------------------------------

;+
; NAME:
;
;  collect_data
;
; PURPOSE:
;
;  Read station data and process into weekly means
;
; INPUTS:
;    sim:    simulation structure
;    stats:  StrArr(nstat) list of stations
;    contri: StrArr(nstat) contributing networks
;    type:   (String) station type, one of 'flask', 'cont', or 'agage'
;    year:   (String) year to process
;
; INPUTS AND OUTPUTS:
;    ; Large arrays into which all data of a given year are collected
;    dtgcollect   : StrArr(nall)
;    ch4collect   : DblArr(nall)
;    namecollect  : StrArr(nall)
;    latcollect   : FltArr(nall)
;    loncollect   : FltArr(nall)
;    numcollect   : IntArr(nall)
;    typecollect  : StrArr(nall)
;    a            : start index (points to first index in collectors)
;    e            : end index (points to last index in collectors)
;-
PRO collect_data,sim,stats,contri,type,year,$
                 dtgcollect=dtgcollect,ch4collect=ch4collect,namecollect=namecollect,$
                 latcollect=latcollect,loncollect=loncollect,numcollect=numcollect,$
                 typecollect=typecollect,a=a,e=e

  IF n_params() NE 5 THEN RETURN

  nstat = n_elements(stats)
  
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

  nplus = 0L
  nminus = 0L
  ndat = 0L
  nhighf = 0L

  FOR i=0,nstat-1 DO BEGIN

     station =stats[i]
        
     CASE type OF
        'flask': BEGIN
           contribution = contri[i]
           file = file_search(basedir+station+'*.'+contribution+'*.dat')
           typstr = 'ev'
        END
        'cont': BEGIN
           contribution = contri[i]
           file = file_search(basedir+station+'*.'+contribution+'*.dat')
           typstr = 'cn'
        END
        'agage': BEGIN
           contribution = 'agage'
           file = file_search(basedir+station+'*.agage.*md.ev.dat')
           typstr = 'cn'        ; treat like continuous data
        END
     END
 
     IF n_elements(file) gt 1L THEN BEGIN
        ;; more than one file available, select the one with filtered data
        pos = strpos(file,'filtered')
        index = WHERE(pos NE -1,count)
        IF count GT 0 THEN file = file[index]
     ENDIF
     file = file[0]

     IF file eq '' THEN BEGIN
        print,'no data for station ',station,' for year ',year
        GOTO,nextstat
     ENDIF

     print,'processing station ',station,' of type ',type,' for year ',year
     read_wdcgg_brd,file=file,contri=contribution,lat=lat,lon=lon,cal=cal,$
                    gvtimes=gvtimes,values=ch4,flag=flag,ndata=ndata,$
                    characteristics=characteristics,brw=sim.brw,special=sim.special,$
                    statfilt=sim.statfilt

     dtg = gvtime2dtg(gvtimes)  ; date/times in format YYYYMMDDhhmm

     ;;************************************************
     ;; limit to valid data in given year
     ;;************************************************
     index = WHERE(finite(ch4) AND STRMID(dtg,0,4) EQ year,cnt)
     IF cnt EQ 0 THEN GOTO,nextstat

     ndat += cnt
     ch4 = ch4[index]
     gvtimes = gvtimes[index]
     dtg = dtg[index]

     ;;************************************************
     ;;* filter out suspiciously high and low CH4 data
     ;;************************************************
     ;; high values more than 3 sigma higher than mean
     nstdev = 3.0               ; multiples of 1sigma to filter out
     miin = mean(ch4)
     ober = miin + nstdev*stddev(ch4)
     indo  = WHERE(ch4 gt ober,cplus,complement=include)
     IF cplus GT 0 THEN print,'Data at ', characteristics[0], ': ', cplus, ' suspiciously high measurements'
     nplus += cplus

     ch4 = ch4[include]
     gvtimes = gvtimes[include]
     dtg = dtg[include]
     
     ;; low values lower than GLOBALVIEW CH4 at this latitude - range
     indlat = WHERE(abs(latglob-lat) EQ min(abs(latglob-lat)),cch)

     cminus = 0
     ;; loop over data and compare against GLOBALVIEW for given YYYYMM
     FOR k=0,n_elements(ch4)-1 DO BEGIN
        indt = WHERE(STRMID(dtg[k],0,6) eq timeglob,ct)
        IF ct EQ 0 THEN stop
        IF ch4[k] LT ch4glob[indt[0],indlat[0]]-range THEN BEGIN
           ch4[k]=!values.f_nan
           cminus += 1
        ENDIF
     ENDFOR
     nminus += cminus

     IF cminus gt 0 THEN print, 'Data at ', characteristics[0], ': ', cminus, ' suspiciously low measurements'
     index = WHERE(finite(ch4),cnt)
     IF cnt EQ 0 THEN GOTO,nextstat

     ch4 = ch4[index]
     gvtimes = gvtimes[index]
     dtg = dtg[index]

     ;; calculate daily means
     ;daily_means,dtgdat=dtg,ch4=ch4,out=out,timestamp=timestamp,ndays=ndays
     daily_means,dtgin=dtg,ch4in=ch4,ch4out=ch4daily,dtgout=dtgdaily,ndays=ndays
 
     ;; calculate weekly means
     weekly_means,dtgin=dtgdaily,ch4in=ch4daily,ch4week=ch4week,dtgweek=dtgweek,$
                  numweek=numweek,nweek=nweek

     e        += nweek
     ch4collect[a:e-1]  = ch4week[0:nweek-1]
     dtgcollect[a:e-1]  = dtgweek[0:nweek-1]
     namecollect[a:e-1] = station
     latcollect[a:e-1]  = lat
     loncollect[a:e-1]  = lon
     numcollect[a:e-1]  = numweek[0:nweek-1]
     typecollect[a:e-1] = typstr
     a        += nweek
     
     nextstat:
  ENDFOR                        ; end loop over dat data
  
  ;; store percentage of data invalidated to a log-file
  fileober = sim.basedir+'/log/percentage_data_dismissed_upperthreshold_'+year+'_'+type+'.txt'
  openw,lun1,fileober,/get_lun

  nout = nplus+nminus
  print, 'Number of filtered out data: ', nout, ', = ', float(nout)/float(ndat)*100., ' % of raw data'
  printf,lun1,year,':',$
         string(nminus,format='(i6)'),' low and ',$
         string(nplus,format='(i6)'),' high points of ',string(ndat,format='(i6)'),' removed',$
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

  ;; number of weeks per year (a week can be more than 7 days)
  nweekly = 12L*4L
  
  ;; total number of weekly data points to hold for a given year
  ;; (origin of formula unclear)
  nall    = (2L*nweekly+366L)*100L

  FOR yy = 0,nyear-1 DO BEGIN
     year = years[yy]

     ;; (re-)initialize collector arrays
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
           latmon = FltArr(cm) & lonmon = FltArr(cm) & namemon = StrArr(cm)
           ch4mon = DblArr(cm) & dtgmon = StrArr(cm)
           nummon = IntArr(cm) & typemon = StrArr(cm)
           latmon[*]  = slon[indm]  & lonmon[*] = slat[indm]
           namemon[*] = sname[indm] & dtgmon[*] = sdtg[indm]
           ch4mon[*]  = sch4[indm]  & nummon[*] = snum[indm] 
           typemon[*] = stype[indm]

           IF keyword_set(sim.flask) THEN BEGIN
              fileout = dirout+'z_allweekly_flask_'+sn+'_'+year+umm[i]+'.dat'
           ENDIF ELSE BEGIN
              fileout = dirout+'z_allweekly_'+sn+'_'+year+umm[i]+'.dat'             
              IF keyword_set(sim.special) THEN fileout = dirout+'z_allweekly_special_'+sn+'_'+year+umm[i]+'.dat'
              IF keyword_set(sim.brw) THEN     fileout = dirout+'z_allweekly_brwnobg_'+sn+'_'+year+umm[i]+'.dat' 
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

