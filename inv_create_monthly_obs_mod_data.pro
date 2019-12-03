;+
; NAME:
;
;   inv_create_monthly_obs_mod_data
;
; PURPOSE:
;
;   Pre-process all observation and model data for the inversion simulation by
;   a) reading in observation data for complete simulation period 1989-2012
;   b) averaging continuous data to "weekly" data, where each month has 4 weeks
;      and considering only afternoon values at surface sites and early morning values at
;      mountain sites (local time)
;   c) averaging flask data collected on same day to single value
;   d) looping over all months and 
;      - reading in model data 
;      - averaging to same time periods to match exactly the observation data 
;        (in case of event data, take only 3-days mean centered
;        on event, in case of continuous data, average over same days.
;   e) write out both observation and model data to monthly netcdf files 
;
; CATEGORY:
;
;   MAIOLICA-II, observation and model data pre-processing
;
; CALLING SEQUENCE:
;
;   inv_create_monthly_obs_mod_data,obslist=obslist,dlr=dlr
;
; INPUTS:
;
;   none
;
; KEYWORD PARAMETERS:
;
;   obslist    : if provided from a previous call, it is not recomputed
;   /dlr       : set this keyword to process DLR model data
;
; OUTPUTS:
;
;   obslist    : list of observation data. Each list element is a structure
;                containing all data at one site. Some sites have more than 1
;                list element if they have both flask and continous data.
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  writes out monthly netcdf files to directory sim.obsmoddir
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;   event_avg,gvtimes,ch4,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4
;   (Average flask (event) data. For details see procedure header below)
;
;   weekly_avg,gvtimes,ch4,sinfo,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4
;   (Average continous observations to weekly means. See procedure header below)
;
;   model_avg,sim,statid,otype,odtg,mdtg,mdata,pdtg,pdata,ndtg,ndata,$
;                  ach4=ach4,sch4=sch4,atrace=atrace,strace=strace,nch4=nch4
;   (Compute model averages for all observations. For details see procedure header below)
;
;   filter_outliers,gvtimes,ch4,sinfo,typstr,latglob,timeglob,ch4glob,pfilt=pfilt
;   (Filter positive (>3sigma) and negative (< 100 ppb below GlobalView. For details
;   see procedure header below)
;
;   dump_obs_mod_netcdf,sim,yyyymm,obslist,modlist
;   (Write out pre-processed/averaged observation and model data to monthly netcdf
;    files. For details see procedure header below)
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
;   DB, 02 Jan 2018:  first implementation
;
;-

;+
;-----------------------------------------------------------------------------
; NAME:
;   event_avg
;
; PURPOSE:
;   Average all event observation data less than 2 days apart
;   Usually, NOAA takes 2 flask samples at the same time for quality control
;   This routine averages over these two samples.
;
; CALLING SEQUENCE:
;   event_avg,gvtimes,ch4,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4
;
; INPUTS:
;   gvtimes    : observation times in GREDVIS time format
;   ch4        : the corresponding CH4 values
;
; OUTPUTS:
;   agvt       : average date/times of the averages in format YYYYMMDDhhmm
;   agvt       : average date/times of the averages in GREDVIS format
;   ach4       : the corresponding averaged (or non-averaged) CH4 values
;   sch4       : standard deviations of the values used for averaging (or 0)
;   nch4       : number of data points contributing to average (or 1)
;-----------------------------------------------------------------------------
;-
PRO event_avg,gvtimes,ch4,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4

  ndat = n_elements(ch4)
  ach4 = FltArr(ndat)+!values.f_nan
  sch4 = FltArr(ndat)+!values.f_nan
  agvt = DblArr(ndat)
  nch4 = FltArr(ndat)+1.

  k = -1L
  FOR i=1,n_elements(ch4)-1 DO BEGIN
     IF (gvtimes[i]-gvtimes[i-1]) LT 2D THEN BEGIN
        ach4[k]=ach4[k]+ch4[i]
        sch4[k]=sch4[k]+ch4[i]^2
        agvt[k]=agvt[k]+gvtimes[i]
        nch4[k]++
     ENDIF ELSE BEGIN
        k++
        ach4[k]=ch4[i]
        sch4[k]=ch4[i]^2
        agvt[k]=gvtimes[i]
     ENDELSE
  ENDFOR
  
  IF k EQ -1L THEN stop

  ach4 = ach4/nch4
  sch4 = sqrt(sch4/nch4-ach4^2)
  agvt = agvt/nch4

  ;; reduce vectors
  index = WHERE(FINITE(ach4),cnt)
  agvt = agvt[index]
  adtg = gvtime2dtg(agvt)
  ach4 = ach4[index]
  sch4 = sch4[index]
  nch4 = floor(nch4[index])

END

;+
;-----------------------------------------------------------------------------
; NAME:
;   weekly_avg
;
; PURPOSE:
;   Calculate  weekly averages for all months of continuous data. Each month
;   has 4 "weeks", a week having either 7 or 8 days.
;
; CALLING SEQUENCE:
;   weekly_avg,gvtimes,ch4,sinfo,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4
;
; INPUTS:
;   gvtimes    : observation times in GREDVIS time format
;   ch4        : the corresponding CH4 values
;   sinfo      : station information structure (see station_info.pro) with
;                information on station type (surface, tower, mountain) used
;                to subset specific times of the day.
;                Currently, diurnal subsampling is only applied to mountain sites
;                (morning values from 0-6 LT) because afternoon increases due to upslope
;                winds etc. can not be represented by the model.
;                For surface sites, ideally afternoon value should be subsampled when
;                the PBL is highest and near-field influence smallest, but since model
;                values are averages over whole day, such subsampling is difficult to justify.
;
; OUTPUTS:
;   agvt       : average date/times of the averages in format YYYYMMDDhhmm
;   agvt       : average date/times of the averages in GREDVIS format
;   ach4       : the corresponding averaged (or non-averaged) CH4 values
;   sch4       : standard deviations of the values used for averaging (or 0)
;   nch4       : number of data points contributing to average (or 1)
;-----------------------------------------------------------------------------
;-
PRO weekly_avg,gvtimes,ch4,sinfo,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4

  dtg = gvtime2dtg(gvtimes)     ; date/times in format YYYYMMDDhhmm

  ;; unique months
  yyyymm  = STRMID(dtg,0,6)
  u   = uniq(yyyymm)
  umm = yyyymm[u]
  nmonths = n_elements(umm)

  ;; calculate local hour to subset afternoon or morning data
  tshift = sinfo.lon/360.*24
  hh = (fix(STRMID(dtg,8,2))+24+round(tshift)) MOD 24
  IF sinfo.type EQ 'surface' OR sinfo.type EQ 'tower' THEN BEGIN
;     shh = 12 & ehh = 18        ; average afternoon values for surface sites
     shh = 0 & ehh = 24         ; we take all hours because we do the same for the model
  ENDIF ELSE BEGIN
     shh = 0 & ehh = 6          ; average early morning values for mountain sites
;     shh = 0 & ehh = 24         ; we take all hours because we do the same for the model
  ENDELSE

  ;; calculate weekly means from either afternoon (12-18 LT) data (surface and tower sites) or
  ;; early morning 00-06 LT (mountain sites)
  ach4 = FltArr(nmonths*4L)+!values.f_nan
  adtg = StrArr(nmonths*4L)
  sch4 = FltArr(nmonths*4L)+!values.f_nan
  nch4 = LonArr(nmonths*4L)

  ;; loop over the months and average to weekly means
  k = -1L
  FOR im = 0L,nmonths-1L DO BEGIN     

     nmm = STRMID(gvtime2dtg(dtg2gvtime(umm[im]+'01')+35),0,6)
     ndays = round(dtg2gvtime(nmm+'01')-dtg2gvtime(umm[im]+'01'))

     ;; start and end days of each "week"
     CASE ndays OF
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

     ;; loop over the 4 "weeks" of the month and average
     FOR j = 0L,3L DO BEGIN
        datmwa = umm[im]+wmin[j]    ; beginning date week j in month
        datmwe = umm[im]+wmax[j]    ; end date of week j in month
        ind = WHERE(gvtimes GE dtg2gvtime(datmwa+'0000') AND gvtimes LE dtg2gvtime(datmwe+'2359') $
                    AND hh GE shh AND hh LT ehh,cweek)
        IF cweek GT 0L THEN BEGIN
           k++
           ach4[k] = mean(ch4[ind],/nan)
           sch4[k] = stddev(ch4[ind],/nan)
           nch4[k] = cweek
           adtg[k] = umm[im]+dates[j]+'0000'
        ENDIF
     ENDFOR                     ; endloop over weeks in month im
  ENDFOR                        ; endloop over months in year

  IF k EQ -1 THEN stop

  ;; reduce vectors
  ach4 = ach4[0:k]
  adtg = adtg[0:k]
  agvt = dtg2gvtime(adtg)
  sch4 = sch4[0:k]
  nch4 = floor(nch4[0:k])

END

;+
;-----------------------------------------------------------------------------
; NAME:
;   model_avg
;
; PURPOSE:
;   For all observation points of a given station and for a given month
;   compute the corresponding model averages.
;   For continuous data, calculate weekly means 
;   For event data, calculate 3-day averages centered on event.
;   Because this may involve model data from the previous or following month
;   we also need to provide model data from the adjacent months.
;
;   So far no use is made of statistical uncertainties of model output
;
; CALLING SEQUENCE:
;   model_avg,sim,statid,stattyp,odtg,mdtg,mdata,pmdtg,pmdata,nmdtg,nmdata,$
;                  ach4=ach4,sch4=sch4,atrace=atrace,strace=strace,nch4=nch
;
; INPUTS:
;   sim (STRUCTURE)   : the simulation structure, should contain 'all' 91 stations
;   statid            : station ID, e.g. 'alt'
;   otype             : observation type, either 'ev' for event or 'cn' for continuous data
;   odtg              : date/times of (already averaged) observations (YYYYMMDDhhmm)
;   cdtg              : date/times of daily mean model data for current month
;   cdata             : the model data at all 91 receptor points for current month
;   pdtg              : date/times of model data of previous month
;   pdata             : model data of previous month
;   ndtg              : date/times of model data of next month
;   ndata             : model data of next month
;
; KEYWORD INPUTS:
;   nday              : number of days over which to average model data for a given
;                       observation event. Default is three, i.e. average over previous,
;                       current and next day.
;
; OUTPUTS: 
;   ach4              : averaged total model ch4 corresponding to dates odtg
;   sch4              : corresponding standard deviations
;   nch4              : number of model data points (days) available for averaging
;   atrace            : average ch4 tracer data for all dates odtg
;   strace            : corresponding standard deviations
; 
;-----------------------------------------------------------------------------
;-
PRO model_avg,sim,statid,otype,odtg,cdtg,cdata,pdtg,pdata,ndtg,ndata,nday=nday,$
                  ach4=ach4,sch4=sch4,atrace=atrace,strace=strace,nch4=nch4
  
  ;; default average model data over three days for event observations
  IF n_elements(nday) EQ 0 THEN nday = 7

  ;; station index of current receptor
  mind = WHERE(sim.stats EQ statid,cnt)
  IF cnt EQ 0 THEN stop

  ;; additional station information
  sinfo = station_info(statid)
  
  ;; local hour to subset afternoon or morning data depending on station type
  tshift = sinfo.lon/360.*24
  IF sinfo.type EQ 'surface' OR sinfo.type EQ 'tower' THEN BEGIN
;     shh = 12 & ehh = 18        ; average afternoon values for surface sites
     shh = 0 & ehh = 24         ; we take all hours because we do the same for the model
  ENDIF ELSE BEGIN
;     shh = 0 & ehh = 6          ; average early morning values for mountain sites
     shh = 0 & ehh = 24         ; we take all hours because we do the same for the model
  ENDELSE

  ;; total number of tracers
  ntrace = sim.ntrace * sim.nage

  ;; model ch4 tracer values and total CH4 for current receptor
  ;; Attention: first model output tracer is air tracer
  np = n_elements(pdtg) & nn = n_elements(ndtg)

  IF np EQ 0 THEN BEGIN
     ;; data from previous month not available
     mch4trace = transpose([transpose(cdata.ppb[1:ntrace,mind]),$
                            transpose(ndata[0:nday-1].ppb[1:ntrace,mind])])
     mdtg = [cdtg,ndtg[0:nday-1]]
     ;; statistical uncertainties
     ;;mstdch4trace = cdata.std[1:ntrace,mind]
     ;;mstdch4 = sqrt(total(mstdch4trace^2,1))/data.kernweights[mind]
  ENDIF ELSE BEGIN
     IF nn EQ 0 THEN BEGIN
        ;; data from next month not available
        mch4trace = transpose([transpose(pdata[np-nday-1:np-1].ppb[1:ntrace,mind]),$
                               transpose(cdata.ppb[1:ntrace,mind])])
        mdtg = [pdtg[np-nday-1:np-1],cdtg]
     ENDIF ELSE BEGIN
        ;; data from previous and next month available
        mch4trace = transpose([transpose(pdata[np-nday-1:np-1].ppb[1:ntrace,mind]),$
                               transpose(cdata.ppb[1:ntrace,mind]),$
                               transpose(ndata[0:nday-1].ppb[1:ntrace,mind])])
        mdtg = [pdtg[np-nday-1:np-1],cdtg,ndtg[0:nday-1]]
     ENDELSE
  ENDELSE

  mch4 = total(mch4trace,1)
  mgvt = dtg2gvtime(mdtg)-0.5D  ; shift model times by -0.5 days because model time
                                ; represents end of a 1-day averaging interval
                                ; Like this the model time is centered on the day
  hh = (fix(STRMID(mdtg,8,2))+24+round(tshift)) MOD 24

  ;; now average model data for each observation time
  IF otype EQ 'ev' THEN BEGIN
     ndat = n_elements(odtg)
     ach4 = FltArr(ndat)
     atrace = FltArr(ntrace,ndat)
     adtg = StrArr(ndat)
     sch4 = FltArr(ndat)
     strace = FltArr(ntrace,ndat)
     nch4 = LonArr(ndat)

     ;; loop over observations and average model data
     FOR k=0,ndat-1 DO BEGIN
        ogvt = dtg2gvtime(odtg[k])
        ind = WHERE( abs(mgvt-ogvt) LE (nday/2.), cnt)
        IF cnt GT 0 THEN BEGIN
           ach4[k] = mean(mch4[ind],/nan)
           atrace[*,k] = mean(mch4trace[*,ind],dimension=2,/nan)
           sch4[k] = stddev(mch4[ind],/nan)
           strace[*,k] = stddev(mch4trace[*,ind],dimension=2,/nan)
           nch4[k] = cnt
        ENDIF
     ENDFOR

  ENDIF ELSE BEGIN

     ;; calculate weekly means for the 4 weeks of the month
     ach4 = FltArr(4L)
     atrace = FltArr(ntrace,4L)
     adtg = StrArr(4L)
     sch4 = FltArr(4L)
     strace = FltArr(ntrace,4L)
     nch4 = LonArr(4L)

     ;; number of days of this month
     yyyymm  = STRMID(cdtg[0],0,6)
     nyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'01')+40),0,6)
     ndays = round(dtg2gvtime(nyyyymm+'01')-dtg2gvtime(yyyymm+'01'))
     
     ;; start and end days of each "week"
     CASE ndays OF
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
  
     ;; loop over the 4 "weeks" of the month and average
     k=-1L
     FOR j = 0L,3L DO BEGIN

        ;; check if an observation value is available for this date
        index = WHERE(odtg EQ yyyymm+dates[j]+'0000',cnt)
        IF cnt EQ 0 THEN GOTO,nextweek

        datmwa = yyyymm+wmin[j] ; beginning date week j in month
        datmwe = yyyymm+wmax[j] ; end date of week j in month
        ind = WHERE(mgvt GE dtg2gvtime(datmwa+'0000') AND mgvt LE dtg2gvtime(datmwe+'2359') $
                    AND hh GE shh AND hh LT ehh,cweek)
        IF cweek GT 0L THEN BEGIN
           k++
           ach4[k] = mean(mch4[ind],/nan)
           atrace[*,k] = mean(mch4trace[*,ind],dimension=2,/nan)
           sch4[k] = stddev(mch4[ind],/nan)
           strace[*,k] = stddev(mch4trace[*,ind],dimension=2,/nan)
           nch4[k] = cweek
           adtg[k] = yyyymm+dates[j]+'0000'
        ENDIF

        nextweek:
     ENDFOR                     ; endloop over weeks in month im
     
     IF k EQ -1L THEN stop

     ;; reduce vectors
     ach4 = ach4[0:k]
     atrace = atrace[*,0:k]
     adtg = adtg[0:k]
     sch4 = sch4[0:k]
     strace = strace[*,0:k]
     nch4 = nch4[0:k]

     ;; adtg should now be identical to odtg
     IF n_elements(adtg) NE n_elements(odtg) THEN stop 
  ENDELSE

END


;+
;-----------------------------------------------------------------------------
; NAME:
;   filter_outliers
;
; PURPOSE:
;
;    Filter out suspiciously high and low CH4 data using GLOBALVIEW product
;    as a reference.
;
; CALLING SEQUENCE:
;   filter_outliers,gvtimes,ch4,sinfo,typstr,latglob,timeglob,ch4glob,pfilt=pfilt
;
; INPUTS:
;   gvtimes    : observation times in GREDVIS time format
;   ch4        : the corresponding CH4 values
;   sinfo      : station information structure (see station_info.pro)
;   latglob    : latitudes of 2D (time-latitude) GLOBALVIEW product
;   timeglob   : date/times of 2D (time-latitude) GLOBALVIEW product
;   ch4glob    : CH4 values of 2D (time-latitude) GLOBALVIEW product
;
; OUTPUTS:
;   pfilt      : percentage of filtered data
;-----------------------------------------------------------------------------
;-
PRO filter_outliers,gvtimes,ch4,sinfo,latglob,timeglob,ch4glob,pfilt=pfilt,plot=plot

  range  = 100.                 ; invalidate values more than 100 ppb below GLOBALVIEW

  ;; First, remove high values more than 3 sigma higher than deviations from a smooth mean
  
  ndat = n_elements(ch4)
  dt = (gvtimes[ndat-1]-gvtimes[0])/float(ndat) ; average time difference
  
  width = 60./dt < ndat         ; force smoothing window of 60 days
 
  smoothch4 = smooth(ch4,width,/nan,/edge_truncate)

  IF keyword_set(plot) THEN BEGIN
     plot,gvtimes,ch4,title=sinfo.name+' ('+sinfo.id+')'
     oplot,gvtimes,smoothch4,color=5,thick=3
  ENDIF

  nstdev = 3.0                  ; multiples of 1sigma to filter out
  upper = smoothch4 + nstdev*stddev(ch4-smoothch4,/nan)
  indo  = WHERE(ch4 GT upper,cplus,complement=include)
  IF cplus GT 0 THEN BEGIN
     print,'Data at ',sinfo.id,': ', cplus, ' suspiciously high measurements'
     IF keyword_set(plot) THEN oplot,gvtimes[indo],ch4[indo],psym=1,thick=4,color=24
  ENDIF

  ch4 = ch4[include]
  gvtimes = gvtimes[include]
  
  ;; low values lower than GLOBALVIEW CH4 at this latitude - range
  indlat = WHERE(abs(latglob-sinfo.lat) EQ min(abs(latglob-sinfo.lat)),cch)
  
  cminus = 0
  ;; loop over data and compare against GLOBALVIEW for given YYYYMM
  FOR k=0,n_elements(ch4)-1 DO BEGIN
     indt = WHERE(STRMID(gvtime2dtg(gvtimes[k]),0,6) eq timeglob,ct)
     IF ct GT 0 THEN BEGIN
        IF ch4[k] LT ch4glob[indt[0],indlat[0]]-range THEN BEGIN
           IF keyword_set(plot) THEN plots,gvtimes[k],ch4[k],psym=1,thick=4,color=24
           ch4[k]=!values.f_nan
           cminus += 1
        ENDIF
     ENDIF ; otherwise data point is outside of GLOBALVIEW time span (and thus irrelevant for inversion)
  ENDFOR
  
  IF cminus GT 0 THEN print, 'Data at ',sinfo.id, ': ', cminus, ' suspiciously low measurements'
  index = WHERE(finite(ch4),cnt)
  
  ch4 = ch4[index]
  gvtimes = gvtimes[index]
  pfilt = (cminus+cplus)/float(ndat)*100
  print,pfilt,' % of data filtered out'
  wait,0.1

END

;+
; NAME:
;  dump_obs_mod_netcdf
; 
; PURPOSE:
;  write processed observation and model data for current month to netcdf file
;-
PRO dump_obs_mod_netcdf,sim,yyyymm,obslist,modlist

  ;; metadata for global file attributes
  IF NOT keyword_set(sim.dlr) THEN BEGIN
     model = 'FLEXPART-CTM' 
     institute = 'EMPA'
     modeller = 'Stephan Henne, Florian Arfeuille, Dominik Brunner'
  ENDIF ELSE BEGIN
     model = 'EMAC'
     institute = 'DLR'
     modeller = 'Franziska Frank, Patrick Joeckel'
  ENDELSE
  metadata = create_struct($
             "description" , "CH4 observations and model output pre-processed for Kalman smoother", $
             "observations" , "flask and continuous measurements from WDCGG", $
             "model", model, $
             "institute", institute, $
             "modeller", modeller, $
             "references"  , " ", $
             "creator"     , "Dominik Brunner", $
             "email"       , "dominik.brunner@empa.ch", $
             "affiliation" , "Empa Duebendorf, Switzerland", $
             "version"     , '1.0', $
             "date"        ,  systime(/UTC), $
             "study"       , "MAIOLICA-2" )

  ;; total number of data
  ntot = 0 & nmodtot = 0
  FOR i=0,obslist.count()-1 DO BEGIN
     ntot=ntot + n_elements(obslist[i].dtg)
     nmodtot=nmodtot + n_elements(modlist[i].dtg)
  ENDFOR
  
  IF nmodtot NE ntot THEN stop

  ntrace = sim.ntrace*sim.nage

  ;; observation and model data arrays
  och4 = FltArr(ntot)
  ogvt = DblArr(ntot)
  soch4 = FltArr(ntot)
  nodat = LonArr(ntot)
  typ = StrArr(ntot)
  id = StrArr(ntot)
  lon = FltArr(ntot)
  lat = FltArr(ntot)
;  pfilt = FltArr(ntot)
 
  mch4 = FltArr(ntot)
  smch4 = FltArr(ntot)
  mch4trace = FltArr(ntrace,ntot)
  smch4trace = FltArr(ntrace,ntot)
  nmdat = LonArr(ntot)

  ;; fill in the arrays
  cnt = 0L
  FOR i=0,obslist.count()-1 DO BEGIN

     ndat = n_elements(obslist[i].dtg)
     ogvt[cnt:cnt+ndat-1]=obslist[i].gvtimes
     och4[cnt:cnt+ndat-1]=obslist[i].ch4
     soch4[cnt:cnt+ndat-1]=obslist[i].stdch4
     nodat[cnt:cnt+ndat-1]=obslist[i].ndat
     typ[cnt:cnt+ndat-1]=obslist[i].type
     id[cnt:cnt+ndat-1]=obslist[i].id
;     pfilt[cnt:cnt+ndat-1]=obslist[i].pfilt

     mch4[cnt:cnt+ndat-1]=modlist[i].ch4
     smch4[cnt:cnt+ndat-1]=modlist[i].stdch4
     mch4trace[*,cnt:cnt+ndat-1]=modlist[i].ch4trace
     smch4trace[*,cnt:cnt+ndat-1]=modlist[i].stdch4trace
     nmdat[cnt:cnt+ndat-1]=modlist[i].ndat
     cnt = cnt + ndat

  ENDFOR

  ;; sort data in ascending time order
  isort = sort(ogvt)

  och4 = och4[isort]
  ogvt = ogvt[isort]
  soch4 = soch4[isort]
  nodat = nodat[isort]
  typ = typ[isort]
  id = id[isort]
  lon = lon[isort]
  lat = lat[isort]
;  pfilt = pfilt[isort]
 
  mch4 = mch4[isort]
  smch4 = smch4[isort]
  mch4trace = mch4trace[*,isort]
  smch4trace = smch4trace[*,isort]
  nmdat = nmdat[isort]

  ;;**************************************************************************
  ;; write to netcdf file
  ;;**************************************************************************
  ncfile = sim.obsmoddir + 'maiolica2_obs_mod_'+yyyymm+'.nc'

  IF file_test(ncfile) THEN BEGIN
     print,'replacing file ',ncfile
     file_delete, ncfile,/quiet
  ENDIF ELSE print,'writing file ',ncfile

  ncid = NCDF_CREATE(ncfile,/CLOBBER,/NETCDF4_FORMAT)
  NCDF_CONTROL, ncid, /FILL

  ;;******************************************
  ;; global attributes
  ;;******************************************
  metatags = tag_names(metadata)
  FOR i = 0, n_elements(metatags)-1 DO BEGIN
     NCDF_ATTPUT, ncid, metatags[i], metadata.(i), /GLOBAL
  ENDFOR

  ;;******************************************
  ;; create dimensions
  ;;******************************************

  tid = NCDF_DIMDEF(ncid,'time',/UNLIMITED)
  tracid  = NCDF_DIMDEF(ncid,'tracer', ntrace)
  len2id = NCDF_DIMDEF(ncid,'len2', 2)
  len3id = NCDF_DIMDEF(ncid,'len3', 3)
  
  ;;******************************************
  ;; create variables
  ;;******************************************

  dimids = [tracid,tid]

  timeid = NCDF_VARDEF(ncid,"time", [tid],/DOUBLE)
  yyyy = STRMID(yyyymm,0,4)
  reftime = dtg2gvtime(yyyy+'01010000')
  ogvt = ogvt - reftime
  timeString = STRJOIN( [ "days since ",yyyy,"-01-01 00:00" ] )
  NCDF_ATTPUT, ncid, timeid, "units", timeString
  NCDF_ATTPUT, ncid, timeid, "calendar", "proleptic_gregorian"

  tracerid  = NCDF_VARDEF(ncid,'tracer', [tracid], /SHORT)
  NCDF_ATTPUT, ncid, tracerid, "units", '1'
  NCDF_ATTPUT, ncid, tracerid, "standard_name", "tracer_id"

  units = 'ppb'
  missing_value = !values.f_nan

  och4id = NCDF_VARDEF(ncid,'obs_CH4',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, och4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, och4id, 'units',units

  soch4id = NCDF_VARDEF(ncid,'obs_stdev_CH4',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, soch4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, soch4id, 'units',units

  noch4id = NCDF_VARDEF(ncid,'obs_num',[tid],/SHORT)
  NCDF_ATTPUT, ncid, noch4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, noch4id, 'units','1'

  toid = NCDF_VARDEF(ncid,'obs_type',[len2id,tid],/CHAR)
  sid = NCDF_VARDEF(ncid,'statid',[len3id,tid],/CHAR)
  slonid = NCDF_VARDEF(ncid,'lon',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, slonid, 'units','degE'
  slatid = NCDF_VARDEF(ncid,'lat',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, slatid, 'units','degN'
;  spid  = NCDF_VARDEF(ncid,'FILTERED',[tid],/FLOAT)
;  NCDF_ATTPUT, ncid, spid, 'units','percent'
 
  mch4id = NCDF_VARDEF(ncid,'mod_CH4',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, mch4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, mch4id, 'units',units

  mch4tracid = NCDF_VARDEF(ncid,'mod_CH4tracer',dimids,/FLOAT)
  NCDF_ATTPUT, ncid, mch4tracid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, mch4tracid, 'units',units

  smch4id = NCDF_VARDEF(ncid,'mod_stdev_CH4',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, smch4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, smch4id, 'units',units

  smch4tracid = NCDF_VARDEF(ncid,'mod_stdev_CH4tracer',dimids,/FLOAT)
  NCDF_ATTPUT, ncid, smch4tracid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, smch4tracid, 'units',units

  nmch4id = NCDF_VARDEF(ncid,'mod_num',[tid],/SHORT)
  NCDF_ATTPUT, ncid, nmch4id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, nmch4id, 'units','1'

  NCDF_CONTROL, ncid, /ENDEF
  
  ;;******************************************
  ;; fill with data
  ;;******************************************
  NCDF_VARPUT, ncid, timeid, ogvt
  tracers = indgen(ntrace)
  NCDF_VARPUT, ncid, tracerid, tracers

  NCDF_VARPUT, ncid, sid, id
  NCDF_VARPUT, ncid, slonid, lon
  NCDF_VARPUT, ncid, slatid, lat
  NCDF_VARPUT, ncid, toid, typ
  NCDF_VARPUT, ncid, och4id, och4
  NCDF_VARPUT, ncid, soch4id, soch4
  NCDF_VARPUT, ncid, noch4id, nodat
;  NCDF_VARPUT, ncid, spid, pfilt

  NCDF_VARPUT, ncid, mch4id, mch4
  NCDF_VARPUT, ncid, mch4tracid, mch4trace
  NCDF_VARPUT, ncid, smch4id, smch4
  NCDF_VARPUT, ncid, smch4tracid, smch4trace
  NCDF_VARPUT, ncid, nmch4id, nmdat

  NCDF_CLOSE, ncid

END


;------------------------------------------------------------------
;                         main
;------------------------------------------------------------------

PRO inv_create_monthly_obs_mod_data,obslist=obslist,dlr=dlr

  load_ctb,'ive.ctb'

; loop over all months from 198901 to 201212 and read data from
; continuous and flask sites and write to monthly files. For continous sites,
; average to weekly data

  ;; create a simulation structure with all stations: run name is not important
  ;; but value of sconfig needs to be 'all'
  run = '32.8'
  sconfig = 'all'
  sim = inv_configurations(run=run,sconfig=sconfig,dlr=dlr,ok=ok)
  sim.filter=1B
  sim.statfilt = ['brw']

  nstat = n_elements(sim.stats)

  ;; loop over stations and read in all observation data
  IF n_elements(obslist) NE 0 THEN goto,getmodel

  ;; read GLOBALVIEW reference data to identify clear outliers
  ;; Problem: data set only extends to 2009 and is no longer updated by NOAA
  read_globalview_final,sim.inputdir,latglob=latglob,timeglob=timeglob,ch4glob=ch4glob

  obslist = LIST()
  plot = 1                      ; set plot = 1 to display observation data and outlier removal
  FOR i=0,nstat-1 DO BEGIN
     sinfo = station_info(sim.stats[i])

     ;; flask event data
     IF sinfo.flask THEN BEGIN
        basedir = sim.wdcggdir+'CH4/event/'
        contribution = sinfo.flasknetwork
        file = file_search(basedir+sinfo.id+'*.'+contribution+'*.dat')
        typstr = 'ev'
        IF n_elements(file) gt 1L THEN BEGIN
           ;; more than one file available, select the one with filtered data
           pos = strpos(file,'filtered')
           index = WHERE(pos NE -1,count)
           IF count GT 0 THEN file = file[index[0]]
        ENDIF
        IF NOT file_test(file) AND sinfo.id EQ 'tdf' THEN BEGIN
           print,'Attention: station tdf (Tierra del Fuego) has been renamed to ush (Ushuaia)'
           file = file_search(basedir+'ush*.'+contribution+'*.dat')
        ENDIF

        IF file_test(file) THEN BEGIN
           print,'reading flask data from station ',sinfo.id
           read_wdcgg,file=file,contri=contribution,lat=lat,lon=lon,cal=cal,$
                          gvtimes=gvtimes,values=ch4,flag=flag,ndata=ndata,$
                          characteristics=characteristics,filter=sim.filter,$
                          statfilt=sim.statfilt

           filter_outliers,gvtimes,ch4,sinfo,latglob,timeglob,ch4glob,pfilt=pfilt,plot=plot

           event_avg,gvtimes,ch4,agvt=agvt,adtg=adtg,ach4=ach4,sch4=sch4,nch4=nch4

           ;; create data structure and add to list
           rec = {id:sinfo.id,type:typstr,lon:lon,lat:lat,cal:cal,characteristics:characteristics,$
                  pfilt:pfilt,gvtimes:agvt,dtg:adtg,ch4:ach4,stdch4:sch4,ndat:nch4}
           obslist.Add,rec
        ENDIF ELSE BEGIN
           print,'no flask data found for station ',sinfo.id
        ENDELSE
     ENDIF                      ; flask data
     
     ;; continuous agage data
     IF sinfo.agage THEN BEGIN
        basedir = sim.wdcggdir+'CH4/event/'
        contribution = 'agage'
        file = file_search(basedir+sinfo.id+'*.'+contribution+'*.dat')
        typstr = 'cn'           ; treat like continuous data
        
        IF n_elements(file) GT 1 THEN BEGIN
           ;; more than one file available, select the one with filtered data
           pos = strpos(file,'filtered')
           index = WHERE(pos NE -1,count)
           IF count GT 0 THEN file = file[index[0]]
        ENDIF
        IF n_elements(file) GT 1 THEN stop ELSE file = file[0]

        IF file_test(file) THEN BEGIN
           print,'reading AGAGE data from station ',sinfo.id
           read_wdcgg,file=file,contri=contribution,lat=lat,lon=lon,cal=cal,$
                          gvtimes=gvtimes,values=ch4,flag=flag,ndata=ndata,$
                          characteristics=characteristics,filter=sim.filter,$
                          statfilt=sim.statfilt

           filter_outliers,gvtimes,ch4,sinfo,latglob,timeglob,ch4glob,pfilt=pfilt,plot=plot

           ;; average to weekly means
           print,'averaging to weekly means'
           weekly_avg,gvtimes,ch4,sinfo,adtg=adtg,agvt=agvt,ach4=ach4,sch4=sch4,nch4=nch4
           ;; create data structure and add to list
           rec = {id:sinfo.id,type:typstr,lon:lon,lat:lat,cal:cal,characteristics:characteristics,$
                  pfilt:pfilt,gvtimes:agvt,dtg:adtg,ch4:ach4,stdch4:sch4,ndat:nch4}
           obslist.Add,rec
        ENDIF ELSE BEGIN
           print,'no AGAGE data found for station ',sinfo.id
        ENDELSE
     ENDIF                      ; AGAGE data

     ;; other continuous data
     IF sinfo.continuous THEN BEGIN

        contribution = sinfo.contnetwork
        typstr = 'cn'
        first = 1L
        ;; loop over all years from 1989 to 2012
        FOR year = 1989,2012 DO BEGIN
           yyyy=STRING(year,format='(i4)')
           basedir = sim.wdcggdir+'CH4/hourly/y'+yyyy+'/'
           file = file_search(basedir+sinfo.id+'*.'+contribution+'*.dat')
        
           IF n_elements(file) GT 1 THEN BEGIN
              ;; more than one file available, select the one with filtered data
              pos = strpos(file,'filtered')
              index = WHERE(pos NE -1,count)
              IF count GT 0 THEN file = file[index[0]]
           ENDIF

           IF n_elements(file) GT 1 THEN stop ELSE file = file[0]

           ;; do not process data from ryo before 1993 as they are obviously biased
           IF sinfo.id EQ 'ryo' AND year LT 1993 THEN file = 'none'

           IF file_test(file) THEN BEGIN
              IF first THEN print,'reading continuous data from station ',sinfo.id
              read_wdcgg,file=file,contri=contribution,lat=lat,lon=lon,cal=cal,$
                             gvtimes=gvtimes,values=ch4,flag=flag,ndata=ndata,$
                             characteristics=characteristics,filter=sim.filter,$
                             statfilt=sim.statfilt

              ;; restrict data to current year (some sites have data from last day of previous year)
              sgvt = dtg2gvtime(yyyy+'01010000') & egvt = dtg2gvtime(yyyy+'12312359')
              index = WHERE(gvtimes GT sgvt AND gvtimes LT egvt,cnt)
              IF cnt LT n_elements(gvtimes) THEN BEGIN
                 gvtimes = gvtimes[index]
                 ch4 = ch4[index]
              ENDIF

              filter_outliers,gvtimes,ch4,sinfo,latglob,timeglob,ch4glob,pfilt=pfilt,plot=plot

              ;; average to weekly means
              IF first THEN print,'averaging to weekly means'
              weekly_avg,gvtimes,ch4,sinfo,agvt=atgvt,adtg=atdtg,ach4=atch4,sch4=stch4,nch4=ntch4
              IF first THEN BEGIN
                 ach4 = atch4 &  agvt = atgvt
                 adtg = atdtg &  sch4 = stch4
                 nch4 = ntch4 & first = 0L
              ENDIF ELSE BEGIN
                 ach4 = [ach4,atch4] &  agvt = [agvt,atgvt]
                 adtg = [adtg,atdtg] &  sch4 = [sch4,stch4]
                 nch4 = [nch4,ntch4]
              ENDELSE
           ENDIF
        ENDFOR

        IF first THEN BEGIN
           print,'no continuous data found for station ',sinfo.id
        ENDIF ELSE BEGIN
           ;; create data structure and add to list
           rec = {id:sinfo.id,type:typstr,lon:lon,lat:lat,cal:cal,characteristics:characteristics,$
                  pfilt:pfilt,gvtimes:agvt,dtg:adtg,ch4:ach4,stdch4:sch4,ndat:nch4}
           obslist.Add,rec
        ENDELSE
     ENDIF                      ; continuous data

  ENDFOR

  getmodel:


  ;;---------------------------------------------------------------------------
  ;;  Read in model data (stored in monthly receptor output files), average
  ;;  to the date/times of the observation data, and write out processed
  ;;  observation and model data to monthly netcdf files
  ;;  Note: Because model data are averaged over several days centered on all
  ;;  observation points of a given month, we need to manage model data of
  ;;  three consecutive months (previous, current, next)
  ;;---------------------------------------------------------------------------

  ;; number of observation data sets 
  ;(larger than nstat because some stations have both flask and continuous data)
  nset = n_elements(obslist)

  ;; process all months from 198901 to 201212
  syyyymm = '198901' & eyyyymm = '201301' ; last month is not processed anymore
  cyyyymm = syyyymm

  ;; Read in model data for first month
  IF NOT sim.dlr THEN BEGIN
     read_receptors_maiolica,sim,cyyyymm,info=cinfo,data=cdata,dtg=cdtg
  ENDIF ELSE BEGIN
     read_receptors_dlr,sim,cyyyymm,info=cinfo,data=cdata,dtg=cdtg
  ENDELSE

  WHILE cyyyymm NE eyyyymm DO BEGIN
     
     ;; next month
     nyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)

     ;; start and end times of current month
     sgvt = dtg2gvtime(cyyyymm+'01')
     egvt = dtg2gvtime(nyyyymm+'01')

     ;; Read in model data for next month
     IF nyyyymm NE eyyyymm THEN BEGIN
        IF NOT sim.dlr THEN BEGIN
           read_receptors_maiolica,sim,nyyyymm,info=ninfo,data=ndata,dtg=ndtg
        ENDIF ELSE BEGIN
           read_receptors_dlr,sim,nyyyymm,info=ninfo,data=ndata,dtg=ndtg
        ENDELSE
     ENDIF
     
     ;; loop over the data sets and generate corresponding averaged model data
     
     ;; create new lists for observation and model data for current month
     modmlist = LIST()
     obsmlist = LIST() 
     FOR i=0,nset-1 DO BEGIN
        statid = (obslist[i]).id
        stattyp = (obslist[i]).type
        ind = WHERE((obslist[i]).gvtimes GE sgvt AND (obslist[i]).gvtimes LT egvt,cnt)
        IF cnt EQ 0 THEN GOTO,nextset

        ;; limit observation times
        odtg = obslist[i].dtg[ind]
        ogvt = obslist[i].gvtimes[ind]

        ;; create observation data structure and add to list
        rec = {id:statid,type:stattyp,lon:obslist[i].lon,lat:obslist[i].lat,$
               cal:obslist[i].cal,characteristics:obslist[i].characteristics,$
               pfilt:obslist[i].pfilt,gvtimes:ogvt,dtg:odtg,ch4:obslist[i].ch4[ind],$
               stdch4:obslist[i].stdch4[ind],ndat:obslist[i].ndat[ind]}
        obsmlist.Add,rec

        model_avg,sim,statid,stattyp,odtg,cdtg,cdata,pdtg,pdata,ndtg,ndata,ach4=ach4,sch4=sch4,$
                  atrace=atrace,strace=strace,nch4=nch4
        
        ;; create data structure and add to list
        rec = {id:statid,type:stattyp,lon:obslist[i].lon,lat:obslist[i].lat,$
               gvtimes:ogvt,dtg:odtg,ch4:ach4,stdch4:sch4,ch4trace:atrace,$
               stdch4trace:strace,ndat:nch4}
        modmlist.Add,rec
        nextset:
     ENDFOR

     ;; write out observation and model data for current month
     dump_obs_mod_netcdf,sim,cyyyymm,obsmlist,modmlist
     
     pdata = cdata & pdtg = cdtg
     cdata = ndata & cdtg = ndtg

     cyyyymm = nyyyymm

  ENDWHILE


END
