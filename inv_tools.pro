;+
; NAME:
;   inv_tools (library)
;
; PURPOSE
;   library of support routines for the MAIOLICA-II inverse modelling
; 
; PROCEDURES:
;  emiss_categories,main=main,dlr=dlr,labels=labels
;   Get names of all 48 emission categories as well as the corresponding variable
;   names (labels) used in the model (different for DLR than for Empa)
;
;  emiss_colors,groups=groups
;   Color IDs used for plotting individual categories
;
;  plot_category_legend,position=position
;   Plot a figure legend listing all 48 emission categories using
;   the names from emiss_categories() and colors from emiss_colors()
;
;  station_rcpt_levs,stats,levs=levs
;   Get for all receptor locations the optimal receptor altitude
;   (important for mountain and tall tower sites)
;
;  get_week,yyyymmdd,leap=leap,wmin=wmin,wmax=wmax,wcenter=wcenter,iscenter=iscenter
;    For a given day get the corresponding week to which it belongs
;
;  get_sim_dates,sim
;    For a given simulation, compute the string vector yyyymm
;    with all months from sim.syyyymm to sim.eyyyymm
;  
;  station_mapping_empa2dlr,empaid
;    Map station IDs used by Empa onto those used by DLR
;
;  read_receptor_list,sim
;    Get list of receptor points at which FLEXPART output was written for
;    the MAIOLICA-2 simulations. For elevated stations, output was generated 
;    for several heights above ground.
;
;  get_yyyymm
;    For a given simulation, get the vector of months yyyymm
;
;  check_stationlist,sim,yyyymm
;    Check if station info in station_info.pro is compatible with receptor point 
;    locations used for FLEXPART runs
;
;  sim_filename_str,sim,prelim=prelim
;    Get string used in file names for a given simulation
;
;  read_inv_results_netcdf,sim,prelim=prelim,yyyymm=yyyymm,$
;                                 emisonly=emisonly,statonly=statonly
;    Read output of the inversion either for a single month yyyymm or
;    for the complete simulation. Output include prior and posterior emissions
;    (posterior from all 4 optimization steps), their covariances, the
;    correction factors for the oldest age class, and the chi square statistics
;    and log-likelihoods. Set /emisonly to return only prior and posterior
;    emission fields, and set /statonly to return only zlen, dllh, and nobs
;
;  HISTORY:
;   Dominik Brunner (DB)
;   Empa, Swiss Federal Institute of Materials Science and Technology
;
;  DB, 05 Jan 2018: first comprehensively documented version
;-

FUNCTION emiss_categories,main=main,dlr=dlr,labels=labels

  IF keyword_set(main) THEN BEGIN
     cat = ['ANTH','WETL','BB','RICE','TERMITE','OCEAN','VOLC','WILD_anim']
  ENDIF ELSE IF keyword_set(dlr) THEN BEGIN
     cat  = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA',$
             'ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
             'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS',$
             'BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
             'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_CHIN',$
             'WETL_EU','WETL_IND','WETL_MID','WETL_NAbor','WETL_NAtemp','WETL_NAFR',$
             'WETL_RUS','WETL_SAtemp','WETL_SAtrop','WETL_SE_ASIA','WETL_SAFR','WILD_anim',$
             'TERMITES','OCEAN','VOLC']
     nanth=11 & nbb=13 & nwet=13 & nrice=7
     ;; variable names in model receptor output
     labels = ['A'+strcompress(indgen(nanth)+1,/rem),'B'+strcompress(indgen(nbb)+1,/rem),$
               'R'+strcompress(indgen(nrice)+1,/rem),'W'+strcompress(indgen(nwet)+1,/rem),$
               'WAN','TER','OCE','VOL']
     nanth=11 & nbb=13 & nwet=13 & nrice=7
     anth = string(indgen(nanth)+1,format='(i2.2)')
     bb = ['12','13','14','15','19','16','18','17','20','22','23','24','21']
     rice =  string(indgen(nrice)+26,format='(i2.2)')
     wet = ['35','36','37','38','39','40','42','41','43','45','46','47','44']
     other = ['48','33','25','34'] ; WAN, TER, OCE, VOL
     labels = 'tracer_gp_CH4_fx_e'+[anth,bb,rice,wet,other]
  ENDIF ELSE BEGIN
     ;; Florian's version with wrong labelling of wetlands
     ;cat  = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA',$
     ;        'ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
     ;        'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS',$
     ;        'BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
     ;        'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_EU',$
     ;        'WETL_MID','WETL_NAbor','WETL_NAFR','WETL_RUS','WETL_SAtemp','WETL_SAFR',$
     ;        'WETL_CHIN','WETL_IND','WETL_NAtemp','WETL_SAtrop','WETL_SE_ASIA','WILD_anim',$
     ;        'TERMITES','OCEAN','VOLC']
     ;; correct version
     cat  = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA',$
             'ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
             'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS',$
             'BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
             'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_CHIN',$
             'WETL_EU','WETL_IND','WETL_MID','WETL_NAbor','WETL_NAtemp','WETL_NAFR',$
             'WETL_RUS','WETL_SAtemp','WETL_SAtrop','WETL_SE_ASIA','WETL_SAFR','WILD_anim',$
             'TERMITES','OCEAN','VOLC']
     nanth=11 & nbb=13 & nwet=13 & nrice=7
     ;; variable names in model receptor output
     labels = ['A'+strcompress(indgen(nanth)+1,/rem),'B'+strcompress(indgen(nbb)+1,/rem),$
               'R'+strcompress(indgen(nrice)+1,/rem),'W'+strcompress(indgen(nwet)+1,/rem),$
               'WAN','TER','OCE','VOL']
  ENDELSE

  RETURN,cat

END

;---------------------------------------------------------------------------------

;+
; NAME:
;   emiss_colors
; PURPOSE:
;   return color values for individual tracers (emission categories)
;   used for plotting
;   Set keyword /groups to shrink number of colors considerably
;-
FUNCTION emiss_colors,groups=groups
  
  col = [cgcolor("OLIVE"),cgcolor("DARKGREEN"),cgcolor("PALEGREEN"),$
         cgcolor("AQUAMARINE"),cgcolor("GREENYELLOW"),cgcolor("TEAL"),$
         cgcolor("LIGHTSEAGREEN"),cgcolor("GREEN"),cgcolor("KHAKI"),$
         cgcolor("SPRINGGREEN"),cgcolor("LIMEGREEN"),cgcolor("RED"),$
         cgcolor("ORANGERED"),cgcolor("CRIMSON"),cgcolor("FIREBRICK"),$
         cgcolor("SALMON"),cgcolor("DARKRED"),cgcolor("TOMATO"),$
         cgcolor("PINK"),cgcolor("ROSE"),cgcolor("VIOLETRED"),cgcolor("MAGENTA"),$
         cgcolor("SIENNA"),cgcolor("ORANGE"),cgcolor("BEIGE"),cgcolor("SEASHELL"),$
         cgcolor("LIGHTYELLOW"),cgcolor("PAPAYA"),cgcolor("WHEAT"),$
         cgcolor("BURLYWOOD"),cgcolor("LIGHTGRAY"),cgcolor("BLUE"),$
         cgcolor("ROYALBLUE"),cgcolor("NAVY"),cgcolor("STEELBLUE"),$
         cgcolor("CADETBLUE"),cgcolor("CORNFLOWERBLUE"),cgcolor("SKYBLUE"),$
         cgcolor("DARKSLATEBLUE"),cgcolor("PURPLE"),cgcolor("POWDERBLUE"),$
         cgcolor("DODGERBLUE"),cgcolor("YGB3"),cgcolor("TURQUOISE"),$
         cgcolor("BLACK"),cgcolor("YELLOW"),cgcolor("GOLD"),cgcolor("GOLDENROD")]
  
  IF keyword_set(groups) THEN BEGIN
     anth = 11 & bb = 254 & rice=21 & wetl=76 & other=120
     col = [anth,anth,anth,anth,anth,anth,anth,anth,anth,anth,anth,$
            bb,bb,bb,bb,bb,bb,bb,bb,bb,bb,bb,bb,bb,$
            rice,rice,rice,rice,rice,rice,rice,$
            wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,wetl,$
            other,other,other,other]
  ENDIF

  RETURN,col

END

;-------------------------------------------------------------------------------

PRO station_rcpt_levs,inputdir,stats,levs=levs,check=check

  ;; get optimal FLEXPART receptor levels stored in a text file
  rec = {name:'',lev:0}
  allstat = replicate(rec,93)
  file = inputdir+'choice_modellevel_station_93.dat'
  openr,lun,file,/get_lun
  readf,lun,allstat,format='(a3,2x,i4)'
  free_lun,lun

  nstat = n_elements(stats)
  levs = StrArr(nstat)+'-9999'

  IF keyword_set(check) THEN BEGIN
     ;; check how well station level agrees with station altitude
     erafile = maiolicadir+'/input/ERAInterim_surface_fields.nc'
     ncid = ncdf_open(erafile)
     varid = ncdf_varid(ncid,'lon')
     ncdf_varget,ncid,varid,lon
     varid = ncdf_varid(ncid,'lat')
     ncdf_varget,ncid,varid,lat
     varid = ncdf_varid(ncid,'var129')
     ncdf_varget,ncid,varid,z
     ncdf_close,ncid
     z = z / 9.81
     lat = reverse(lat)
     z = reverse(z,2)
     minlon=min(lon) & dlon = 1.
     minlat=min(lat) & dlat = 1.
     
     FOR i=0,nstat-1 DO BEGIN
        index = WHERE(allstat.name EQ stats[i],cnt)
        IF cnt EQ 1 THEN BEGIN
           levs[i]=strcompress(allstat[index[0]].lev,/rem)
           sinfo = station_info(stats[i])
           ;; interpolate z to station coordinate
           ilon = (sinfo.lon-minlon)/dlon
           ilat = (sinfo.lat-minlat)/dlon
           zint = interpolate(z,ilon,ilat)
           IF sinfo.type EQ 'mountain' OR sinfo.type EQ 'tower' THEN BEGIN
              print,''
              print,'Station ',sinfo.name,' (',stats[i],')',', type ',sinfo.type
              print,'True alt  :',sinfo.alt,'; Model alt :',zint+levs[i],' with ',levs[i],' m.a.g.'
           ENDIF
        ENDIF
     ENDFOR
  ENDIF ELSE BEGIN

     ;; subset stations belonging to the station list
     FOR i=0,nstat-1 DO BEGIN
        index = WHERE(allstat.name EQ stats[i],cnt)
        IF cnt EQ 1 THEN levs[i]=strcompress(allstat[index[0]].lev,/rem)
     ENDFOR
  ENDELSE
  
END

;-------------------------------------------------------------------------------------------------------

PRO plot_category_legend,position=position,sort=sort

  IF n_elements(position) NE 4 THEN position=[0,0.1,0.3,0.9]
  IF keyword_set(sort) THEN BEGIN
     sort_emiss_categories,ocats=cat,oind=oind,ocol=col
  ENDIF ELSE BEGIN
     cat = emiss_categories()
     col = emiss_colors()
  ENDELSE

  dy = (position[2]-position[0])/14
  dybox = 0.01
  dxbox = 0.008

  dylab = -0.003
  ;; anthropogenic emissions
  xoffs = 0.0 & yoffs = 0.0
  index = WHERE(strpos(cat,'ANTH') NE -1,ncnt)
  FOR i=0,ncnt-1 DO BEGIN
     top    = position[2] - yoffs -dy*i + dybox/2.
     bottom = position[2] - yoffs -dy*i - dybox/2.
     left   = position[1] + xoffs - dxbox/2.
     right  = position[1] + xoffs + dxbox/2.
     linre  = [left,right,right,left,left]
     obun   = [bottom,bottom,top,top,bottom]
     polyfill,linre,obun,color=col[index[i]],/FILL,/normal
     xyouts,right+0.01,bottom+dylab,cat[index[i]],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR
  
  ;; Wetland emissions
  xoffs = 0.4 & yoffs = 0.0
  index = WHERE(strpos(cat,'WET') NE -1,ncnt)
  FOR i=0,ncnt-1 DO BEGIN
     top    = position[2] - yoffs -dy*i + dybox/2.
     bottom = position[2] - yoffs -dy*i - dybox/2.
     left   = position[1] + xoffs - dxbox/2.
     right  = position[1] + xoffs + dxbox/2.
     linre  = [left,right,right,left,left]
     obun   = [bottom,bottom,top,top,bottom]
     polyfill,linre,obun,color=col[index[i]],/FILL,/normal
     xyouts,right+0.01,bottom+dylab,cat[index[i]],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR

  ;; Biomass burning emissions
  xoffs = 0.2 & yoffs = 0.0
  index = WHERE(strpos(cat,'BB') NE -1,ncnt)
  FOR i=0,ncnt-1 DO BEGIN
     top    = position[2] - yoffs -dy*i + dybox/2.
     bottom = position[2] - yoffs -dy*i - dybox/2.
     left   = position[1] + xoffs - dxbox/2.
     right  = position[1] + xoffs + dxbox/2.
     linre  = [left,right,right,left,left]
     obun   = [bottom,bottom,top,top,bottom]
     polyfill,linre,obun,color=col[index[i]],/FILL,/normal
     xyouts,right+0.01,bottom+dylab,cat[index[i]],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR

  ;; Rice emissions
  xoffs = 0.6 & yoffs = 0.0
  index = WHERE(strpos(cat,'RICE') NE -1,ncnt)
  FOR i=0,ncnt-1 DO BEGIN
     top    = position[2] - yoffs -dy*i + dybox/2.
     bottom = position[2] - yoffs -dy*i - dybox/2.
     left   = position[1] + xoffs - dxbox/2.
     right  = position[1] + xoffs + dxbox/2.
     linre  = [left,right,right,left,left]
     obun   = [bottom,bottom,top,top,bottom]
     polyfill,linre,obun,color=col[index[i]],/FILL,/normal
     xyouts,right+0.01,bottom+dylab,cat[index[i]],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR

  ;; all others emissions
  xoffs = 0.6 & yoffs = 0.15
  index = WHERE(strpos(cat,'RICE') EQ -1 AND strpos(cat,'WETL') EQ -1 AND $
                strpos(cat,'BB') EQ -1 AND strpos(cat,'ANTH') EQ -1,ncnt)
  FOR i=0,ncnt-1 DO BEGIN
     top    = position[2] - yoffs -dy*i + dybox/2.
     bottom = position[2] - yoffs -dy*i - dybox/2.
     left   = position[1] + xoffs - dxbox/2.
     right  = position[1] + xoffs + dxbox/2.
     linre  = [left,right,right,left,left]
     obun   = [bottom,bottom,top,top,bottom]
     polyfill,linre,obun,color=col[index[i]],/FILL,/normal
     xyouts,right+0.01,bottom+dylab,cat[index[i]],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR

END

;----------------------------------------------------------------------------
PRO get_week,yyyymmdd,leap=leap,wmin=wmin,wmax=wmax,wcenter=wcenter,iscenter=iscenter
;+
; PURPOSE:
;   For a given day get the corresponding week to which it belongs.
; 
;  In the inversion the year is separated into 12 * 4 "weeks", where a week
;  can be 7 or 8 days. Each week is identified by a start day, an end day
;  (inclusive) and a central day
;
; INPUTS:
;   yyyymmdd (string) : the day for which to find the correspoinding week 
;   /leap             : set this keyword to indicate that it is a leap year
;                       (not checked in routine to save computation time)
;
; OUTPUTS:
;   wmin (string)     : start of the week in format yyyymmdd
;   wmax (string)     : end of the week
;   wcen (string)     : center of the week
;-

  IF keyword_set(leap) THEN BEGIN
     days = [31,29,31,30,31,30,31,31,30,31,30,31]
  ENDIF ELSE BEGIN
     days = [31,28,31,30,31,30,31,31,30,31,30,31]    
  ENDELSE

  IF STRLEN(yyyymmdd) NE 8 THEN dtg = STRMID(yyyymmdd,0,8) ELSE dtg = yyyymmdd
  yyyy = STRMID(dtg,0,4)
  mm = STRMID(dtg,4,2)
  ndays = days[fix(mm)-1]

  CASE ndays OF
     31: BEGIN
        wmin    = yyyy+mm+['01','09','17','25']
        wmax    = yyyy+mm+['08','16','24','31']
        wcenter = yyyy+mm+['04','12','20','28']
     END
     30: BEGIN
        wmin    = yyyy+mm+['01','09','17','25']
        wmax    = yyyy+mm+['08','16','24','30']
        wcenter = yyyy+mm+['04','12','20','27']
     END
     28: BEGIN
        wmin    = yyyy+mm+['01','08','15','23']
        wmax    = yyyy+mm+['07','14','22','28']
        wcenter = yyyy+mm+['04','11','18','26']
     END
     29: BEGIN
        wmin    = yyyy+mm+['01','08','15','23']
        wmax    = yyyy+mm+['07','14','22','29']
        wcenter = yyyy+mm+['04','11','18','26']
     END
     ELSE: stop
  ENDCASE

  tol = 1/86400D
  gvtmin = dtg2gvtime(wmin)-tol & gvtmax = dtg2gvtime(wmax)+tol
  gvt = dtg2gvtime(dtg)
  index = WHERE(gvt GE gvtmin AND gvt LE gvtmax,cnt)
  IF cnt EQ 0 THEN BEGIN
     print,'date does not belong to any week - weird'
     RETURN
  ENDIF

  wmin = wmin[index[0]] & wmax = wmax[index[0]] & wcenter = wcenter[index[0]]
  iscenter = wcenter EQ dtg

END

;-----------------------------------------------------------------------------

FUNCTION get_sim_dates,sim

  ;; get dates (months) of the simulation
  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  nyear    = eyyyy-syyyy+1
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1 ; number months in inversion
  yyyymm   = StrArr(n)
  cnt = 1
  FOR i=0,n-1 DO BEGIN
     im = i + smm - 1
     yyyy = STRING(syyyy + round(im/12),format='(i4)')
     mm = STRING(im MOD 12 + 1,format='(i2.2)')
     yyyymm[i] = yyyy+mm
  ENDFOR
  
  RETURN,yyyymm

END


;-----------------------------------------------------------------------------

FUNCTION station_mapping_empa2dlr,empaid
;+
;  Map station IDs used by Empa onto those used by DLR
;-

  ;; map an Empa station ID to a DLR station ID  
  CASE strupcase(empaID) OF
     'ABP': dlrid='ARE'
     'ALT': dlrid='ALE' 
     'AMS': dlrid='AMS'
     'AMT': dlrid='ARG'
     'AMY': dlrid='ANM' 
     'ARH': dlrid='ARR'
     'ASC': dlrid='ASC'
     'ASK': dlrid='ASS'
     'AZR': dlrid='TCI'
     'BAL': dlrid='BAL'
     'BGU': dlrid='BEG'
     'BHD': dlrid='BAR'
     'BIK': dlrid='BIA'
     'BKT': dlrid='BUK'
     'BEM': dlrid='BRE'
     'BME': dlrid='STD'
     'BMW': dlrid='TUD'
     'BRW': dlrid='BRW'
     'BSC': dlrid='BLA'
     'CBA': dlrid='COL'
     'CDL': dlrid='CAN'
     'CFA': dlrid='CPF'
     'CGO': dlrid='CPG'         ; Cape Grim
     'CHM': dlrid='CHI'
     'CHR': dlrid='CHR'
     'COI': dlrid='CPO'
     'CPT': dlrid='CPP'
     'CRI': dlrid='CPR'
     'CRZ': dlrid='CRO'
     'CVO': dlrid='CPV'
     'CYA': dlrid='CAS'
     'DEU': dlrid='DEU'
     'EGB': dlrid='EGB'
     'EIC': dlrid='EAS'
     'ESP': dlrid='EST'
     'ETL': dlrid='ETL'
     'FSD': dlrid='FRA'
     'GMI': dlrid='GUA'
     'GOZ': dlrid='missing'     ; Dwejra
     'HAT': dlrid='HAT'
     'HBA': dlrid='HAL'
     'HPB': dlrid='HOH'
     'HUN': dlrid='HEG'
     'ICE': dlrid='HEI'         ; Storhofdi, Iceland, Heimaey
     'IZO': dlrid='IZA'
     'JFJ': dlrid='JUN'
     'KEY': dlrid='KEY'
     'KMW': dlrid='KOL'
     'KUM': dlrid='CPK'
     'KZD': dlrid='SAR'
     'LEF': dlrid='PAR'
     'LLB': dlrid='LAC'
     'LLN': dlrid='LUL'
     'LMP': dlrid='LAM'
     'LPO': dlrid='ILE'
     'MAA': dlrid='MAW'
     'MHD': dlrid='MAC'         ; Mace Head
     'MKN': dlrid='MTK'
     'MLO': dlrid='MAU'         ; Mauna Loa
     'MNM': dlrid='MIN'
     'MQA': dlrid='MQI'         ; Macquarie Island; Florian used MAC
     'NGL': dlrid='NEU'
     'NMB': dlrid='GLO'
     'NWR': dlrid='NIW'
     'OXK': dlrid='OCH'
     'PAL': dlrid='PSA'
     'PDM': dlrid='PIC'
     'PRS': dlrid='PLR'
     'PSA': dlrid='PST'
     'PTA': dlrid='POI'
     'PUY': dlrid='PUY'
     'RPB': dlrid='RAG'
     'RYO': dlrid='RYO'
     'SEY': dlrid='MAH'
     'SGP': dlrid='SGP'
     'SHM': dlrid='SHI'         ; Shemya Island; Florian used SHE (Shetland?)
     'SIS': dlrid='SHE'         ; Lerwick, Shetland Island
     'SMO': dlrid='TUT'         ; Cape Matatula on Tutuila Island, Samoa
     'SSL': dlrid='SCH'
     'SUM': dlrid='SUM'
     'SYO': dlrid='SYO'
     'TAP': dlrid='TAE'
     'TDF': dlrid='TIE'
     'TER': dlrid='TER'
     'THD': dlrid='TRI'
     'TKB': dlrid='TSU'
     'USH': dlrid='TIE'
     'UTA': dlrid='WEN'
     'UUM': dlrid='ULA'
     'WIS': dlrid='SED'
     'WKT': dlrid='MOO'
     'WLG': dlrid='MTW'
     'WSA': dlrid='SAB'         ; Sable Island
     'YON': dlrid='YON'
     'ZEP': dlrid='ZEP'
     'ZGT': dlrid='ZIN'
     'ZSF': dlrid='ZUG'
     'ILEGRANDE': dlrid='ILE'
     'BEGUR': dlrid='BEG'
     ELSE: dlrid = strupcase(empaid)
  ENDCASE
  
  RETURN,dlrid
END


;---------------------------------------------------------------------

FUNCTION read_receptor_list
;+
;  Get list of receptor points at which FLEXPART output was written for
;  the MAIOLICA-2 simulations.
;  The routine returns a structure with receptor point name and coordinates.
;  Note that for elevated stations, output was generated for several heights
;  above ground.
;-

  ;; attention, file 199001 contains 347 receptors, but all files starting
  ;; with 199002 have 341 receptors
  file ='/project/brd134/maiolica/output/final_sim01/19900201/receptor_pptv.nc'
  ncid = ncdf_open(file)
  
  dimyid=ncdf_dimid(ncid,'rec')
  ncdf_diminq,ncid,dimyid,name,nrcpt
  dimtid=ncdf_dimid(ncid,'pnt')
  ncdf_diminq,ncid,dimtid,name,npnt
  dimtid=ncdf_dimid(ncid,'age')
  ncdf_diminq,ncid,dimtid,name,nage

  ncdf_varget,ncid,'rec',rec
  ncdf_varget,ncid,'pnt',pnt
  ncdf_varget,ncid,'age',age
  ncdf_varget,ncid,'receptorname',receptorname
  ncdf_varget,ncid,'lon',lon
  ncdf_varget,ncid,'lat',lat
  ncdf_varget,ncid,'lev',lev
  ncdf_varget,ncid,'hx',hx
  ncdf_varget,ncid,'hy',hy
  ncdf_varget,ncid,'hz',hz
  ncdf_close,ncid

  names = string(receptorname)
  
  rec = {name:'',lon:0.,lat:0.,lev:0.,hx:0.,hy:0.,hz:0.}
  receptors=replicate(rec,nrcpt)
  FOR i=0,nrcpt-1 DO BEGIN
     receptors[i].name=names[i]
     receptors[i].lon=lon[i]
     receptors[i].lat=lat[i]
     receptors[i].lev=lev[i]
     receptors[i].hx=hx[i]
     receptors[i].hy=hy[i]
     receptors[i].hz=hz[i]
  ENDFOR
  RETURN,receptors

END


;; for a given simulation, get the vector of months yyyymm
FUNCTION get_yyyymm,sim

  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing',/continue
     RETURN,-1
  ENDIF

  ;; start and end dates of the simulation
  syyyy    = strmid(sim.syyyymm,0,4) & smm = STRMID(sim.syyyymm,4,2)
  eyyyy    = strmid(sim.eyyyymm,0,4) & emm = STRMID(sim.eyyyymm,4,2)
  n        = fix(eyyyy)*12L+fix(emm)-(fix(syyyy)*12L+fix(smm))+1
  yyyymm   = StrArr(n)
  
  ;; loop over the years and months and get observation and model data
  cyyyymm = syyyy+smm
  FOR i = 0,n-1 DO BEGIN
     yyyymm[i] = cyyyymm
     cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)
  ENDFOR

  RETURN,yyyymm

END

;-------------------------------------------------------------------------
PRO check_stationlist,sim,yyyymm
;+
; Check if station info in station_info.pro is compatible with receptor point locations
; used for FLEXPART runs
;-
  IF n_elements(yyyymm) EQ 0 THEN yyyymm='200501'
  read_receptors_maiolica,sim,yyyymm,info=info,data=data,dtg=dtg

  FOR i=0,n_elements(info)-1 DO BEGIN
     sstr = STRSPLIT(info[i].rcptname,' ',/extract)
     IF n_elements(sstr) GT 1 THEN alt = sstr[1]
     id = sstr[0]
     sinfo = station_info(id)
     IF abs(sinfo.lon-info[i].xrcpt) GT 0.01 OR abs(sinfo.lat-info[i].yrcpt) GT 0.01 THEN BEGIN
        print,'------------------'
        print,i,' ',sinfo.id,' ',sinfo.name
        print,'sinfo lon = ',sinfo.lon,', rcpt lon = ',info[i].xrcpt
        print,'sinfo lat = ',sinfo.lat,', rcpt lat = ',info[i].yrcpt
        print,'sinfo alt = ',sinfo.alt,', rcpt alt = ',info[i].zrcpt
        print,''
        stop
     ENDIF ELSE BEGIN
        print,i,' ',sinfo.id,' ',sinfo.name,' is ok'
     ENDELSE
  ENDFOR

END

;------------------------------------------------------------------------------
;+
; Get a substring used for filenames for a given simulation
FUNCTION sim_filename_str,sim,prelim=prelim
  sstr = ''
  IF keyword_set(prelim) THEN sstr = 'prelim_'
  IF keyword_set(sim.flask) THEN sstr = sstr + 'flask_'
  IF keyword_set(sim.filter) THEN sstr = sstr + 'filter_'
  sstr = sstr +sim.sn + '_' + sim.name + '_' + sim.qunc + '_'
  RETURN,sstr
END


;----------------------------------------------------

FUNCTION read_inv_results_netcdf,sim,prelim=prelim,yyyymm=yyyymm,$
                                 emisonly=emisonly,statonly=statonly

  IF n_elements(sim) EQ 0 THEN RETURN,-1
 
  sstr =sim_filename_str(sim,prelim=prelim)
  ncfile = sim.outdir + 'inv_result_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'
  
  IF file_test(ncfile) EQ 0 THEN BEGIN
     print,'inversion output file ',ncfile,' not found'
     RETURN,-1
  ENDIF

  print,'reading results from file ',ncfile
  ncid = NCDF_OPEN(ncfile)
  
  ;; get dimensions
  ncdf_diminq, ncid, 0, timedim, ntime
  ncdf_diminq, ncid, 1, tracdim, ntrace

  ;; read times
  varid = ncdf_varid(ncid,'year')
  ncdf_varget,ncid,varid,yyyy
  varid = ncdf_varid(ncid,'month')
  ncdf_varget,ncid,varid,mm

  allyyyymm = string(yyyy)+string(mm)
  
  toffset = 0 & tcount = ntime
  tracoffset = 0 & traccount = ntrace

  IF n_elements(yyyymm) NE 0 THEN BEGIN
     ;; read in only data for this month
     index = WHERE(allyyyymm EQ yyyymm,cnt)
     IF cnt EQ 0 THEN BEGIN
        print,'no data found for month ',yyyymm
        ncdf_close,ncid
        RETURN,-1
     ENDIF ELSE BEGIN
        toffset = index[0] & tcount = 1
        allyyyymm = yyyymm
     ENDELSE
  ENDIF

  IF keyword_set(statonly) THEN GOTO,statonly

  offset = [toffset,0L] & count = [tcount,traccount]

  varid = ncdf_varid(ncid,'emis_apri')
  ncdf_varget,ncid,varid,emis_apri,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_apost')
  ncdf_varget,ncid,varid,emis_apost,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_apost1')
  ncdf_varget,ncid,varid,emis_apost1,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_apost2')
  ncdf_varget,ncid,varid,emis_apost2,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_apost3')
  ncdf_varget,ncid,varid,emis_apost3,offset=offset,count=count

  varid = ncdf_varid(ncid,'scalef')
  ncdf_varget,ncid,varid,scalef,offset=offset,count=count

  IF keyword_set(emisonly) THEN BEGIN
     result = {yyyymm:allyyyymm,emis_apri:emis_apri,emis_apost:emis_apost,$
              emis_apost1:emis_apost1,emis_apost2:emis_apost2,$
               emis_apost3:emis_apost3,scalef:scalef}
     ncdf_close,ncid
     RETURN,result
  ENDIF

  statonly:
  varid = ncdf_varid(ncid,'zlen')
  ncdf_varget,ncid,varid,zlen,offset=toffset,count=tcount

  varid = ncdf_varid(ncid,'dllh')
  ncdf_varget,ncid,varid,dllh,offset=toffset,count=tcount

  varid = ncdf_varid(ncid,'nobs')
  ncdf_varget,ncid,varid,nobs,offset=toffset,count=tcount

  IF keyword_set(statonly) THEN BEGIN
     result = {yyyymm:allyyyymm,zlen:zlen,dllh:dllh,nobs:nobs}
     ncdf_close,ncid
     RETURN,result
  ENDIF

  offset = [toffset,0L,0L] & count = [tcount,traccount,traccount]

  varid = ncdf_varid(ncid,'emis_cov')
  ncdf_varget,ncid,varid,emis_cov,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_cov1')
  ncdf_varget,ncid,varid,emis_cov1,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_cov2')
  ncdf_varget,ncid,varid,emis_cov2,offset=offset,count=count

  varid = ncdf_varid(ncid,'emis_cov3')
  ncdf_varget,ncid,varid,emis_cov3,offset=offset,count=count

  ncdf_close,ncid

  result = {yyyymm:allyyyymm,emis_apri:emis_apri,emis_apost:emis_apost,$
            emis_apost1:emis_apost1,emis_apost2:emis_apost2,$
            emis_apost3:emis_apost3,cov_apost:emis_cov,$
            cov_apost1:emis_cov1,cov_apost2:emis_cov2,cov_apost3:emis_cov3,$
            scalef:scalef,zlen:zlen,dllh:dllh,nobs:nobs}   

  RETURN,result

END

;------------------------------------------------------------------------

PRO read_inv_station_output_netcdf,sim,prelim=prelim,yyyymm=yyyymm,prior=prior,ok=ok,$
                                   ndata=ndata,ch4obs=ch4obs,ch4apri=ch4apri,ch4post=ch4post,$
                                   catapri=catapri,catpost=catpost,cats=cats,stats=stats

  ok = 0
  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing',/continue
     RETURN
  ENDIF
  
  sstr =sim_filename_str(sim,prelim=prelim)
  ncfile = sim.outdir + 'inv_station_output_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'
  
  IF file_test(ncfile) EQ 0 THEN BEGIN
     message,'inversion output file '+ncfile+' not found',/continue
     RETURN
  ENDIF

  print,'reading monthly mean station output from file ',ncfile
  ncid = NCDF_OPEN(ncfile)
  
  ;; get dimensions
  ncdf_diminq, ncid, 0, timedim, ntime
  ncdf_diminq, ncid, 1, tracdim, ntrace
  ncdf_diminq, ncid, 2, statdim, nstat

  ;; read times
  varid = ncdf_varid(ncid,'year')
  ncdf_varget,ncid,varid,yyyy
  varid = ncdf_varid(ncid,'month')
  ncdf_varget,ncid,varid,mm

  yyyymm = string(yyyy)+string(mm)
  
  toffset = 0L & tcount = ntime
  tracoffset = 0L & traccount = ntrace
  statoffset = 0L & statcount = nstat

  ;; IF n_elements(yyyymm) NE 0 THEN BEGIN
  ;;    ;; read in only data for this month
  ;;    index = WHERE(allyyyymm EQ yyyymm,cnt)
  ;;    IF cnt EQ 0 THEN BEGIN
  ;;       message,'no data found for month '+yyyymm,/continue
  ;;       ncdf_close,ncid
  ;;       RETURN
  ;;    ENDIF ELSE BEGIN
  ;;       toffset = index[0] & tcount = 1
  ;;       allyyyymm = yyyymm
  ;;    ENDELSE
  ;; ENDIF

  offset = [toffset,statoffset] & count = [tcount,statcount]
  offsetcat = [toffset,tracoffset,statoffset]
  countcat = [tcount,traccount,statcount]

  varid = ncdf_varid(ncid,'source_cat')
  ncdf_varget,ncid,varid,cats
  cats = string(cats)

  varid = ncdf_varid(ncid,'station')
  ncdf_varget,ncid,varid,stats
  stats = string(stats)

  varid = ncdf_varid(ncid,'num_data')
  ncdf_varget,ncid,varid,ndata,offset=offset,count=count

  varid = ncdf_varid(ncid,'CH4_obs')
  ncdf_varget,ncid,varid,ch4obs,offset=offset,count=count

  varid = ncdf_varid(ncid,'CH4_apri')
  ncdf_varget,ncid,varid,ch4apri,offset=offset,count=count

  varid = ncdf_varid(ncid,'CH4_apri_tracer')
  ncdf_varget,ncid,varid,catapri,offset=offsetcat,count=countcat

  IF NOT keyword_set(prior) THEN BEGIN
     varid = ncdf_varid(ncid,'CH4_apost')
     ncdf_varget,ncid,varid,ch4post,offset=offset,count=count
     
     varid = ncdf_varid(ncid,'CH4_apost_tracer')
     ncdf_varget,ncid,varid,catpost,offset=offsetcat,count=countcat
  ENDIF

  ncdf_close,ncid

  ok = 1
END


;------------------------------------------------------------------------------
;                     sort emission categories
;------------------------------------------------------------------------------
PRO sort_emiss_categories,ocats=ocats,oind=oind,ocol=ocol
 
  cats = emiss_categories()
  ncat = n_elements(cats)

  ;; first anthropogenic emissions (11 tracers)
  ocats = StrArr(ncat) & oind = LonArr(ncat)
  i=0
  ocats[i] = 'ANTH_RUS' & i++
  ocats[i] = 'ANTH_EU' & i++
  ocats[i] = 'ANTH_NA' & i++
  ocats[i] = 'ANTH_CHIN' & i++
  ocats[i] = 'ANTH_SE_ASIA' & i++
  ocats[i] = 'ANTH_IND' & i++
  ocats[i] = 'ANTH_MIDEAST' & i++
  ocats[i] = 'ANTH_SA' & i++
  ocats[i] = 'ANTH_AFR' & i++
  ocats[i] = 'ANTH_AUS' & i++
  ocats[i] = 'ANTH_OCE' & i++

  ;; second wetland emissions (13 tracers)
  ocats[i] = 'WETL_RUS' & i++
  ocats[i] = 'WETL_EU' & i++
  ocats[i] = 'WETL_NAbor' & i++
  ocats[i] = 'WETL_NAtemp' & i++
  ocats[i] = 'WETL_CHIN' & i++
  ocats[i] = 'WETL_SE_ASIA' & i++
  ocats[i] = 'WETL_IND' & i++
  ocats[i] = 'WETL_MID' & i++
  ocats[i] = 'WETL_SAtrop' & i++
  ocats[i] = 'WETL_SAtemp' & i++
  ocats[i] = 'WETL_NAFR' & i++
  ocats[i] = 'WETL_SAFR' & i++
  ocats[i] = 'WETL_AUS' & i++
  
  ;; third biomass burning (13 tracers)
  ocats[i] = 'BB_RUS' & i++
  ocats[i] = 'BB_EU' & i++
  ocats[i] = 'BB_NAbor' & i++
  ocats[i] = 'BB_NAtemp' & i++
  ocats[i] = 'BB_CHIN' & i++
  ocats[i] = 'BB_SE_ASIA' & i++
  ocats[i] = 'BB_IND' & i++
  ocats[i] = 'BB_MID' & i++
  ocats[i] = 'BB_SAtrop' & i++
  ocats[i] = 'BB_SAtemp' & i++
  ocats[i] = 'BB_NAFR' & i++
  ocats[i] = 'BB_SAFR' & i++
  ocats[i] = 'BB_AUS' & i++

  ;; fourth rice (7 tracers)
  ocats[i] = 'RICE_EU' & i++
  ocats[i] = 'RICE_NA' & i++
  ocats[i] = 'RICE_CHIN' & i++
  ocats[i] = 'RICE_IND' & i++
  ocats[i] = 'RICE_ASIA_AUS' & i++
  ocats[i] = 'RICE_SA' & i++
  ocats[i] = 'RICE_AFR' & i++

  ocats[i] = 'WILD_anim' & i++
  ocats[i] = 'TERMITES' & i++
  ocats[i] = 'OCEAN' & i++
  ocats[i] = 'VOLC'

  FOR i=0,47 DO BEGIN
     index = WHERE(cats EQ ocats[i],cnt)
     oind[i]=index[0]
  ENDFOR

  ocol = bindgen(ncat)+1
  ocol[24:*]=ocol[24:*]+1B
  ocol[37:*]=ocol[37:*]+1B
  ocol[44:*]=ocol[44:*]+1B

END

;--------------------------------------------------------------------

FUNCTION read_model_data_mismatch_netcdf,sim,prelim=prelim,yyyymm=yyyymm,$
                                         stats=stats

  IF n_elements(sim) EQ 0 THEN RETURN,-1
 
  sstr =sim_filename_str(sim,prelim=prelim)
  ncfile = sim.outdir + 'inv_model_data_mismatch_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'
  
  IF file_test(ncfile) EQ 0 THEN BEGIN
     print,'model data mismatch file ',ncfile,' not found'
     RETURN,-1
  ENDIF

  ;print,'reading model-data mismatches from file ',ncfile
  ncid = NCDF_OPEN(ncfile)
  
  ;; get dimensions
  ncdf_diminq, ncid, 0, timedim, ntime
  ncdf_diminq, ncid, 1, tracdim, nst

  ;; read times
  varid = ncdf_varid(ncid,'year')
  ncdf_varget,ncid,varid,yyyy
  varid = ncdf_varid(ncid,'month')
  ncdf_varget,ncid,varid,mm

  allyyyymm = string(yyyy)+string(mm)
  
  toffset = 0 & tcount = ntime

  IF n_elements(yyyymm) NE 0 THEN BEGIN
     ;; read in only data for this month
     index = WHERE(allyyyymm EQ yyyymm,cnt)
     IF cnt EQ 0 THEN BEGIN
        print,'no data found for month ',yyyymm
        ncdf_close,ncid
        RETURN,-1
     ENDIF ELSE BEGIN
        toffset = index[0] & tcount = 1
        allyyyymm = yyyymm
     ENDELSE
  ENDIF

  offset = [toffset,0L] & count = [tcount,nst]

  varid = ncdf_varid(ncid,'sigma_CH4')
  ncdf_varget,ncid,varid,sigma_CH4,offset=offset,count=count
  
  IF n_elements(stats) NE 0 THEN BEGIN
     ;; subset data for stations listed in stats
     varid = ncdf_varid(ncid,'station')
     ncdf_varget,ncid,varid,stations
     stations = STRING(stations)
     nstout=n_elements(stats)
     outind = LonArr(nstout)
     FOR k=0,nstout-1 DO BEGIN
        index = WHERE(stats[k] EQ stations,cnt)
        IF cnt NE 1 THEN stop
        outind[k]=index[0]
     ENDFOR
     sigma_ch4=sigma_ch4[*,outind]
  ENDIF

  ncdf_close,ncid

  RETURN,(reform(sigma_ch4))^2

END
