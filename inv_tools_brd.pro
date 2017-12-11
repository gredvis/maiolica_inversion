;; library of support routines for the MAIOLICA-II inverse modelling

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

FUNCTION emiss_colors

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

RETURN,col

END

;-------------------------------------------------------------------------------

PRO station_rcpt_levs,stats,levs=levs
  ;; optimal FLEXPART receptor levels
  rec = {name:'',lev:0}
  allstat = replicate(rec,91)
  file='/home/brd134/IDL/arfeuille_final/choice_modellevel_station_91.dat'
  openr,lun,file,/get_lun
  readf,lun,allstat,format='(a3,2x,i4)'
  free_lun,lun

  nstat = n_elements(stats)
  levs = StrArr(nstat)+'-9999'
  FOR i=0,nstat-1 DO BEGIN
     index = WHERE(allstat.name EQ stats[i],cnt)
     IF cnt EQ 1 THEN levs[i]=strcompress(allstat[index[0]].lev,/rem)
  ENDFOR
  
END

;-------------------------------------------------------------------------------------------------------

PRO plot_category_legend,position=position

  IF n_elements(position) NE 4 THEN position=[0,0.1,0.3,0.9]
  cat = emiss_categories()
  col = emiss_colors()

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
     'ALT': dlrid='ALE' 
     'MHD': dlrid='MAC'         ; Mace Head
     'MLO': dlrid='MAU'         ; Mauna Loa
     'RPB': dlrid='RAG'
     'SMO': dlrid='?missing?'
     'THD': dlrid='TRI'
     'WSA': dlrid='SAB'         ; Sable Island
     'CGO': dlrid='CPG'         ; Cape Grim
     'IZO': dlrid='IZA'
     'PAL': dlrid='PSA'
     'ICE': dlrid='?missing?'
     'SIS': dlrid='?missing?'
     'CBA': dlrid='COL'
     'SHM': dlrid='SHI'         ; Shemya Islan; Florian used SHE (Shetland?)
     'OXK': dlrid='OCH'
     'LPO': dlrid='ILE'
     'ESP': dlrid='EST'
     'HPB': dlrid='HOH'
     'HUN': dlrid='HEG'
     'LEF': dlrid='PAR'
     'AMT': dlrid='?missing?'
     'BSC': dlrid='BLA'
     'KZD': dlrid='SAR'
     'UUM': dlrid='ULA'
     'PDM': dlrid='PIC'
     'BGU': dlrid='BEG'
     'NWR': dlrid='NIW'
     'UTA': dlrid='WEN'
     'AZR': dlrid='TCI'
     'PTA': dlrid='POI'
     'TAP': dlrid='TAE'
     'WLG': dlrid='MTW'
     'BMW': dlrid='TUD'
     'BME': dlrid='STD'
     'WKT': dlrid='MOO'
     'WIS': dlrid='SED'
     'ASK': dlrid='ASS'
     'LLN': dlrid='LUL'
     'KUM': dlrid='CPK'
     'CRI': dlrid='CPR'
     'GMI': dlrid='GUA'
     'ABP': dlrid='ARE'
     'MKN': dlrid='MTK'
     'SEY': dlrid='MAH'
     'CFA': dlrid='CPF'
     'NMB': dlrid='GLO'
     'EIC': dlrid='EAS'
     'AMS': dlrid='AMS'
     'MAA': dlrid='MAW'
     'ARH': dlrid='ARR'
     'BHD': dlrid='BAR'
     'CRZ': dlrid='CRO'
     'MQA': dlrid='MQI'         ; Macquarie Island; Florian used MAC
     'TDF': dlrid='TIE'
     'PSA': dlrid='PST'
     'CYA': dlrid='CAS'
     'HBA': dlrid='HAL'
     'LMP': dlrid='LAM'
     'BEGUR': dlrid='BEG'
     'ILEGRANDE': dlrid='ILE'
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
  file ='/project/arf/nas/output/final_sim01/19900201/receptor_pptv.nc'
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
