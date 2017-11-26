;+
; NAME:
;
;   inv_modvector_mon_weekly_brd
;
; PURPOSE:
;
;   Read daily model data and compute weekly means. Write out in monthly files.
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;      inv_modvector_mon_weekly_brd,sim,plot=plot
;
;
; INPUTS:
;
;       sim   (structure) : the simulation structure
;
;
; KEYWORD PARAMETERS:
;
;       /plot             : set keyword to plot comparisons of model and observation
;                           time series
;
; OUTPUTS:
;
;        weekly model data for years of inversion as monthly files
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
;   read_weekly_model_data
;
;     Read daily model data per month
;     Output weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;    CSP 16 April 2012
;    Originally called read_weekly_model_data_all.pro
;    
;    Dominik Brunner, 5 Feb 2017:
;      Major rewrite and strong simplification of this routine and its subroutines
;      Renamed to inv_modvector_mon_weekly_brd.pro as its observation
;      counterpart inv_obsvector_mon_weekly_brd.pro
;-

;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_modvector_mon_weekly_brd,sim,plot=plot

  from = fix(STRMID(sim.syyyymm,0,4))
  to   = fix(STRMID(sim.eyyyymm,0,4))

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
  
  weekly   = 1
                                                       
  ; number of stations
  nst      = n_elements(sim.stats)
  sn       = STRCOMPRESS(nst,/REM)+'stats'  
  nmonths  = 12
  mon      = STRING(indgen(nmonths)+1,format='(i2.2)')
  nyears   = long(to-from+1)
  nall     = nyears*nmonths*31L
  syear    = STRCOMPRESS(string(from+indgen(nyears)),/REM)

  ;***************************************
  ;* MAIN LOOP: YEARS
  ;***************************************
  FOR ij=0,nyears-1 DO BEGIN

    print, 'process year ', syear[ij]
    
    ;***********************************
    ;* 1. read in observational data to
    ;  get times with valid observations
    ;***********************************
    read_processed_obs_data_year,sim,syear[ij],ch4obs=ch4obs

    ;************************************************
    ;* read in corresponding model receptor output
    ;************************************************
    read_orig_model_data_brd,sim,syear[ij],ch4obs=ch4obs,ch4mod=ch4mod,oldest=oldest
 
    IF keyword_set(plot) THEN BEGIN
       psdir = '/home/brd134/projects/MAIOLICAII/station_tseries_plots/'
       ;; compare measurement and observation time series
       nplot = 6                ; max number of plots on same page
       dplot = 0.93/nplot
       modcol=24

       ;; sort data by station name and latitude
       isort = sort(ch4obs.name)
       iuniq = uniq(ch4obs[isort].name)
       nmax = n_elements(iuniq)
       snames = ch4obs[isort[iuniq]].name
       slats = ch4obs[isort[iuniq]].lat
       isort2 = sort(slats)
       snames = snames[isort2]
       
       print,nmax,' stations available for year ',syear[ij]
       
       ;; sort stations by latitude
       isort2 = sort(ch4obs[isort].lat)

       xrange=[dtg2gvtime(syear[ij]+'0101'),dtg2gvtime(string(fix(syear[ij])+1,format='(i4)')+'0101')]

       FOR i=0,nmax-1 DO BEGIN
          sinfo = station_info(snames[i])
          pos = i MOD nplot
          IF pos EQ 0 THEN BEGIN
             open_ps,psdir+syear[ij]+'_'+strjoin(snames[i:i+(nplot-1)<(nmax-1)],'_')+'.eps',/eps,/color,$
                     pssize=[18,20],tt_type='Helvetica'
          ENDIF
          
          index = WHERE(ch4obs.name EQ snames[i])
          gvt = dtg2gvtime(ch4obs[index].dtg)

          position = [0.1,pos*dplot+0.05,0.95,(pos+1)*dplot]
          plot_tseries,gvt,ch4obs[index].ch4,yrange=sinfo.yrange,xrange=xrange,/xstyle,$
                       position=position,noerase=(pos GT 0),title=sinfo.name+$
                       ' ('+strupcase(sinfo.id)+', lon='+strcompress(sinfo.lon)+$
                       ', lat = '+strcompress(sinfo.lat)+', alt = '+$
                       strcompress(sinfo.alt)+')',ytitle='CH!D4!N [ppb]',xtitle=''
          plot_tseries,gvt,ch4obs[index].ch4,/over,psym=plotsymbol(1)
          plot_tseries,gvt,ch4mod[index].ch4,/over,color=modcol
          plot_tseries,gvt,ch4mod[index].ch4,/over,color=modcol,psym=plotsymbol(1)
          IF pos EQ (nplot-1) OR i EQ (nmax-1) THEN BEGIN
             XYOUTS,0.5,(pos+1)*dplot+0.03,syear[ij],charsize=1.8,alignment=0.5,/normal
             close_ps
          ENDIF
       ENDFOR
    ENDIF


    ;;***************************************
    ;;* write out model data for every month 
    ;;***************************************
    ipos = 0L ; data pointer
    
    FOR im=0,nmonths-1 DO BEGIN

       IF keyword_set(sim.flask) THEN BEGIN  
          monfile = sim.moddir+'m_allweekly_flask_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'        
          IF keyword_set(sim.nobg) THEN $
             monfile  = sim.moddir+'m_allweekly_flask_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'          
       ENDIF ELSE BEGIN
          monfile = sim.moddir+'m_allweekly_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'       
          IF keyword_set(sim.nobg) THEN $
             monfile = sim.moddir+'m_allweekly_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'
          IF keyword_set(sim.special) THEN $
             monfile = sim.moddir+'m_allweekly_special_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'           
       ENDELSE
       openw,lun,monfile,/get_lun
       WHILE STRMID(ch4mod[ipos].dtg,4,2) EQ mon[im] DO BEGIN
          printf,lun,ch4mod[ipos].dtg,ch4mod[ipos].name,$
                 ch4obs[ipos].lat,ch4obs[ipos].lon,ch4mod[ipos].ch4,$
                 format='(a12,1x,a3,1x,f8.3,1x,f8.3,1x,f9.4)'
          printf,lun,ch4mod[ipos].ch4trace
          ipos ++
          IF ipos EQ n_elements(ch4mod) THEN GOTO,nextmonth
       ENDWHILE

       nextmonth:
       free_lun,lun
    ENDFOR
 ENDFOR

END

