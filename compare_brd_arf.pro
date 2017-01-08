PRO compare_brd_arf,type = type,_extra=e

type = 'obs'
run = 'NEW_DLR'               ; or '22.4'
sim = inv_configurations_brd(run=run,ok=ok)

IF type EQ 'obs' THEN BEGIN
   ;; compare observation data processed by myself with those of Florian

   brdobsdir = sim.obsdir
   arfobsdir = '/nas/arf/INVERSION/OBSINPUT/FINAL/'

   stats = 'z_allweekly_91stats'
   weekly = 1

   ;; load all weekly observations
   brdfiles = file_search(brdobsdir,stats+'*.dat')
   arffiles = file_search(arfobsdir,stats+'*.dat')

   yyyymm = STRMID(file_basename(brdfiles),strlen(stats)+1,6)

   ;; loop over the years
   years = fix(STRMID(sim.syyyymm,0,4)) + indgen(fix(STRMID(sim.eyyyymm,0,4))-$
                                                 fix(STRMID(sim.syyyymm,0,4))+1)
   FOR y = 0,n_elements(years)-1 DO BEGIN
      yyyy = STRING(years[y],format='(i4)')
      index = WHERE(STRMID(yyyymm,0,4) EQ yyyy,cnt)
      IF cnt GT 0 THEN BEGIN
         
         ;; loop over files of this year and load data
         FOR i = 0,cnt-1 DO BEGIN
            
            read_data_single_final,sim=sim,yyyymm=yyyymm[index[i]],ch4obs=ch4,dtgobs=dtg,$
                                   lonobs=lon,latobs=lat,nameobs=name,nobs=nbrd,$
                                   weekly=weekly,basedir=brdobsdir
            
            IF i EQ 0 THEN BEGIN
               brdch4=ch4 & brddtg=dtg
               brdlon=lon & brdlat=lat
               brdname = name
            ENDIF ELSE BEGIN
               brdch4=[brdch4,ch4] & brddtg=[brddtg,dtg]
               brdlon=[brdlon,lon] & brdlat=[brdlat,lat]
               brdname = [brdname,name]
            ENDELSE
            
            read_data_single_final,sim=sim,yyyymm=yyyymm[index[i]],ch4obs=ch4,dtgobs=dtg,$
                                   lonobs=lon,latobs=lat,nameobs=name,nobs=narf,$
                                   weekly=weekly,basedir=arfobsdir
            IF i EQ 0 THEN BEGIN
               arfch4=ch4 & arfdtg=dtg
               arflon=lon & arflat=lat
               arfname = name
            ENDIF ELSE BEGIN
               arfch4=[arfch4,ch4] & arfdtg=[arfdtg,dtg]
               arflon=[arflon,lon] & arflat=[arflat,lat]
               arfname = [arfname,name]
            ENDELSE

         ENDFOR

         uname = name[uniq(name[sort(brdname)])]
            
         ;; plot station time series for this year     
         plot_tseries,dtg2gvtime(brddtg),brdch4,title=yyyy,_extra=e
         plot_tseries,dtg2gvtime(brddtg),brdch4,psym=plotsymbol(1),symsize=2,/over
         plot_tseries,dtg2gvtime(arfdtg),arfch4,psym=plotsymbol(1),color=24,/over

      ENDIF
      
   ENDFOR

   stop

ENDIF


END
