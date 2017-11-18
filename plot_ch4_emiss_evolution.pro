;+
; NAME:
;   plot_ch4_emiss_evolution
; 
; PURPOSE:
;   Plot evolution of emissions for main categories.
;   The script loops over all monthly emission files used as input for FLEXPART,
;   computes the totals per category, and plots the evolution as a stack plot
; 
; HISTORY:
;   Dominik Brunner, 4 Feb 2017
;-
  
@inv_tools_brd
PRO plot_ch4_emiss_evolution,plotmap=plotmap,emiss=emiss,eps=eps

  IF keyword_set(plotmap) THEN load_ctb,'bgyr18.ctb'

  emisdir ='/nas/arf/copy_home/FLEXPART/FINAL/'
  psdir = '/home/brd134/projects/MAIOLICAII/'

  nyear = 23
  years = STRING(indgen(nyear)+1990,format='(i4)')
  nmonth = 12
  months = STRING(indgen(nmonth)+1,format='(i2.2)')
 
  mcat = emiss_categories(/main)
  nmcat = n_elements(mcat)
  cat = emiss_categories()
  ncat = n_elements(cat)

  IF n_elements(emiss) EQ 0 THEN BEGIN
     emiss = FltArr(nyear,nmonth,ncat)

     levels = [0.01,0.02,0.04,0.07,0.1,0.2,0.4,0.7,1,2,4,7,10,20,40,70,100,200]
     c_colors = bindgen(n_elements(levels))+1B
     
     FOR i = 0,nyear-1 DO BEGIN
        print,'processing year ',years[i]
        FOR j = 0,nmonth-1 DO BEGIN
           ;; read in emissions and compute total
           filename = emisdir + 'Em'+STRMID(years[i],2,2)+months[j]+'1500.nc'
           ncid = ncdf_open(filename)
           varid = ncdf_varid(ncid,'lon')
           ncdf_varget,ncid,varid,lon
           varid = ncdf_varid(ncid,'lat')
           ncdf_varget,ncid,varid,lat
           
           FOR k = 0,ncat-1 DO BEGIN
              id = STRING(k+2,format='(i3.3)')
              varid = ncdf_varid(ncid,'SPECIES_'+id)
              ncdf_varget,ncid,varid,val
              emiss[i,j,k] = total(val)
              IF keyword_set(plotmap) THEN BEGIN
                 title = years[i]+months[j]+' - '+cat[k]
                 map_set,position=[0.05,0.05,0.85,0.9]
                 xyouts,0.5,0.95,title,/normal,charsize=1.5,alignment=0.5
                 contour,val,lon,lat,/cell_fill,levels=levels,c_colors=c_colors,/over
                 map_continents
                 map_grid,/box_axes,londel=30,latdel=15
                 colorbar,levels,c_colors,/col,/right,/ive,charsize=1.5,format='(f6.2)'
                 wait,1
              ENDIF
           ENDFOR
           ncdf_close,ncid
        ENDFOR
     ENDFOR
  ENDIF

  ;; create stacked emissions plot for main categories
  dtg=strarr(nyear*nmonth)
  FOR i=0,nyear-1 DO FOR j=0,nmonth-1 DO dtg[i*nmonth+j]=years[i]+months[j]
  
  chs = 1.4

  kgsec2tgyr = 86400D * 365.25 / 1e9

  yrange = [0,720] & yticks=8 & ytickv = indgen(yticks)*100 & yminor=5
  xtickv = indgen(nyear)*12
  xtickname =STRMID(dtg[xtickv[0:nyear-1]],0,4)
  xvals = indgen(n_elements(dtg))
  position = [0.08,0.31,0.98,0.98]

  IF keyword_set(eps) THEN BEGIN
     open_ps,psdir+'fig1_emissions_evol.eps',/color,/eps,pssize=[24,18],tt_type='Helvetica'
  ENDIF ELSE BEGIN
     window,xsize=800,ysize=800
  ENDELSE

  plot,xvals,fltarr(n_elements(dtg))-1,/nodata,position=position,yticks=yticks,yminor=yminor,$
       yrange=yrange,ytickv=ytickv,xrange=[0,275],xtickv=xtickv,xtickname=xtickname,$
       xticks=nyear-1,xstyle=4,ystyle=5,ytitle = 'CH!D4!N emission [Tg/yr]',charsize=chs
  
  lastecat = FltArr(n_elements(dtg))
  IF keyword_set(maincat) THEN BEGIN
     FOR k=0,nmcat-1 DO BEGIN
        ecat = FltArr(n_elements(dtg))
        icat = WHERE(strpos(cat,mcat[k]) NE -1,cnt)
        FOR i=0,nyear-1 DO BEGIN
           FOR j=0,nmonth-1 DO BEGIN
              ecat[i*nmonth+j]=total(emiss[i,j,icat]) * kgsec2tgyr
           ENDFOR
        ENDFOR
        
        polyfill,[xvals,reverse(xvals)],[lastecat,reverse(lastecat+ecat)],$
                 color=k*2+2
        lastecat = lastecat + ecat
     ENDFOR

     plot,xvals,fltarr(n_elements(dtg))-1,/nodata,position=position,$
          yrange=yrange,xrange=[0,275],xtickv=xtickv,xtickname=xtickname,$
          xticks=nyear-1,/noerase,ystyle=1,ytitle = 'CH!D4!N emission [Tg/yr]',charsize=chs
  ENDIF ELSE BEGIN

     colors = emiss_colors()
     
     FOR k=0,ncat-1 DO BEGIN
        ecat = FltArr(n_elements(dtg))
        FOR i=0,nyear-1 DO BEGIN
           FOR j=0,nmonth-1 DO BEGIN
              ecat[i*nmonth+j]=emiss[i,j,k] * kgsec2tgyr
           ENDFOR
        ENDFOR

        polyfill,[xvals,reverse(xvals)],[lastecat,reverse(lastecat+ecat)],$
                 color=colors[k],noclip=0
        lastecat = lastecat + ecat
     ENDFOR

     plot,xvals,fltarr(n_elements(dtg))-1,/nodata,position=position,xthick=2,ythick=2,$
          ytickv=ytickv,yrange=yrange,xrange=[0,275],xtickv=xtickv,xtickname=xtickname,$
          yminor=yminor,yticks=yticks,$
          xticks=nyear-1,/noerase,ystyle=1,ytitle = 'CH!D4!N emission [Tg/yr]',charsize=chs

     plot_category_legend,position=[0.,0.1,0.25,0.9]
     
  ENDELSE

  IF keyword_set(eps) THEN close_ps

END
