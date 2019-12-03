PRO plot_kreisdiagram_final,sim,cat=cat

  rads   = !PI/180.D

  TWO_PI = 2.D * !PI

  testfile = sim.outdir+'inv_output_weekly_flask_'+sim.sn+'_'+sim.name+'_'+$
             sim.syyyymm+'-'+sim.eyyyymm+'_'+sim.qunc+'.txt'  

  syyyy    = 1990 & smm = 1
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  m        = eyyyy*12L+emm-(1989*12L+2)+1  
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
 ; n        = 96 ;for 2001-2008
 
  fhelp    = DblArr(m,sim.ntrace)
  sahelp   = DblArr(m,sim.ntrace)
  sphelp   = DblArr(m,sim.ntrace)

  sa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)
    
  samean   = DblArr(sim.ntrace)
  spmean   = DblArr(sim.ntrace)

  openr,lun,testfile,/get_lun
  readf,lun,fhelp
  readf,lun,sahelp
  readf,lun,sphelp
  free_lun,lun
  
  ; only evaluate from 2001 on. 2000 is spin up year of inversion
  FOR it=0,sim.ntrace-1 DO BEGIN

    sa[*,it] = sahelp[11:m-1,it]
    sp[*,it] = sphelp[11:m-1,it]

  ENDFOR  
  FOR it=0,sim.ntrace-1 DO samean[it] = mean(sa[*,it])
  FOR it=0,sim.ntrace-1 DO spmean[it] = mean(sp[*,it]) 
  
  nyears = 23
  ;nyears = 8 ;for 2001-2008
  sayr = FltArr(nyears,sim.ntrace)
  spyr = FltArr(nyears,sim.ntrace)
  a = 0 & e = 0
  FOR ij=0,nyears-1 DO BEGIN
    e += 12
    FOR it=0,sim.ntrace-1 DO BEGIN
      sayr[ij,it] = mean(sa[a:e-1,it])
      spyr[ij,it] = mean(sp[a:e-1,it])
    ENDFOR
    a += 12
  ENDFOR

  FOR it=0,sim.ntrace-1 DO BEGIN
    print, it, ' ', mean(sayr[*,it])*1.e-9*365.
    print, it, ' ', mean(spyr[*,it])*1.e-9*365.
  ENDFOR

  suma = total(samean)
  sump = total(spmean)
  
  print, suma*1.e-9*365., ' ', sump*1.e-9*365.
  

  factora = rads * 360.D / suma
  factorp = rads * 360.D / sump

  rela = DblArr(sim.ntrace)
  relp = DblArr(sim.ntrace)
  rela = samean/suma
  relp = spmean/sump
  
  If cat eq 0 THEN BEGIN
     samean2=DblArr(11)
     spmean2=DblArr(11)
     samean2[*]=samean[0:10]
     spmean2[*]=spmean[0:10]
     sim.ntrace=11
     suma = total(samean2)
     sump = total(spmean2)
  ENDIF
  
  IF cat eq 1 THEN BEGIN
     samean2=DblArr(13)
     spmean2=DblArr(13)
     samean2[*]=samean[11:23]
     spmean2[*]=spmean[11:23]
     sim.ntrace=13
     suma = total(samean2)
     sump = total(spmean2)
  ENDIF
  
  IF cat eq 2 THEN BEGIN
     samean2=DblArr(7)
     spmean2=DblArr(7)
     samean2[*]=samean[24:30]
     spmean2[*]=spmean[24:30]
     suma = total(samean2)
     sump = total(spmean2)
     sim.ntrace=7
  ENDIF
  
  IF cat eq 3 THEN BEGIN
     samean2=DblArr(13)
     spmean2=DblArr(13)
     samean2[*]=samean[31:43]
     spmean2[*]=spmean[31:43]
     suma = total(samean2)
     sump = total(spmean2)
     sim.ntrace=13
  ENDIF
  
  IF cat eq 4 THEN BEGIN
     samean2=DblArr(4)
     spmean2=DblArr(4)
     samean2[*]=samean[44:47]
     spmean2[*]=spmean[44:47]
     suma = total(samean2)
     sump = total(spmean2)
     sim.ntrace=4
  ENDIF
  
  
  rela2=samean2/suma
  relp2=spmean2/sump
  
  plotdir = sim.invdir + 'FIGURES/'
  plotfile = plotdir+'total_emission_kreis'+cat+'.eps'
  load_ctb,'/home/spc134/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[24,20],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1

  values = spmean2
  col    = [4,5,6,132,19,20,9,11,14,15,207,12,13] 

  If cat eq 0 THEN col    = [cgcolor("OLIVE"),cgcolor("DARKGREEN"),cgcolor("PALEGREEN"),cgcolor("AQUAMARINE"),cgcolor("GREENYELLOW"),cgcolor("TEAL"),cgcolor("LIGHTSEAGREEN"),cgcolor("GREEN"),cgcolor("KHAKI"),cgcolor("SPRINGGREEN"),cgcolor("LIMEGREEN")]
  If cat eq 1 THEN col    = [cgcolor("RED"),cgcolor("ORANGERED"),cgcolor("CRIMSON"),cgcolor("FIREBRICK"),cgcolor("SALMON"),cgcolor("DARKRED"),cgcolor("TOMATO"),cgcolor("PINK"),cgcolor("ROSE"),cgcolor("VIOLETRED"),cgcolor("MAGENTA"),cgcolor("SIENNA"),cgcolor("ORANGE")]
  
  If cat eq 2 THEN col= [cgcolor("BEIGE"),cgcolor("SEASHELL"),cgcolor("LIGHTYELLOW"),cgcolor("PAPAYA"),cgcolor("WHEAT"),cgcolor("BURLYWOOD"),cgcolor("LIGHTGRAY")]
  
  If cat eq 3 THEN col= [cgcolor("BLUE"),cgcolor("ROYALBLUE"),cgcolor("NAVY"),cgcolor("STEELBLUE"),cgcolor("CADETBLUE"),cgcolor("CORNFLOWERBLUE"),cgcolor("SKYBLUE"),cgcolor("DARKSLATEBLUE"),cgcolor("PURPLE"),cgcolor("POWDERBLUE"),cgcolor("DODGERBLUE"),cgcolor("YGB3"),cgcolor("TURQUOISE")]
  
  If cat eq 4 THEN col= [cgcolor("BLACK"),cgcolor("YELLOW"),cgcolor("GOLD"),cgcolor("GOLDENROD")]
  
  pie_chart,values,colors=col,/outline
  
  dx = !d.x_size
  dy = !d.y_size
  
  cx = dx/2 & cy = dy/2
  
  radiusa = 2*min([dx,dy])/8.
  values = samean2
  pie_chart,values,colors=col,radius=radiusa,/outline,/overplot  
  
  radiusp = 1*min([dx,dy])/8.  
  pie_chart,[100],colors=0,radius=radiusp,/overplot,/outline
  
  angle = 0.                    ; 90 degrees
  oa    = angle
  
  textwidth = !d.x_ch_size * (6+StrLen(StrTrim(suma, 1)))
  
  xa    = [0.62,0.48,0.46,0.42,0.38,0.41,0.42,0.47,0.48,0.49,0.49]
  ya    = [0.45,0.34,0.36,0.34,0.45,0.63,0.66,0.67,0.68,0.69,0.69]
  
  xchar    = STRCOMPRESS(string(round(rela2*100.)),/REM)+' %'
  xchar[4] = STRCOMPRESS(string(rela2[4]*100.,format='(f3.1)'),/REM)+' %'
  FOR it=0,sim.ntrace-1 DO xyouts,xa[it],ya[it],xchar[it],charthick=1.,charsize=1.4,/normal
 
  xp    = [0.71,0.50,0.44,0.37,0.35,0.45,0.47,0.49,0.50,0.51,0.51]
  yp    = [0.45,0.25,0.26,0.28,0.45,0.66,0.79,0.81,0.82,0.83,0.83]

  xchar    = STRCOMPRESS(string(round(relp2*100.)),/REM)+' %'
  xchar[2] = STRCOMPRESS(string(rela[2]*100.,format='(f3.1)'),/REM)+' %'
  FOR it=0,sim.ntrace-1 DO xyouts,xp[it],yp[it],xchar[it],charthick=1.,charsize=1.4,/normal 
  
  close_ps

END
