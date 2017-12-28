PRO plot_kreisdiagram_final,sim

  rads   = !PI/180.D

  TWO_PI = 2.D * !PI

  testfile = sim.outdir+'inv_output_weekly_flask_'+sim.sn+'_'+sim.name+'_'+$
             sim.syyyymm+'-'+sim.eyyyymm+'_'+sim.qunc+'.txt' 
 
  syyyy    = 1990 & smm = 1
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  m        = eyyyy*12L+emm-(1989*12L+2)+1  
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
  n        = 96 ;for 2001-2008
 
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

;    sa[*,it] = sahelp[11:m-1,it]
;    sp[*,it] = sphelp[11:m-1,it]

;for 2001:2008
     sa[*,it] = sahelp[143:m-49,it]
     sp[*,it] = sphelp[143:m-49,it]

  ENDFOR  
  FOR it=0,sim.ntrace-1 DO samean[it] = mean(sa[*,it])
  FOR it=0,sim.ntrace-1 DO spmean[it] = mean(sp[*,it]) 
  
  nyears = 23
  nyears = 8 ;for 2001-2008
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
  
  samean2=DblArr(11)
  spmean2=DblArr(11)
  
  samean2[0]=total(samean[0:10])
  samean2[1]=samean[12]+samean[13]+samean[15]+samean[16]+samean[17]+samean[19]+samean[20]
  samean2[2]=samean[11]+samean[14]+samean[18]+samean[21]+samean[22]+samean[23]
  samean2[3]=total(samean[24:30])
  samean2[4]=total(samean[31:43])-samean[29]-samean[32]
  samean2[5]=samean[29]
  samean2[6]=samean[32]
  samean2[7]=samean[44]
  samean2[8]=samean[45]
  samean2[9]=samean[46]
  samean2[10]=samean[47]
  
  
  spmean2[0]=total(spmean[0:10])
  spmean2[1]=spmean[12]+spmean[13]+spmean[15]+spmean[16]+spmean[17]+spmean[19]+spmean[20]
  spmean2[2]=spmean[11]+spmean[14]+spmean[18]+spmean[21]+spmean[22]+spmean[23]
  spmean2[3]=total(spmean[24:30])
  spmean2[4]=total(spmean[31:43])-spmean[29]-spmean[32]
  spmean2[5]=spmean[29]
  spmean2[6]=spmean[32]
  spmean2[7]=spmean[44]
  spmean2[8]=spmean[45]
  spmean2[9]=spmean[46]
  spmean2[10]=spmean[47]
  
  
  rela2=samean2/suma
  relp2=spmean2/sump
    
  ;sim.ntrace=11

  plotdir = sim.basedir + 'FIGURES/'
  plotfile = plotdir+'total_emission_kreis.eps'
  load_ctb,'/home/spc134/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[24,20],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1

  values = spmean2
  col    = [4,5,6,132,19,20,9,11,14,15,207] 
  pie_chart,values,colors=col,/outline
  
  dx = !d.x_size
  dy = !d.y_size
  
  cx = dx/2 & cy = dy/2
  
  radiusa = 2*min([dx,dy])/8.
  values = samean2
  pie_chart,values,colors=col,radius=radiusa,/outline,/overplot  
  
  radiusp = 1*min([dx,dy])/8.  
  pie_chart,[100],colors=0,radius=radiusp,/overplot,/outline
  
  angle = 0. ; 90 degrees
  oa    = angle
    
  textwidth = !d.x_ch_size * (6+StrLen(StrTrim(suma, 1)))
  
  xa    = [0.62,0.48,0.46,0.42,0.38,0.41,0.42,0.47,0.48,0.49,0.49]
  ya    = [0.45,0.34,0.36,0.34,0.45,0.63,0.66,0.67,0.68,0.69,0.69]

  xchar    = STRCOMPRESS(string(round(rela2*100.)),/REM)+' %'
  ;;xchar[4] = STRCOMPRESS(string(rela2[4]*100.,format='(f3.1)'),/REM)+' %'
  FOR it=0,sim.ntrace-1 DO xyouts,xa[it],ya[it],xchar[it],charthick=1.,charsize=1.4,/normal
 
  xp    = [0.71,0.50,0.44,0.37,0.35,0.45,0.47,0.49,0.50,0.51,0.51]
  yp    = [0.45,0.25,0.26,0.28,0.45,0.66,0.79,0.81,0.82,0.83,0.83]

  xchar    = STRCOMPRESS(string(round(relp2*100.)),/REM)+' %'
  ;;xchar[2] = STRCOMPRESS(string(rela[2]*100.,format='(f3.1)'),/REM)+' %'
  FOR it=0,sim.ntrace-1 DO xyouts,xp[it],yp[it],xchar[it],charthick=1.,charsize=1.4,/normal 
  
  close_ps

END
