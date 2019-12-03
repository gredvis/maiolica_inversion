;+
; NAME:
;
;   inv_configurations
;
; PURPOSE:
;
;   Define inversion simulation name, directories, and uncertainty scaling factors for all
;   48 emission categories.
;
;   The routine is called by run_inversion_final.pro
;
; CATEGORY:
;
;    MAIOLICAII, methane inversion, Florian Arfeuille stuff
;
; CALLING SEQUENCE:
;
;    sim = inv_configurations(run=run,sconfig=sconfig,dlr=dlr,ok=ok)
;
; INPUTS:
;
;   run     : the inversion configuration name
;             The latest configuations used by Florian were 'NEW_DLR' and '22.4'
;   sconfig : The station configuration, options are 'flask', 'all', 'special', 'flask_DLR2'
;   /dlr    : Set this keyword to set paths for DLR data processing and to
;             activate the dlr flag in the configuration structure.
;
; OUTPUTS:
;
;   ok : =1 if configuration for given run was found, =0 if not
;   RETURN VALUE:   Simulation structure
;  sim = {name:name,$                       ; simulation name, e.g. 'final_sim01'
;         sconfig:sconfig,$                 ; station configuration, e.g. 'flask'
;         sn,$                              ; string 'NNstats' with NN number of stations 
;                                           ; used for file names
;         qunc:qunc,$                       ; string 'optUU.U' with total a priori uncertainty
;                                           ; used for file names 
;         dlr:dlr,$                         ; =0 for FLEXPART, =1 for DLR output processing
;         obsmoddir:obsmoddir,$             ; directory of pre-processed obs and model data
;         modeldir:modeldir,$               ; base directory of FLEXPART or EMAC model output
;                                           ; model output will be in modeldir/name
;         outdir:outdir,$                   ; directory of inversion output
;         hdir:hdir,$                       ; directory of station sensitivities
;         syyyymm:syyyymm,eyyyymm:eyyyymm,$ ; start and end month of inversion
;         scaleq:scaleq,$                   ; uncertainty scaling factors per category
;         ntrace:ntrace,nage:nage,$         ; number of tracer (48) and age classes (5)
;         stats:stats,ufact:ufact,$         ; list of stations and uncertainy scaling factors
;         stat_levs:levs,$                  ; altitude levels at which FLEXPART is evaluated
;         flask:flask,$                     ; =1 if only flask sites are included
;         filter:filter,$                   ; =1 to enforce filtering for non-background values
;         statfilt:statfilt,$               ; list of sites to be filtered if /filter is set
;         weekly:weekly,$                   ; =1 to aggregate to weekly observation
;         keeppos:keeppos,$                 ; =1 to use log of emissions for positive solutions
;         startcf:startcf}                  ; list of initial scaling factors
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 04 Jan 2017:  first implementation
;-
@inv_tools
FUNCTION inv_configurations,run=run,sconfig=sconfig,dlr=dlr,ok=ok

  ;***********************************************
  ; define inversion parameters
  ;***********************************************

  ok = 0

  basedir = '/scratch/snx3000/dbrunner/maiolica/'
  
  name = 'final_sim01'                    ; name of inversion run
  syyyymm = '198902' & eyyyymm = '201212' ; start and end date of inversion
  
  IF keyword_set(DLR) THEN $
     invdir = basedir+'INVERSION_DLR/' $  ; base directory for intermediate and final inversion results
  ELSE $
     invdir = basedir+'INVERSION/'        ; base directory for intermediate and final inversion results
  
  obsmoddir = invdir+'OBSMODINPUT/'       ; directory of pre-processed obs and model data (netcdf)
  errcovdir = invdir+'ERRORCOVARIANCE/'   ; directory of station errors (diff model - station)
  outdir = invdir+'RESULTS/'              ; output directory for inversion results
  inputdir = basedir+'/input/'
  IF keyword_set(dlr) THEN BEGIN
     modeldir = inputdir+'EMAC_OUTPUT/'   ; base directory of EMAC model output
  ENDIF ELSE BEGIN
     modeldir = inputdir+'FLEXPART_OUTPUT/' ; base directory of FLEXPART model output
  ENDELSE

  hdir = invdir+'SENSITIVITIES/'          ; directory to store weekly sensitivities per station
  wdcggdir = inputdir+'GAW_WDCGG2017/'

  ntrace = 48                             ; number of tracers
  nage = 5                                ; number of age classes
  weekly  = 1                             ; use weekly means of observational and model data
  keeppos = 1                             ; take the logarithm of the emissions  

  dlrscale = 1.03               ; default scaling factor for DLR output to compensate low
                                ; bias with prescribed OH field
  
  ;; station configuration
  IF n_elements(sconfig) EQ 0 THEN sconfig = 'flask' ; options are 'flask', 'all', 'special', 'brd', 'flask_DLR2'

  ;; flags for certain station configurations
  flask   = strpos(sconfig,'flask') NE -1
  ;; flag for additional filtering for non-background values
  filter = strpos(sconfig,'special') NE -1 OR strpos(sconfig,'brd') NE -1
  
  ; non-background conditions only where available
  statfilt = ['brw']
   
  CASE run OF
     'NEW_DLR': BEGIN ; probably the final setting used!
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori
        ;; uncert from literature, taking into account diff regions and also fact that we 
        ;; optimize each month 4 times
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.40,0.40,0.40,0.40,0.40,$
                0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.30,0.30,$
                0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4]
     END
     'NEW_DLR_large': BEGIN
        ;; keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori
        ;; uncert from literature, taking into account diff regions and also fact that we 
        ;; optimize each month 4 times 22.4000
        keeppos = 0
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.4,0.4,0.4,0.4]
     END
     '22.4': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times 22.4
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.4,0.4,0.4,0.4]
     END
     '23.7': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times 23.7
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,$
                0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4]
     END
     '23.7+0.42': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times 23.7 + 0.42
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,$
                0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.42,0.42,0.42,0.42]
     END
     '26.1': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times 26.1000
        scaleq=[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.40,0.40,$
                0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.4,0.4,0.4,0.4]
     END
     '25.1': BEGIN
        ;; Test of Dominik, xhishquare about 1.4 when using ufact[*] = 0.6
        scaleq=[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.60,0.60,0.60,0.60,0.60,$
                0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,$
                0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.60,0.50,0.50,0.50,0.50]
     END
     '32.8': BEGIN
        ;; Xhisquare about 1.0 when using ufact[*] = 0.68
        scaleq=[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50]
     END
     '65.6': BEGIN
        ;; Xhisquare 0.94 when using ufact[*] = 0.68, high log-likelihood. However,
        ;; Extended Kalman smoother appears to become unstable at such high prior uncertainties,
        ;; correlations of posterior time series mostly lower than those of prior
        scaleq=[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50]*2
     END
     'all_flask_large_uncert': BEGIN
        ;;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert large
        scaleq=[1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.10,1.10,1.10,1.10,1.10,1.10,1.10,$
                1.10,1.10,1.10,1.10,1.10,1.10,1.2,1.2,1.2,1.2,1.2,1.2,1.2,2.0,2.0,2.0,2.0,2.0,$
                2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.50,2.50,2.50,2.50]
     END
     'all_flask_lit_prior_a': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times
        scaleq=[0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.10,0.10,0.10,0.10,0.10,$
                0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.25,$
                0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.30,0.30,0.30,0.30]
     END
     'all_flask_lit_prior_b': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times
        scaleq=[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.40,0.40,0.40,0.40,0.40,$
                0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.40,0.40,0.40,0.40]
     END
     'all_flask_lit_prior_c': BEGIN
        ;; keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 
        ;; apriori uncert from litterature, taking into account diff regions and also 
        ;; fact that we optimize each month 4 times
        scaleq=[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.80,0.80,0.80,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.80,0.80,$
                0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50]
     END
     'all_flask_inv_run_sp23a': BEGIN ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23
        scaleq=[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.70,0.70,0.70,0.70]
     END
     'all_flask_inv_run_sp23b': BEGIN ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23
        scaleq=[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.10,0.40,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.20,0.20,0.20,0.20]
     END
     'all_flask_inv_run_sp23c': BEGIN ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23
        scaleq=[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,$
                0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,$
                0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10]
     END
     'all_flask': BEGIN         ;keeppos1 change fcorr all flask
        scaleq=[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50]
     END
     'all_flask_not1a': BEGIN   ;keeppos1 change fcorr all flask initialised not to 1
        scaleq=[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50]
     END
     'all_flask_not1b': BEGIN   ;keeppos1 change fcorr all flask initialised not to 1
        scaleq=[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50]
     END
     'all_flask_not1c': BEGIN   ;keeppos1 change fcorr all flask initialised not to 
        scaleq=[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.50,$
                1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,0.50,0.50,0.50,0.50]
     END
     'all_flask_not1d': BEGIN   ;keeppos1 change fcorr all flask initialised not to 1
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.30,0.30,0.30,0.30,0.30,$
                0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.50,$
                0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.20,0.20,0.20,0.20]
     END
     'all_flask_not1e': BEGIN   ;keeppos1 change fcorr all flask initialised not to 1
        scaleq=[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,$
                0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.50,$
                1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,0.50,0.50,0.50,0.50]
     END
     'all_flask_not1f': BEGIN   ;keeppos1 change fcorr all flask initialised to 1
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20]
     END
     'all_flask_not1g': BEGIN   ;keeppos1 change fcorr all flask NOT initialised to 1
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20]
     END
     'all_flask_not1h': BEGIN   ;keeppos1 change fcorr all flask NOT initialised to 1
        scaleq=[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,$
                0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.05,0.20,0.20,0.20,0.20,0.20,0.20,0.20]
     END
     'antarctica_scaled': BEGIN ;keeppos1 change fcorr all , antarctica scaled
        scaleq=[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,$
                1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,$
                1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0.9,0.9,0.9,0.9]
     END
     'tropics_scaled': BEGIN    ;keeppos1 change fcorr all , tropics scaled
        syyyymm='198902' & eyyyymm='199312'
        scaleq=[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,$
                1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,$
                1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0.9,0.9,0.9,0.9]
     END
     'nocomm1': scaleq=[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,$
                        1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,$
                        0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,$
                        1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45]
     'nocomm2': scaleq=[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,$
                        1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,$
                        1.55,1.55,1.55,1.55,1.55,1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,$
                        1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.0,1.0,1.0,1.0]
     'nocomm3': BEGIN           ;keeppos0 change fcorr all
        keeppos=0
        scaleq=[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,$
                1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,$
                1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.8,1.8,1.8,1.8]
     END
     'nocomm4': BEGIN           ;keeppos1 change fcorr all
        scaleq=[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.65,0.65,0.65,$
                0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.55,0.55,0.55,0.55,$
                0.55,0.55,0.55,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.4,$
                0.4,0.4,0.4]
     END
     'nocomm5': BEGIN           ;keeppos1 change fcorr all
        syyyymm='198902' & eyyyymm='199312'
        scaleq=[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,$
                0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,$
                0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27]
     END
     ELSE: BEGIN
        print,'invalid run'
        RETURN,-1
     END
  ENDCASE
  ok = 1

  ;; get list of stations and uncertainty scaling factors
  inv_station_settings,sconfig,stats=stats,ufact=ufact,ok=ok
  IF NOT ok THEN RETURN,-1

  ;; amplify all uncertainties by constant factor 1.75
  ufact = ufact * 1.75

  ;IF run EQ '32.8' OR run EQ '22.4' THEN ufact = ufact * 1.5
  ;IF run EQ '25.1' THEN ufact = ufact * 0.6
  ;IF (run EQ '32.8' OR run EQ '65.6') AND NOT keyword_set(dlr) THEN ufact = ufact * 0.68

  ;; get optimal FLEXPART receptor output levels for these sites
  station_rcpt_levs,inputdir,stats,levs=levs

  sn = STRCOMPRESS(n_elements(stats),/REM)              
  print, 'Run inversion with ', sn, ' stations'

  ;; initial scaling factors

  ;;startcf = FltArr(48) + 1.  ; initial test
  ;;startcf[*] = 1/0.98871541  ; first global scaling
  ;;startcf[*] =1.0275         ; 2nd global scaling
  
  ;; refined scaling per category
  ;;startcf = [1.00000,1.00000,1.00000,1.00000,1.04629,1.04930,1.00000,1.00000,$
  ;;          1.00000,1.00000,1.17080,1.00000,1.02708,1.00000,1.05412,1.00132,$
  ;;          1.04544,1.21240,1.06390,1.00000,1.24261,0.955105,1.11636,1.00000,$
  ;;          1.09041,1.00000,1.00000,1.03611,0.939701,1.09191,1.00000,0.909144,$
  ;;          1.00000,1.00000,1.39767,1.05893,1.03421,1.00000,0.907644,1.09670,$
  ;;          1.00000,1.00000,1.00000,1.00000,1.08723,1.00000,1.00000,1.00000]

  ;; scaling factors at the end of 1991
  ;; 0.991477 0.998450 0.972916 0.971567 0.974735 0.989856 0.976809 0.994048 0.995995  1.05789
  ;; 0.986327  1.02056 0.991524 0.993904 0.996534  1.00001  1.00663 0.993243 0.978914 0.998782
  ;; 1.20031  1.09269 0.984832 0.972935 0.997707 0.966160 0.977547 0.999849 0.961963 0.997453
  ;; 0.991012 0.980430 0.968409 0.987643 0.962847 0.996981 0.938558 0.935513 0.968860  1.03806
  ;; 1.77183 0.917666 0.936982 0.975773 0.998232 0.982960  1.07199 0.914840
  
  ;; finally used (corresponds to scaling factors at the end of 1992
  ;;     startcf = [0.990934,0.998725,0.966918,0.970774,0.969145,0.988575,0.967936,0.989237,$
  ;;                0.999264,1.08774,0.980201,1.02635,0.990535,1.00026, 0.995482,1.00000,$
  ;;                1.00258,0.988409,0.964153,0.999978,1.26844,1.12428,0.915826,0.972333,$
  ;;                0.997255,0.954269,0.970943,0.999860,0.954971,0.996703,0.987350,0.964997,$
  ;;                0.951639,0.989671,0.958195,0.997164,0.905059,0.898224,0.961744,1.03772,$
  ;;                2.27135,0.882971,0.910372,0.969549,1.00155,0.983641,1.10581,0.885359]
  
  ;; same as above but scaling for south american temperate wetlands set to 1.0 instead of 2.27
  startcf = [0.990934,0.998725,0.966918,0.970774,0.969145,0.988575,0.967936,0.989237,$
             0.999264,1.08774,0.980201,1.02635,0.990535,1.00026, 0.995482,0.6,$
             1.00258,0.988409,0.964153,0.999978,1.26844,1.12428,0.915826,0.972333,$
             0.997255,0.954269,0.970943,0.999860,0.954971,0.996703,0.987350,0.964997,$
             0.951639,0.989671,0.958195,0.997164,0.905059,0.898224,0.961744,1.03772,$
             1.0,0.882971,0.910372,0.969549,1.00155,0.983641,1.10581,0.885359]

  sn   = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'
  qunc = 'opt'+STRCOMPRESS(string(total(scaleq)),/REM)

  sim = {name:name,$            ; simulation name, e.g. '25.1'
         sconfig:sconfig,$      ; station configuration, e.g. 'flask', 'all'
         sn:sn,$                ; string 'NNstats' with NN number of stations used for file names
         qunc:qunc,$            ; string 'optUU.U' with total a priori uncertainty used for file names 
         dlr:keyword_set(dlr),$ ; flag for DLR output
         dlrscale:dlrscale,$    ; scaling factor to correct low bias in DLR data, default is 1.04
         invdir:invdir,$         ; base directory of inversion output
         obsmoddir:obsmoddir,$  ; directory of pre-processed weekly obs and model data in netcdf format
         errcovdir:errcovdir,$  ; directory with diagnonal elements of model-data mismatch uncert
         inputdir:inputdir,$    ; base directory of input files
         outdir:outdir,$        ; results directory
         modeldir:modeldir,$
         wdcggdir:wdcggdir,$
         hdir:hdir,$
         syyyymm:syyyymm,eyyyymm:eyyyymm,$
         scaleq:scaleq,$
         ntrace:ntrace,nage:nage,$
         weekly:weekly,$
         keeppos:keeppos,$
         ;; station names, uncertainty scaling factors, and altitude levels
         stats:stats,$
         ufact:ufact,$
         stat_levs:levs,$
         flask:flask,$
         filter:filter,$
         statfilt:statfilt,$
         startcf:startcf}

  RETURN,sim

END
