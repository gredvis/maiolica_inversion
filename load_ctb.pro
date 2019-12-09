;+
; NAME:
;	load_ctb
;
; PURPOSE:
;	load a colortable (written with save_ctb) from the
;	directory idl/colortables
;	A colortable may be produced using xpalette and then
;	calling save_ctb,filename. This will store the table
;	in the directory $HOME/idl/colortables which must exist.
;	load_ctb,filename will then restore that colortable
;
; CATEGORY:
;	color_tables
;
; CALLING SEQUENCE:
;	load_ctb,c_table
; EXAMPLE:
;	load_ctb,'bsp.ctb'
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;	c_table: (string) the name of the own colortable file in the
;                directory ~idl/colortables/
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS: creates new colortables in the directory colortables/
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY: 
;   FeldWaldWiesen-Programmierung GmbH
;   D.Brunner 27.4.1996
;-
PRO load_ctb,c_table

ON_ERROR,2

;check, if postscript device is the actual device
IF (!d.flags AND 1) EQ 1 THEN return

IF n_elements(c_table) eq 0 THEN $
   c_table=dialog_pickfile(filter='*.ctb',/must_exist)
IF c_table EQ '' THEN return

; check if file exists
IF file_search(expand_path(c_table)) EQ '' THEN BEGIN
   IF float(!version.release) GT 6.9 THEN start_dir='IDL_START_DIR' ELSE $
   start_dir = 'IDL_WDE_START_DIR'
   CASE strlowcase(!VERSION.OS_FAMILY) OF
      'windows': default_dir=pref_get(start_dir)+'\maiolica_inversion\colortables\'
      'unix': default_dir=expand_path('~')+'/IDL/maiolica_inversion/colortables/'
      else: default_dir=''
   ENDCASE

   c_table=file_search(default_dir+c_table)
   c_table=c_table[0]
   IF c_table EQ '' THEN $
      ;; check other location
     c_table=dialog_pickfile(title='Select colortable file',filter='*.ctb',/must_exist)
   IF c_table EQ '' THEN return
ENDIF

;restore values for red,grn,blu colorvectors
print,'Restoring color table ',c_table
restore,c_table
tvlct,red,grn,blu
print,'successful!'

end
