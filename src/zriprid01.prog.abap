***INCLUDE RIPRID01 .
*$*$ GENERAL DATA SECTION for PRINT ABAPS used via FUNCTION:
*$*$ PM_NOTIFICATION_PRINT_CONTROL.

TABLES:
                                       "... work areas
        diadr,                         " Partner address
        bsvz,                          " Status system
        wiprt,                         " Special Variables in forms
        viqmel,                        " Notif Header (QMEL;QMIH;ILOA)
        viqmfe,                        " Notif position
        viqmma,                        " Notif activities
        viqmsm,                        " Notif procedures
        viqmur,                        " Notif damage codes
        wqmfe,                         " Notif position
        wqmur,                         " Notif damage codes
        wqmsm,                         " Notif procedures
        wqmma,                         " Notif activities
        qkat,                          " Quality codes catalogue WorkA
        ihpad,                         " Partner info
        qpct,                          " Catalogue code Texts
                                       "... Text  AND WORK TABLES
        crhd,                          " Workcenters
        crtx,                          " Workcenter texts
        caufvd,                        " Order header
        afvgd,                         " Order operations
        kbedp,                         " capacity record with Person
        objk,                          " Object list
        resbd,                         " Material reservations
        rcr01,                         " Workarea for WORKCENTERS
        ripw0,                         " Workarea for OBJECT LIST
        riwo1,                         " Workarea for ILOA details/text
        riwo00,                        " Workarea Notif header details
        rqm00,                         " Workarea QM-QN special data
        affhd,                         " Prod Res and tools dialog tab
        iloa,                          " Location - accounting info
        draw,                          " Document manag System
        drad,                          " DMS Allocations to objects
        diadrp,                        " partner info (addr. Text)
        pdwork,                        " HR operation details
        pdavoea,                       " quick operation work area order
        ihsg,                          " permissions
        ihgns,                         " granted permissions
        v_t357g,                       " text view over permissions
        t357g,                         "  permissions
        t357g_t,                       " text  permissions
        t352t_t,                       " text  permissions
        eqkt,                          " Equipment short text
        efhm,                          " Res tool for equipment
*       EQSE,   details in EQUI NOW    " Serial number
        equi,                          " Equipment self
        equz,                          " time segment
        makt,                          " Material short text
        iflo,                          " Functional location
        swor,                          " Catch words for classes
        klah,                          " Classification HEADER
        stxh,                          " Standard text header
        tco09,                         " LONG TEXT keys
        t003p,                         " Order type texts
        t352r,                         " Revisions
        t357z_t,                       " Machine status texts
        t024i,                         " Maintenance groups
        t356_t,                        " Priority texts
        tcf10,                         " PRT Control keys
        t430,                          " Control keys
        t430t,                         " Control keys texts
        t352b,                         " Report schema
        tq15t,                         " Q.Catalogue type texts
        tq80,                          " Notification types
        tq80_t,                        " Notification texts
        tq8t,                          " Notification categories
        pmpl,                          " PM Print Log
        t390,                          " Workpaper definitions
        t390_t,                        " Workpaper text
        t392,                          " Print redirection definitions
        t392_v,                        " Values for print redirection
        itcpo,                         " Print options for SAPSCRIPT
        itcpp,                         " SAPSCRIPT Close result
        wworkpaper.                    " Workpaper and print options

TABLES: soud,
        t001,
        jsto,
        lfa1.
CONSTANTS: GV_TZONE(10) type C VALUE 'EAM_TZONE'. " For time zone
*... CONSTANTS.........................................................
CONSTANTS:
      c_box(5)      VALUE '|___|',     " used for creating templates
      c_part_box(5) VALUE ' ___ ',     " in forms See activity report
      c_underscore_str(80) VALUE
      '____________________________________' &
      '____________________________________',               "72 chars _
      c_qmel(10)      VALUE 'QMEL',    " Text file name
      c_qmfe(10)      VALUE 'QMFE',    " Text file name
      c_qmur(10)      VALUE 'QMUR',    " Text file name
      c_qmma(10)      VALUE 'QMMA',    " Text file name
      c_qmsm(10)      VALUE 'QMSM',    " Text file name
      c_aufk(10)      VALUE 'AUFK',    " Text file name
      c_ltxt(4)       VALUE 'LTXT',    " Long text ID for SAPSCRIPT
      c_qm_02(2)      VALUE '02',      " indicatior for QM Notification
      c_pm_01(2)      VALUE '01',      " indicatior for PM Notification
      c_sm_03(2)      VALUE '03',      " indicatior for SM Notification
      c_g0_05(2)      VALUE '05',      " indicatior for General Notif.
      c_ltqm(4)       VALUE 'LTQM',    " longtext ID for QM Longtexts
      c_avot(4)       VALUE 'AVOT',    " Long text ID for SAPSCRIPT
      c_pm_order_type(2) VALUE '30',   " PM order type
      c_start_line_nr LIKE syst-tabix  " Start line number for longtext
                      VALUE 1,
      c_last_line_nr LIKE syst-tabix   " Start line number for longtext
                      VALUE 99999,
      c_main(4)       VALUE 'MAIN',    " WINDOW MAIN
      c_header        VALUE 'H',       " (H)eader
      c_header_order  VALUE 'A',       " (A) Order
      c_header_notif  VALUE 'M',       " (M) Notification
      c_operation     VALUE 'O',       " (O)peration
      c_pos           VALUE 'P',       " (P)osition
      c_notif         VALUE 'N',       " (N)otification
      c_material      VALUE 'R',       " (R) Material
      c_insert LIKE riupd-indupd
                      VALUE 'I',       " (I)nsert
      c_printer(7)    VALUE 'PRINTER', " Device for SAPSCRIPT output
      c_telefax(7)    VALUE 'TELEFAX', " Fax
      c_preview(7)    VALUE 'PREVIEW', " Print view
      c_screen(7)     VALUE 'SCREEN ', " Device for SAPSCRIPT output
      c_prt_e         VALUE 'E',       " PRT is Equipment   "30c
      c_prt_m         VALUE 'M',       " PRT is material
      c_prt_s         VALUE 'S',       " PRT is Other (SONSTIGE)
      c_prt_d         VALUE 'D',       " PRT is document
      yes             VALUE 'X',
      no              VALUE ' ',
      generic         VALUE '*',
      status_print LIKE jest-stat
                   VALUE 'I0007',      " Status system internal
      id_iprt_orddata(16)  VALUE 'ID_IPRT_ORDDATA',
      id_iprt_options(16)  VALUE 'ID_IPRT_OPTIONS',
      id_iprt_struct(16)   VALUE 'ID_IPRT_STRUCT',
      id_iprt_rqm00(16)    VALUE 'ID_IPRT_RQM00',
      c_dara LIKE toa_dara-function VALUE 'DARA',
*..   C_ARC_TYPE_QMEL LIKE SWOTBASDAT-OBJTYPE VALUE 'QMEL',
*.. replace with and G_Arc_Type
      c_arc_type_qm   LIKE swotbasdat-objtype VALUE 'BUS2078',
      c_arc_type_sm   LIKE swotbasdat-objtype VALUE 'BUS2080',
      c_arc_type_pm   LIKE swotbasdat-objtype VALUE 'BUS2038',
      c_arc_type_g0   LIKE swotbasdat-objtype VALUE 'BUS7051',
*     C_ARC_TYPE_AUFK LIKE SWOTBASDAT-OBJTYPE VALUE 'BUS2007', "n766146
      c_arc_type_a_pm LIKE swotbasdat-objtype VALUE 'BUS2007', "n766146
      c_arc_type_a_cs LIKE swotbasdat-objtype VALUE 'BUS2088'. "n766146

*... General variables
DATA:
       g_arc_type LIKE    swotbasdat-objtype,
       gv_arc_type_aufk   LIKE swotbasdat-objtype,          "n766146
       gs_t350            LIKE t350,                        "n766146
       g_repeat LIKE pmpl-copy_nr,
       rc LIKE syst-subrc,             " local return code
       ltxt_id  LIKE ttxid-tdid,       " Text id for longtexts
       op_entries LIKE syst-tabix,     " Individual oper. print tab cnt
       entries LIKE syst-tabix,        " entries in a table
       time LIKE qmih-auszt,           " Time conversion field
       time_packed(12) TYPE p DECIMALS 2, " special output field
       unit_int    LIKE t006-msehi,
       found(1) TYPE c,                " found flag.
       multi_class(1),                 " multi classification indic.
       last_copy_nr LIKE pmpl-copy_nr, " Last copy printed was ?
       no_log,                         " Flag logging not possible (X/ )
       dont_log,                       " Printed on screen, no log rec
       dest_device(7),                 " determined device
       device(7) TYPE c,               " PRINTER or SCREEN or PREVIEW
       original_print_language LIKE thead-tdspras,
       print_language LIKE thead-tdspras, " default print language
       text_object_name LIKE thead-tdname, " Text name workarea
       sy_uname LIKE sy-uname,         " SY field for update task
       sy_datum LIKE sy-datum.         "

*... printer redirection vars
* Unicode: No typing possible here, since every field can be used
FIELD-SYMBOLS: <field_cont>.           " check contents of the field
FIELD-SYMBOLS: <field_werk> TYPE clike." check contents of PLANT field
DATA: field_cont_name(21).             " Build a valid ABAP field name
DATA: field_werk_name(21).             " Build a valid PLANT name
DATA: field_known.                     " assign worked
DATA: plant_field_known.               " plant field assign worked ?
DATA: last_printer(4) VALUE '!$+?'.    " did the the printer change ??
DATA: form_open_flag.                  " is a SAPSCRIPT FORM OPEN

*.......................................................................
*... Workareas IMPORTED in form DATA_IMPORT, information to print.
* These structure must be inlcluded because the IMPORT command
* fills these structures.  The output structures and FORMS
* requires exactly these structures and not with other names.
*.......................................................................
DATA  iviqmel LIKE viqmel.

* TYPES for Tables and Structrues
INCLUDE liprtf04.

* Positions
DATA  iviqmfe   TYPE t_iviqmfe   WITH HEADER LINE.
* Actions
DATA  iviqmma   TYPE t_iviqmma   WITH HEADER LINE.
* Activities
DATA  iviqmsm   TYPE t_iviqmsm   WITH HEADER LINE.
* Causes
DATA  iviqmur   TYPE t_iviqmur   WITH HEADER LINE.
* Quality code texts.
* Used to hold damage, cause and OBject codes and texts
DATA  iqkat     TYPE t_iqkat     WITH HEADER LINE.
* Partner dialog print
DATA  ihpad_tab TYPE t_ihpad_tab WITH HEADER LINE.
* special permissions
DATA  ihsg_tab     TYPE t_ihsg_tab  WITH HEADER LINE.
* special permission activated
DATA  ihgns_tab    TYPE t_ihgns_tab WITH HEADER LINE.
* capacity loads and splits
DATA  kbedp_tab    TYPE t_kbedp_tab WITH HEADER LINE.
* single oper print table
DATA  op_print_tab TYPE t_op_print_tab WITH HEADER LINE.
* NOTIFication partner table
DATA  notif_ihpad_tab TYPE t_notif_ihpad_tab WITH HEADER LINE.
* ORDER partner table
DATA  order_ihpad_tab TYPE t_order_ihpad_tab WITH HEADER LINE.
* order operations
DATA  iafvgd          TYPE t_afvgd           WITH HEADER LINE.
* Import table for PRTs
DATA  iaffhd          TYPE t_affhd           WITH HEADER LINE.
* Import table for object list
DATA  iripw0          TYPE t_ripw0           WITH HEADER LINE.
* Import table for materials
DATA  iresbd          TYPE t_resbd           WITH HEADER LINE.
*--------------------------------------------------------------------*
*--- Assigned Objects
DATA iqnao_tab  TYPE qnaot_obj_data_ui.
*--- Assigned Objects
*--------------------------------------------------------------------*

DATA:   BEGIN OF wcaufvd.              " Import workarea
        INCLUDE STRUCTURE caufvd.      " order header
DATA:   END   OF wcaufvd.

DATA:   BEGIN OF wriwo1.               " Import workarea
        INCLUDE STRUCTURE riwo1.       " ILOA details
DATA:   END   OF wriwo1.

DATA:   BEGIN OF stat_tab OCCURS 0.    " Internal status setting table
        INCLUDE STRUCTURE jstat.                            "
DATA:   END   OF stat_tab.

DATA: BEGIN OF temp_t392_v OCCURS 0.
        INCLUDE STRUCTURE t392_v.
DATA: END   OF temp_t392_v.

DATA: BEGIN OF drad_tab OCCURS   0.   "DMS document to Object allocation
        INCLUDE STRUCTURE drad.
DATA: END OF drad_tab.

DATA: BEGIN OF draw_tab OCCURS   0.    "DMS document
        INCLUDE STRUCTURE draw.
DATA: END OF draw_tab.

DATA:   BEGIN OF iriwo1 OCCURS 0.
        INCLUDE STRUCTURE riwo1.
DATA:   END   OF iriwo1.



*... Other special variables
DATA:   BEGIN OF object,               " classification OBJECT
           qmnum(12),                  " Notif number
           fenum(4),                   " position number
           filler(34),                                      "
        END OF object.

*$*$ Global struct for Print.  AVAILABLE in All Control ABAPS
***LONGTEXT processing vars
DATA: del_lines TYPE p.                " start lines to remove

DATA: BEGIN OF table_header.           " define a local table header
        INCLUDE STRUCTURE thead.         " using SAPSCRIPT structure
DATA: END   OF table_header.

DATA: BEGIN OF table_lines OCCURS 0.   " define a local text lines
        INCLUDE STRUCTURE tline.         " using SAPSCRIPT structure
DATA: END   OF table_lines.

DATA: text_object LIKE thead-tdobject, " To make sure the right
      text_name   LIKE thead-tdname,   " sizes are used.
      text_id     LIKE thead-tdid.

DATA frompage TYPE p.

DATA: dokob LIKE drad-dokob,           " DMS longtext graphics
      objky LIKE drad-objky.                                "

*** end longtext processing vars

DATA: params_ok.
DATA: BEGIN OF g_pri_params.           " Print parameters
        INCLUDE STRUCTURE pri_params.  " fort direct spool
DATA: END OF g_pri_params.             " operations

DATA: BEGIN OF g_toa_dara_tab.         " Index INFO  used  in
        INCLUDE STRUCTURE toa_dara.    " open Form call with
DATA: END OF g_toa_dara_tab.           " archiving info

DATA: BEGIN OF g_arc_params_tab.       " Parameters  used  in
        INCLUDE STRUCTURE arc_params.  " open Form call with
DATA: END OF g_arc_params_tab.         " archiving info

*$*$ for catalog texts of fact

DATA: fact_txtgr   LIKE iviqmfe-txtgr,
      fact_txtcdgr LIKE iviqmfe-txtcdgr.

*$*$ the following is used internally only.
INCLUDE <cntn01>.   " Makros für Zugriff aufs BOR

DATA: sender_id      LIKE swotobjid,
      appl_object_id LIKE swotobjid,
      recipient_id   LIKE swotobjid,
      recipient      TYPE swc_object,
      sender         TYPE swc_object,
      recipient_tab  TYPE swc_object OCCURS 0 WITH HEADER LINE,
      folder         TYPE swc_object,
      BEGIN OF sofmfol_key,
          foldertype   LIKE sofm-foltp,
          folderyear   LIKE sofm-folyr,
          foldernumber LIKE sofm-folno,
          type         LIKE sofm-doctp,
          year         LIKE sofm-docyr,
          number       LIKE sofm-docno,
          forwarder    LIKE soub-usrnam,
      END OF sofmfol_key,
      bor_key        LIKE swotobjid-objkey,
      address_string LIKE soxna-fullname.

*$*$ only used for internal services
TYPE-POOLS msfo.

DATA: variablen TYPE msfo_tab_variablen WITH HEADER LINE.

DATA: formel TYPE msfo_formel.

TABLES: ml_esll,
        esll,
        thead,
        ttxit.

DATA: xthead LIKE thead OCCURS 0 WITH HEADER LINE.
DATA: gliederung LIKE ml_esll OCCURS 0 WITH HEADER LINE,
      leistung LIKE ml_esll OCCURS 0 WITH HEADER LINE.

*$*$ Forms for e-mail support
INCLUDE riprif10.

DATA gb_riprif01_pdf_paper_print type ref to badi_riprif01_pdf_paper_print.
DATA gv_pdf_paper_selected type xflag value space.

*--added for status
data : begin of ijest_wa,
       OBJNR type jest-OBJNR,
       STAT  type jest-STAT ,
       INACT type jest-INACT,
       CHGNR type jest-CHGNR,
       end of ijest_wa.

data : begin of ijest_wa1,
       OBJNR type jest-OBJNR,
       STAT  type jest-STAT ,
       INACT type jest-INACT,
       CHGNR type jest-CHGNR,
       TXT30 type TJ30T-TXT30,
       USNAM  type jcds-USNAM,
       UDATE  type jcds-UDATE,
       indi ,
       end of ijest_wa1.

data : begin of iTJ30T_wa,
       STSMA type TJ30T-STSMA,
       ESTAT type TJ30T-ESTAT,
       SPRAS type TJ30T-SPRAS,
       TXT04 type TJ30T-TXT04,
       TXT30 type TJ30T-TXT30,
       end of  iTJ30T_wa.

data :begin of ijcds_wa,
      OBJNR  type jcds-OBJNR,
      STAT   type jcds-STAT,
      CHGNR  type jcds-CHGNR,
      USNAM  type jcds-USNAM,
      UDATE  type jcds-UDATE,
      UTIME  type jcds-UTIME,
      INACT  type jcds-INACT,
      CHIND  type jcds-CHIND,
      end of ijcds_wa.

data : ijest like standard table of ijest_wa,
       ijest1 like standard table of ijest_wa1,
       iTJ30T like standard table of iTJ30T_wa,
       ijcds  like standard table of ijcds_wa.

data: username(40) type c,
      userresp(40) type c.  " IRDK933219
