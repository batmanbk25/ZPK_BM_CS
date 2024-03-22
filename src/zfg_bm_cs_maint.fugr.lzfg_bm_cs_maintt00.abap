*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTB_BM_CSTR_M...................................*
DATA:  BEGIN OF STATUS_ZTB_BM_CSTR_M                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTB_BM_CSTR_M                 .
CONTROLS: TCTRL_ZTB_BM_CSTR_M
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZVI_BM_CSTR_C...................................*
TABLES: ZVI_BM_CSTR_C, *ZVI_BM_CSTR_C. "view work areas
CONTROLS: TCTRL_ZVI_BM_CSTR_C
TYPE TABLEVIEW USING SCREEN '0102'.
DATA: BEGIN OF STATUS_ZVI_BM_CSTR_C. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVI_BM_CSTR_C.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVI_BM_CSTR_C_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_CSTR_C.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_CSTR_C_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVI_BM_CSTR_C_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_CSTR_C.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_CSTR_C_TOTAL.

*...processing: ZVI_BM_CSTR_DEV.................................*
TABLES: ZVI_BM_CSTR_DEV, *ZVI_BM_CSTR_DEV. "view work areas
CONTROLS: TCTRL_ZVI_BM_CSTR_DEV
TYPE TABLEVIEW USING SCREEN '0104'.
DATA: BEGIN OF STATUS_ZVI_BM_CSTR_DEV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVI_BM_CSTR_DEV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVI_BM_CSTR_DEV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_CSTR_DEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_CSTR_DEV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVI_BM_CSTR_DEV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVI_BM_CSTR_DEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVI_BM_CSTR_DEV_TOTAL.

*.........table declarations:.................................*
TABLES: *ZTB_BM_CSTR_M                 .
TABLES: ZTB_BM_CSTR_C                  .
TABLES: ZTB_BM_CSTR_M                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
