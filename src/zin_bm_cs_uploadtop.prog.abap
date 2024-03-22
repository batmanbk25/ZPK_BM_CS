*&---------------------------------------------------------------------*
*& Include ZIN_BM_CS_UPLOADTOP         - Report ZPG_BM_CS_UPLOAD
*&---------------------------------------------------------------------*
**********************************************************************
* CONSTANTS                                                          *
**********************************************************************
  CONSTANTS:
    BEGIN OF GC_CSFSTS,
      NULL    TYPE ZDD_BM_CS_CSFSTS VALUE ' ', "Null
      NOT_YET TYPE ZDD_BM_CS_CSFSTS VALUE '1', "Not yet
      WIP     TYPE ZDD_BM_CS_CSFSTS VALUE '2', "WIP
      ERROR   TYPE ZDD_BM_CS_CSFSTS VALUE '3', "Error
      SUCCESS TYPE ZDD_BM_CS_CSFSTS VALUE '4', "Done
      CANCEL  TYPE ZDD_BM_CS_CSFSTS VALUE '5', "Cancelled
    END OF GC_CSFSTS.

**********************************************************************
* DATA                                                               *
**********************************************************************
  DATA:
    GT_CSPJ_C          TYPE TABLE OF ZTB_BM_CSPJ_C,
    GT_CSPJ_L          TYPE TABLE OF ZTB_BM_CSPJ_L,
    GS_CSPJ_L          TYPE  ZTB_BM_CSPJ_L,
    GT_CSPJ_D_ORG      TYPE TABLE OF ZTB_BM_CSPJ_D,
    GT_CSPJ_D_UP       TYPE TABLE OF ZTB_BM_CSPJ_D,
    GT_CSPJ_L_UP       TYPE TABLE OF ZTB_BM_CSPJ_L,
    GT_CSTR_C          TYPE TABLE OF ZTB_BM_CSTR_C,
    GT_DATA_RAW        TYPE TABLE OF ZST_BM_CS_DATA_RAW,
    GO_ALV_0100        TYPE REF TO CL_GUI_ALV_GRID,
    GS_LAYOUT          TYPE LVC_S_LAYO,
    G_CUSTOM_CONTAINER TYPE REF TO   CL_GUI_CUSTOM_CONTAINER,
    G_CONTAINER        TYPE          SCRFNAME VALUE 'CONTAINER',
    G_ALV_GRID         TYPE REF TO   CL_GUI_ALV_GRID,
    G_ALV_GRID_D       TYPE REF TO   CL_GUI_ALV_GRID,
    GT_ALV             TYPE TABLE OF  ZST_BM_CSPJ_L,
    GT_DETAIL          TYPE TABLE OF  ZST_LVC_S_DETA,
    GT_FIELDCAT        TYPE LVC_T_FCAT,
    GT_FIELDCAT_D      TYPE LVC_T_FCAT,
    GV_FILE            TYPE LOCALFILE.
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
**********************************************************************
* PARAMETERS                                                         *
**********************************************************************
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
    PARAMETERS:
      P_PROJID TYPE ZTB_BM_CSPJ_D-PROJID MODIF ID GR3 MATCHCODE OBJECT ZSH_CS .
    SELECT-OPTIONS:
    S_DIDATE FOR SY-DATUM MODIF ID GR1,
    S_FILE   FOR GV_FILE MODIF ID GR1.
    PARAMETERS:
      P_FILENM TYPE ESEFTFRONT
                                    MATCHCODE OBJECT ICL_DIAGFILENAME MODIF ID GR2.
  SELECTION-SCREEN END OF BLOCK B1.
  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN POSITION 15.
      PARAMETERS:
       P_UPLOAD RADIOBUTTON GROUP ID  DEFAULT 'X' USER-COMMAND UC1.
      SELECTION-SCREEN COMMENT  17(30) TEXT-001.
      PARAMETERS:
   P_DIPLAY RADIOBUTTON GROUP ID.
      SELECTION-SCREEN COMMENT  50(10) TEXT-002.

    SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK B2.
