*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_REPORTTOP
*&---------------------------------------------------------------------*
REPORT ZPG_BM_CS_REPORT.
TABLES: ZTB_BM_CSPJ_C.

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
DATA:

  GT_DATA   TYPE TABLE OF ZST_BM_CSPJ_C,
  GT_ALV    TYPE TABLE OF ZST_BM_CSPJ_C,
  GT_DETAIL TYPE TABLE OF ZST_BM_CSPJ_D,
  GT_ALV1   TYPE TABLE OF ZST_BM_CSPJ_D,
  GS_ALV1   TYPE ZST_BM_CSPJ_D,
  WA        TYPE ZTB_BM_CSPJ_D,
  GT_CSTR_C TYPE TABLE OF ZTB_BM_CSTR_C.




"Temp chung cua ALV OOP
*-----------------------------------------------------------------------
* Reference variable definition
*-----------------------------------------------------------------------
DATA :
  GT_FIELDCAT        TYPE LVC_T_FCAT,
  GT_FIELDCAT1       TYPE LVC_T_FCAT,
  GV_CANCEL          TYPE MARK,
  GV_CSFSTS          TYPE ZDD_BM_CS_CSFSTS,
  GS_FIELDCAT        TYPE LVC_S_FCAT,
  GS_LAYOUT          TYPE LVC_S_LAYO,
  G_CUSTOM_CONTAINER TYPE REF TO   CL_GUI_CUSTOM_CONTAINER,
  G_CONTAINER        TYPE          SCRFNAME VALUE 'CONTAINER',
  G_ALV_GRID         TYPE REF TO   CL_GUI_ALV_GRID.
*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ZCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: BUTTON_CLICK
      FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
      IMPORTING ES_COL_ID
                ES_ROW_NO.
ENDCLASS.                    "HANDLE_EVENT DEFINITION


**********************************************************************
* PARAMETERS & SELECT-OPTIONS                                        *
**********************************************************************
SELECT-OPTIONS :
  S_PROJID FOR ZTB_BM_CSPJ_C-PROJID,
  S_MODLID FOR ZTB_BM_CSPJ_C-MODLID,
  S_CSFUNC FOR ZTB_BM_CSPJ_C-CSFUNC.
PARAMETERS:
 P_STATUS TYPE C LENGTH 10  AS  LISTBOX VISIBLE LENGTH 15 DEFAULT '*'.
