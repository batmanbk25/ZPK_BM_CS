*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_PROJECTO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_1001 OUTPUT.
  SET PF-STATUS 'ZALV3'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Class (Implementation) ZCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS ZCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD BUTTON_CLICK.
    PERFORM BUTTON_CLICK  USING ES_COL_ID
                                ES_ROW_NO.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE DISPLAY_ALV_1001 OUTPUT.
  DATA: LT_TOOLBAR TYPE UI_FUNCTIONS,
        LS_TOOLBAR TYPE UI_FUNC.
  DATA: GV_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA: GS_VARIANT TYPE DISVARIANT.
  PERFORM BUILD_FCAT.

  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE = 'A'.
  GS_LAYOUT-ZEBRA    = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  IF G_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.

    CREATE OBJECT G_ALV_GRID
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER.

    CALL METHOD G_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = LT_TOOLBAR
      CHANGING
        IT_OUTTAB            = GT_ALV
        IT_FIELDCATALOG      = GT_FIELDCAT[].

    SET HANDLER ZCL_EVENT_HANDLER=>BUTTON_CLICK FOR G_ALV_GRID.
*
  ELSE.
    CALL METHOD G_ALV_GRID->SET_FRONTEND_LAYOUT
      EXPORTING
        IS_LAYOUT = GS_LAYOUT.

    CALL METHOD G_ALV_GRID->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = GT_FIELDCAT[].

    CALL METHOD G_ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_1002 OUTPUT.
  DATA: LT_FUNC TYPE UI_FUNCTIONS.
  CLEAR LT_FUNC.
  IF G_HIDE = '0'.
    APPEND '&DETAIL' TO LT_FUNC.
    APPEND '&EXPORT' TO LT_FUNC.
*    APPEND '&EDIT' TO LT_FUNC.
    APPEND '&TOGGLE' TO LT_FUNC.
    SET PF-STATUS 'STANDARD' EXCLUDING LT_FUNC.
*  ELSEIF G_LOCK <> '0'.
*
*    APPEND '&EDIT' TO LT_FUNC.
*    SET PF-STATUS 'STANDARD' EXCLUDING LT_FUNC.

  ELSEIF G_LOCK = '0'.

    APPEND '&DATA_SAVE' TO LT_FUNC.
*    APPEND '&EDIT' TO LT_FUNC.
    SET PF-STATUS 'STANDARD' EXCLUDING LT_FUNC.
  ELSE.

    SET PF-STATUS 'STANDARD'.
  ENDIF.

  SET TITLEBAR 'CREATE PROJECT'.
  IF G_MODE = '2'.
    PERFORM CHECK.
  ELSEIF G_MODE = '3'.
    PERFORM CHECK2.
  ELSE .
    PERFORM HIDE_ALV.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE DISPLAY_ALV_1002 OUTPUT.
  CHECK G_MODE <> '1'.
  PERFORM RESELECT.
  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE = 'A'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  IF G_CUSTOM_CONTAINER1 IS INITIAL OR GV_FREE1 = '1'.

    CREATE OBJECT G_CUSTOM_CONTAINER1
      EXPORTING
        CONTAINER_NAME = G_CONTAINER1.

    CREATE OBJECT G_ALV_GRID1
      EXPORTING
        I_PARENT = G_CUSTOM_CONTAINER1.

    CALL METHOD G_ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        I_SAVE               = 'A'
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = LT_TOOLBAR
      CHANGING
        IT_OUTTAB            = GT_RESELECT
        IT_FIELDCATALOG      = GT_FIELDCAT1[].

  ELSE.
    CALL METHOD G_ALV_GRID1->SET_FRONTEND_LAYOUT
      EXPORTING
        IS_LAYOUT = GS_LAYOUT.

    CALL METHOD G_ALV_GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = GT_FIELDCAT1[].

    CALL METHOD G_ALV_GRID1->REFRESH_TABLE_DISPLAY.
  ENDIF.
  CLEAR: GV_FREE1.
ENDMODULE.
