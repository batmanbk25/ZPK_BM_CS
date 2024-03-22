*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_REPORTO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'PF1'.
  SET TITLEBAR 'ZGT_0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Class (Implementation) LCL_EVENTHANDLER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS ZCL_EVENT_HANDLER IMPLEMENTATION.
*Handle HOTSPOT_CLICK
  METHOD BUTTON_CLICK.
    PERFORM BUTTON_CLICK  USING ES_COL_ID
                                ES_ROW_NO.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE DISPLAY_ALV_0100 OUTPUT.

  DATA: LT_TOOLBAR TYPE UI_FUNCTIONS,
        LS_TOOLBAR TYPE UI_FUNC.
  DATA: GV_ALVGRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA: GS_VARIANT TYPE DISVARIANT.
  PERFORM BUILD_FCAT.

  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE = 'A'.
  GS_LAYOUT-ZEBRA    = 'A'.
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
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ZGS_0200'.
  SET TITLEBAR 'ZGT_0200'.
ENDMODULE.
