FUNCTION ZFM_BM_CS_EXCEL_UPLOAD .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  I OPTIONAL
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_STEP) TYPE  I DEFAULT 1
*"  CHANGING
*"     REFERENCE(CT_DATA_RAW) TYPE  ZTT_BM_CS_DATA_RAW
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------

* Copy from TEXT_CONVERT_XLS_TO_SAP

  TYPE-POOLS: SOI, CNTL.
  CONSTANTS: G_CON_EXCEL      TYPE CHAR80 VALUE 'Excel.Sheet',
             G_MAX_EMPTY_ROWS TYPE I VALUE 5.

  DATA L_OREF_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
  DATA L_IREF_CONTROL TYPE REF TO I_OI_CONTAINER_CONTROL.
  DATA L_IREF_ERROR TYPE REF TO I_OI_ERROR.
  DATA L_IREF_DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY.
  DATA: L_CNTL_HANDLE TYPE CNTL_HANDLE.
  DATA  L_IREF_SPREADSHEET TYPE REF TO I_OI_SPREADSHEET.
  DATA  L_RETCODE TYPE SOI_RET_STRING.
  DATA: L_CURRENT_ROW TYPE I,
        L_TOP         TYPE I,
        L_LEFT        TYPE I,
        L_ROWS        TYPE I,
        L_COLUMNS     TYPE I,
        L_COUNTER     TYPE I,
        L_LINE_FROM   TYPE I,
        L_LINE_TO     TYPE I.
  DATA: L_OREF_STRUCTURE TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA: L_RANGE_LIST TYPE SOI_RANGE_LIST.
  DATA: L_TABLE       TYPE SOI_GENERIC_TABLE.
  DATA: L_TABLE_RANGE TYPE SOI_GENERIC_TABLE.
  DATA: L_GEN_TAB LIKE LINE OF L_TABLE.
  FIELD-SYMBOLS <FS_DATA>.

  DATA:
    L_TEXT6(6),
    L_TEXT80(80).

  PERFORM GET_SPREADSHEET_INTERFACE USING G_CON_EXCEL
                                    CHANGING
                                      I_FILENAME
                                      L_OREF_CONTAINER L_IREF_CONTROL
                                      L_IREF_ERROR     L_IREF_DOCUMENT
                                      L_IREF_SPREADSHEET.

  IF L_IREF_SPREADSHEET IS INITIAL.
    MESSAGE E893(UX) WITH I_FILENAME RAISING CONVERSION_FAILED.
  ENDIF.

**********************************************************************
* Sheets processing - Start
**********************************************************************
  DATA:
    LT_SHEETS    TYPE SOI_SHEETS_TABLE,
    LR_LINE_DATA TYPE REF TO DATA.
  FIELD-SYMBOLS:
    <LFT_CONVERTED_DATA> TYPE STANDARD TABLE,
    <LF_CONVERTED_DATA>  TYPE ANY.

  CALL METHOD L_IREF_SPREADSHEET->GET_SHEETS
    IMPORTING
      SHEETS = LT_SHEETS.
  LOOP AT LT_SHEETS INTO DATA(LS_SHEET).
**********************************************************************
* Sheets processing - End
**********************************************************************
*   Init
    CLEAR: L_LEFT, L_ROWS, L_TOP, L_CURRENT_ROW, L_TABLE_RANGE,
           L_COUNTER, L_LINE_FROM, L_LINE_FROM, L_COLUMNS.

*   Check sheet name match with function
    READ TABLE CT_DATA_RAW ASSIGNING FIELD-SYMBOL(<LF_DATA_RAW>)
      WITH KEY CSFUNC = LS_SHEET-SHEET_NAME.
    CHECK SY-SUBRC IS INITIAL.

*   Active sheet
    L_IREF_SPREADSHEET->SELECT_SHEET( NAME = LS_SHEET-SHEET_NAME ).

    ASSIGN <LF_DATA_RAW>-REFDATA->* TO <LFT_CONVERTED_DATA>.
    CREATE DATA LR_LINE_DATA LIKE LINE OF <LFT_CONVERTED_DATA>.
    ASSIGN LR_LINE_DATA->* TO <LF_CONVERTED_DATA>.

    L_OREF_STRUCTURE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA(
                        <LF_CONVERTED_DATA> ).
*    L_OREF_STRUCTURE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA(
*                        I_TAB_CONVERTED_DATA ).
*   adjust column and rows ...
    DESCRIBE TABLE L_OREF_STRUCTURE->COMPONENTS LINES L_COLUMNS.
    L_LEFT = 1.
    L_ROWS = 100.
    IF NOT I_LINE_HEADER IS INITIAL.
      L_TOP = I_LINE_HEADER + 1.
    ELSE.
      L_TOP = 1.
    ENDIF.

    WHILE L_CURRENT_ROW <= G_MAX_EMPTY_ROWS.
      REFRESH L_TABLE_RANGE.
      ADD 1 TO L_COUNTER.
      L_LINE_FROM = L_LINE_TO + 1.

      CALL METHOD L_IREF_SPREADSHEET->SET_SELECTION
        EXPORTING
          TOP     = L_TOP
          LEFT    = L_LEFT
          ROWS    = L_ROWS
          COLUMNS = L_COLUMNS
        IMPORTING
          RETCODE = L_RETCODE.
      CALL METHOD L_IREF_SPREADSHEET->INSERT_RANGE
        EXPORTING
          COLUMNS = L_COLUMNS
          ROWS    = L_ROWS
          NAME    = 'SAP_range1'
        IMPORTING
          RETCODE = L_RETCODE.

      CALL METHOD L_IREF_SPREADSHEET->GET_RANGES_NAMES
        IMPORTING
          RANGES  = L_RANGE_LIST
          RETCODE = L_RETCODE.

      DELETE L_RANGE_LIST WHERE NAME <> 'SAP_range1'.

      CALL METHOD L_IREF_SPREADSHEET->GET_RANGES_DATA
*               exporting all      = 'X'
        IMPORTING
          CONTENTS = L_TABLE_RANGE
          RETCODE  = L_RETCODE
        CHANGING
          RANGES   = L_RANGE_LIST.
      LOOP AT L_TABLE_RANGE INTO L_GEN_TAB.
        AT NEW ROW.
          REFRESH L_TABLE.
        ENDAT.

        APPEND L_GEN_TAB TO L_TABLE.

        AT END OF ROW.
          PERFORM PARSE_TABLE_LINE USING    L_TABLE SY-TABIX
                                   CHANGING <LF_CONVERTED_DATA>.
*                                   CHANGING I_TAB_CONVERTED_DATA.
          IF <LF_CONVERTED_DATA> IS INITIAL.
            L_CURRENT_ROW = L_CURRENT_ROW + 1.
          ELSE.
            APPEND <LF_CONVERTED_DATA> TO <LFT_CONVERTED_DATA>.
            CLEAR L_CURRENT_ROW.
          ENDIF.
        ENDAT.
      ENDLOOP.
      L_TOP = L_TOP + L_ROWS.

      IF L_COUNTER MOD I_STEP = 0
        AND SY-BATCH IS INITIAL.
*        DESCRIBE TABLE I_TAB_CONVERTED_DATA LINES L_LINE_TO.
        DESCRIBE TABLE <LFT_CONVERTED_DATA> LINES L_LINE_TO.
        L_TEXT80 = TEXT-KOY.
        L_TEXT6 = L_LINE_FROM.
        REPLACE '&&&&&&' WITH L_TEXT6 INTO L_TEXT80.
        L_TEXT6 = L_LINE_TO.
        REPLACE '&&&&&&' WITH L_TEXT6 INTO L_TEXT80.
        REPLACE '&&&&&&' WITH TEXT-EXL INTO L_TEXT80.
        CONDENSE L_TEXT80.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            TEXT = L_TEXT80.
      ENDIF.
    ENDWHILE.
  ENDLOOP.

  FREE: L_IREF_SPREADSHEET.
  CALL METHOD L_IREF_DOCUMENT->CLOSE_DOCUMENT.
  CALL METHOD L_IREF_DOCUMENT->RELEASE_DOCUMENT.
  FREE L_IREF_DOCUMENT.

  CALL METHOD L_IREF_CONTROL->RELEASE_ALL_DOCUMENTS.
  CALL METHOD L_IREF_CONTROL->DESTROY_CONTROL.

* correct the decimals
  PERFORM CORRECT_DECIMALS_FOR_CURRENT TABLES <LFT_CONVERTED_DATA>.


ENDFUNCTION.
