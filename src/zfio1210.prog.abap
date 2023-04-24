DATA at_data_alv TYPE STANDARD TABLE OF gty_output.

CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_link_click
                  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING column row.

ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_link_click.
    DATA: lt_bdc TYPE TABLE OF bdcdata.
    CASE column.
      WHEN 'VBELN'.
        TRY.
            DATA(lv_vbeln) = at_data_alv[ row ]-vbeln.
            IF lv_vbeln IS NOT INITIAL.
              SET PARAMETER ID 'AUN' FIELD lv_vbeln.
              CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.

        ENDTRY.
      WHEN 'XBLNR'.
        TRY.
            DATA(lv_xblnr) = at_data_alv[ row ]-xblnr.
            IF lv_xblnr IS NOT INITIAL.
              SET PARAMETER ID 'VF' FIELD lv_xblnr.
              CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.

        ENDTRY.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_data_output DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA: ao_salv      TYPE REF TO cl_salv_table,
          ao_sort      TYPE REF TO cl_salv_sorts,
          ao_layout    TYPE REF TO cl_salv_layout,
          ao_functions TYPE REF TO cl_salv_functions,
          ao_columns   TYPE REF TO cl_salv_columns,
          ao_events    TYPE REF TO lcl_handle_events.
    METHODS:
      constructor IMPORTING it_data TYPE tt_vbak,
      get_salv_table RETURNING VALUE(ro_salv_table) TYPE REF TO cl_salv_table,
      adjust_layout,
      adjust_functions,
      adjust_columns,
      display.
ENDCLASS.

CLASS lcl_data_output IMPLEMENTATION.
  METHOD constructor.
    ao_salv = get_salv_table( ).

    IF ao_salv IS BOUND.
      at_data_alv = CORRESPONDING #( it_data MAPPING bukrs = bukrs_vf ).

      ao_layout     = ao_salv->get_layout( ).
      ao_functions  = ao_salv->get_functions( ).
      ao_columns    = ao_salv->get_columns( ).

    ENDIF.
  ENDMETHOD.

  METHOD get_salv_table.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = ro_salv_table
          CHANGING
            t_table     = at_data_alv
        ).

        DATA(lo_events) = ro_salv_table->get_event( ).

        CREATE OBJECT ao_events.
        SET HANDLER ao_events->on_link_click FOR lo_events.

      CATCH cx_salv_msg INTO DATA(lx_salv_error).
        MESSAGE |{ lx_salv_error->get_text( ) }| TYPE if_xo_const_message=>error.
    ENDTRY.
  ENDMETHOD.

  METHOD display.
    "Additional settings before display
    adjust_layout( ).
    adjust_functions( ).
    adjust_columns( ).

    ao_salv->display( ).
  ENDMETHOD.

  METHOD adjust_layout.
    DATA: ls_layout_key TYPE salv_s_layout_key.

    ls_layout_key-report = sy-repid.

    ao_layout->set_key( ls_layout_key ).
    ao_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ENDMETHOD.

  METHOD adjust_functions.
    ao_functions->set_all(  abap_true ).
  ENDMETHOD.

  METHOD adjust_columns.
    DATA: lo_column TYPE REF TO cl_salv_column_table.

    ao_sort = ao_salv->get_sorts( ).

    TRY.
        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'BUKRS'.
        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'VKORG'.
        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'AUGRU'.
        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'KUNNR'.
        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'AUART'
            subtotal   = abap_true.

        CALL METHOD ao_sort->add_sort
          EXPORTING
            columnname = 'VBELN'.

        DATA(lo_aggregations) = ao_salv->get_aggregations( ).

        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = 'NETWR'
            aggregation = if_salv_c_aggregation=>total.

        lo_column ?= ao_columns->get_column( 'VBELN' ).
        lo_column->set_currency_column( value = 'WAERK' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        lo_column ?= ao_columns->get_column( 'XBLNR' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        lo_column ?= ao_columns->get_column( columnname = 'ERDAT' ).
        lo_column->set_short_text( value = 'Created on' ).
        lo_column ?= ao_columns->get_column( columnname = 'ERNAM' ).
        lo_column->set_short_text( value = 'Created by' ).

        ao_columns->set_optimize( abap_true ).



      CATCH cx_salv_not_found
            cx_salv_data_error
            cx_salv_existing INTO DATA(lx_exception).
        MESSAGE |{  lx_exception->get_text( ) }| TYPE if_xo_const_message=>error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
"lcl_handle_events DEFINITION
