DATA at_data_alv  TYPE TABLE OF gty_output.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_tree
        IMPORTING columnname node_key.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_link_click.
    DATA: lt_bdc TYPE TABLE OF bdcdata.
    CASE columnname.
      WHEN 'VBELN'.
        TRY.
            DATA(lv_vbeln) = at_data_alv[ node_key ]-vbeln.
            IF lv_vbeln IS NOT INITIAL.
              SET PARAMETER ID 'AUN' FIELD lv_vbeln.
              CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.

        ENDTRY.
      WHEN 'XBLNR'.
        TRY.
            DATA(lv_xblnr) = at_data_alv[ node_key ]-xblnr.
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
    DATA: ao_tree      TYPE REF TO cl_salv_tree,
          ao_sort      TYPE REF TO cl_salv_sorts,
          ao_layout    TYPE REF TO cl_salv_layout,
          ao_functions TYPE REF TO cl_salv_functions_tree,
          ao_columns   TYPE REF TO cl_salv_columns_tree,
          ao_settings  TYPE REF TO cl_salv_tree_settings,
          ao_events    TYPE REF TO lcl_handle_events,
          at_data_out  TYPE gtt_output.
    METHODS:
      constructor IMPORTING it_data TYPE tt_vbak,
      create_tree,
      add_node      IMPORTING iv_rel_node   TYPE salv_de_node_key
                              is_data       TYPE gty_output
                    RETURNING VALUE(rv_key) TYPE lvc_nkey,
      get_salv_tree RETURNING VALUE(ro_salv_tree) TYPE REF TO cl_salv_tree,
      adjust_layout,
      adjust_functions,
      adjust_columns,
      adjust_settings,
      customize_node IMPORTING io_node TYPE REF TO cl_salv_node,
      set_icon_and_tooltip IMPORTING io_node      TYPE REF TO cl_salv_node
                                     iv_item_name TYPE string
                                     iv_icon      TYPE string
                                     iv_tooltip   TYPE string,
      display.
ENDCLASS.

CLASS lcl_data_output IMPLEMENTATION.
  METHOD constructor.
    ao_tree = get_salv_tree( ).

    IF ao_tree IS BOUND.
      at_data_out = CORRESPONDING #( it_data MAPPING bukrs = bukrs_vf ).

      ao_settings = ao_tree->get_tree_settings( ).
      ao_layout     = ao_tree->get_layout( ).
      ao_functions  = ao_tree->get_functions( ).
      ao_columns    = ao_tree->get_columns( ).

      create_tree( ).

    ENDIF.
  ENDMETHOD.

  METHOD get_salv_tree.
    IF sy-batch = abap_false.
      TRY.
          cl_salv_tree=>factory(
            EXPORTING
              hide_header = abap_true
            IMPORTING
              r_salv_tree = ro_salv_tree
            CHANGING
              t_table     = at_data_alv
          ).

          DATA(lo_events) = ro_salv_tree->get_event( ).

          CREATE OBJECT ao_events.
          SET HANDLER ao_events->on_link_click FOR lo_events.

        CATCH cx_salv_error INTO DATA(lx_salv_error).
          MESSAGE |{ lx_salv_error->get_text( ) }| TYPE if_xo_const_message=>error.
      ENDTRY.
    ELSE.
      MESSAGE TEXT-v02 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD add_node.
    DATA(nodes) = ao_tree->get_nodes( ).
    TRY.
        DATA(node) = nodes->add_node(
                 related_node   = iv_rel_node
                 relationship   = cl_gui_column_tree=>relat_last_child
                 data_row       = is_data
               ).

      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.
    rv_key = node->get_key( ).
  ENDMETHOD.

  METHOD create_tree.
    DATA: ls_data TYPE gty_output.

    LOOP AT at_data_out ASSIGNING FIELD-SYMBOL(<fs_data_out>)
    GROUP BY ( bukrs = <fs_data_out>-bukrs size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_bukrs>).

      CLEAR ls_data.
      ls_data-bukrs = <fg_bukrs>-bukrs.
      ls_data-docs = <fg_bukrs>-size.

      DATA(bukrs_key) = add_node( iv_rel_node = '' is_data =  ls_data ).

      LOOP AT GROUP <fg_bukrs> ASSIGNING FIELD-SYMBOL(<fi_bukrs>)
      GROUP BY ( vkorg = <fi_bukrs>-vkorg size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_vkorg>).

        CLEAR ls_data.
        ls_data-vkorg = <fg_vkorg>-vkorg.
        ls_data-docs = <fg_vkorg>-size.

        DATA(vkorg_key) = add_node( iv_rel_node = bukrs_key is_data = ls_data ).

        LOOP AT GROUP <fg_vkorg> ASSIGNING FIELD-SYMBOL(<fi_vkorg>)
        GROUP BY ( bsark = <fi_vkorg>-bsark size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_bsark>).

          CLEAR ls_data.
          ls_data-bsark = <fg_bsark>-bsark.
          ls_data-docs = <fg_bsark>-size.

          DATA(bsark_key) = add_node( iv_rel_node = vkorg_key is_data = ls_data ).

          LOOP AT GROUP <fg_bsark> ASSIGNING FIELD-SYMBOL(<fi_bsark>)
          GROUP BY ( augru = <fi_bsark>-auart size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_augru>).

            CLEAR ls_data.
            ls_data-augru = <fg_augru>-augru.
            ls_data-docs = <fg_augru>-size.

            DATA(augru_key) = add_node( iv_rel_node = bsark_key is_data = ls_data ).

            LOOP AT GROUP <fg_augru> ASSIGNING FIELD-SYMBOL(<fi_augru>)
            GROUP BY (  kunnr = <fi_augru>-kunnr size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_kunnr>).

              CLEAR ls_data.
              ls_data-kunnr = <fg_kunnr>-kunnr.
              ls_data-docs = <fg_kunnr>-size.

              DATA(kunnr_key) = add_node( iv_rel_node = augru_key is_data = ls_data ).

              LOOP AT GROUP <fg_kunnr> ASSIGNING FIELD-SYMBOL(<fi_kunnr>)
              GROUP BY ( auart = <fi_kunnr>-auart size = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<fg_auart>).

                LOOP AT GROUP <fg_auart> ASSIGNING FIELD-SYMBOL(<fi_auart>).

                  DATA(nodes) = ao_tree->get_nodes( ).
                  TRY.
                      DATA(last_node) = nodes->add_node(
                               related_node   = kunnr_key
                               relationship   = cl_gui_column_tree=>relat_last_child
                               data_row       = <fi_auart>
                             ).

                      customize_node( io_node = last_node ).
                    CATCH cx_salv_msg.
                  ENDTRY.
                ENDLOOP.
              ENDLOOP.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD customize_node.
    DATA: ls_components TYPE abap_compdescr,
          lo_strucdescr TYPE REF TO cl_abap_structdescr.

    DATA: lo_item       TYPE REF TO cl_salv_item,
          lv_item_value TYPE REF TO data.

    DATA(data_row) = io_node->get_data_row( ).

    ASSIGN data_row->* TO FIELD-SYMBOL(<fs_data_row>).
    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( <fs_data_row> ).

    LOOP AT lo_strucdescr->components INTO ls_components.
      CASE ls_components-name.
        WHEN 'VBELN'.
          set_icon_and_tooltip( io_node = io_node
                                iv_item_name = CONV #( ls_components-name )
                                iv_icon = '@16'
                                iv_tooltip = 'Click to see details').
        WHEN 'XBLNR'.
          set_icon_and_tooltip( io_node = io_node
                      iv_item_name = CONV #( ls_components-name )
                      iv_icon = '@FA'
                      iv_tooltip = 'Click to see details').
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_icon_and_tooltip.
    TRY.
        DATA(lo_item) = io_node->get_item( CONV #( iv_item_name ) ).
        lo_item->set_type( if_salv_c_item_type=>link ).
        DATA(lv_item_value) = lo_item->get_value( ).
        ASSIGN lv_item_value->* TO FIELD-SYMBOL(<fs_item_value>).
        DATA(lv_icon_text) = |{ iv_icon }\\Q { iv_tooltip } @|.
        IF <fs_item_value> IS ASSIGNED AND <fs_item_value> IS NOT INITIAL.
          lo_item->set_icon( value = CONV #( lv_icon_text ) ).
        ENDIF.
      CATCH cx_salv_msg.

    ENDTRY.
  ENDMETHOD.

  METHOD display.
    "Additional settings before display
    adjust_settings( ).
    adjust_layout( ).
    adjust_functions( ).
    adjust_columns( ).

    ao_tree->display( ).
  ENDMETHOD.

  METHOD adjust_settings.
    ao_settings->set_hierarchy_header( 'Drilldown' ).
    ao_settings->set_header( CONV #( sy-title ) ).
  ENDMETHOD.

  METHOD adjust_layout.
    DATA: ls_layout_key TYPE salv_s_layout_key.

    ls_layout_key-report = sy-repid.

    ao_layout->set_key( ls_layout_key ).
    ao_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ENDMETHOD.

  METHOD adjust_functions.
    ao_tree->set_screen_status(
      pfstatus      =  'SALV_STANDARD'
      report        =  sy-repid
      set_functions =  ao_tree->c_functions_all ).

  ENDMETHOD.

  METHOD adjust_columns.
    DATA lo_column TYPE REF TO cl_salv_column.

    TRY.
        ao_columns->set_optimize( abap_true ).
        lo_column = ao_columns->get_column( columnname = 'NETWR' ).
        lo_column->set_currency_column( value = 'WAERK' ).

        DATA(lo_aggrs) = ao_tree->get_aggregations( ).

        TRY.
            CALL METHOD lo_aggrs->add_aggregation
              EXPORTING
                columnname  = 'NETWR'
                aggregation = if_salv_c_aggregation=>total.

          CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing. "#EC NO_HANDLER
        ENDTRY.

        lo_column = ao_columns->get_column( columnname = 'DOCS' ).
        lo_column->set_short_text( value = 'No.Docs' ).
        lo_column = ao_columns->get_column( columnname = 'ERDAT' ).
        lo_column->set_short_text( value = 'Created on' ).
        lo_column = ao_columns->get_column( columnname = 'ERNAM' ).
        lo_column->set_short_text( value = 'Created by' ).


      CATCH cx_salv_not_found cx_salv_data_error. " ALV: General Error Class (Checked During Syntax Check)
        MESSAGE |Error in ALV columns| TYPE if_xo_const_message=>error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
"lcl_handle_events DEFINITION
