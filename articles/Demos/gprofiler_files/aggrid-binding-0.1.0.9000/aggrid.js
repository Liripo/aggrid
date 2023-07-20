(function() {
HTMLWidgets.widget({

  name: 'aggrid',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
    let gridOptions = {};
    const getRowId = function(params) {
      // TO DO: group unique id
      // https://www.ag-grid.com/javascript-data-grid/server-side-model-configuration/#supplying-unique-group-ids
      const rowGroupCols = params.columnApi.getRowGroupColumns();
      if (rowGroupCols.length == 0) {
        return params.data.rowid;
      }
      var Id;
      const thisGroupCol = rowGroupCols[params.level];
      if (params.parentKeys === undefined) {
        Id = params.data[thisGroupCol.getColDef().field];
      } else {
        Id = params.parentKeys[0];
      }

      if (params.data.rowid !== undefined) {
        Id = Id + params.data.rowid;
      }
      return Id;
    };
    function convertToArray(value) {
      if (typeof value === 'number') {
        return [value];
      } else if (Array.isArray(value)) {
        return value;
      } else {
        throw new Error('Input must be a number or an array');
      }
    }

    return {

      renderValue: function(x) {

        // columnDefs dict to assay; R Object has names will covert to dict
        // x.gridOptions.columnDefs = x.gridOptions.columnDefs;
        const onSelectionChanged = function(event) {
          // should use data id
          id = el.id + "_" + "rows_selected"
          let row_index;
          if (x.server) {
            let state = event.api.getServerSideSelectionState();
            if (state.selectAll) {
              console.log(state.toggledNodes);
              if (state.toggledNodes.length === 0) {
                row_index = [...Array(x.n_row)].map((v, k) => k + 1);
              } else {
                state.toggledNodes = state.toggledNodes.map((id) => Number(id));
                row_index = [...Array(x.n_row)].map((v, k) => k + 1);
                row_index = row_index.filter(x => !state.toggledNodes.includes(x));
              }
            } else {
              row_index = event.api.getSelectedNodes();
              row_index = row_index.map((item) => item.data.rowid);
            }
          } else{
            row_index = event.api.getSelectedNodes();
            row_index = row_index.map((item) => item.data.rowid);
          }
          Shiny.setInputValue(id, value = row_index);
        };
        gridOptions = x.gridOptions;
        if (HTMLWidgets.shinyMode) {
          gridOptions.onSelectionChanged = onSelectionChanged;
        }
        // console.log(gridOptions);
        if (x.server) {
          gridOptions.rowModelType = 'serverSide';
          gridOptions.maxBlocksInCache = 10;
          gridOptions.getRowId = getRowId;
        } else {
          gridOptions.rowData = x.data;
        }
        el.classList.add(x.theme || "ag-theme-balham");
        new agGrid.Grid(el, gridOptions);
        // set selectedRows
        if (x.selectedRows !== null) {
          if (x.selectedRows === "all") {
            if (x.server) {
              gridOptions.api.setServerSideSelectionState({
                selectAll: true,
              });
            } else {
              gridOptions.api.selectAll();
            }
          } else {
            x.selectedRows = convertToArray(x.selectedRows);
            if (x.server) {
              gridOptions.api.setServerSideSelectionState({
                selectAll: true,
                toggledNodes: x.selectedRows.map(x => `${x}`),
              });
            } else {
              x.selectedRows.map((item) => {
                item = item - 1;
                const rowNode = gridOptions.api.getRowNode(item);
                rowNode.setSelected(true);
              })
            }
          }
        }

        if (x.server) {
          // add Total rows
          const statbar_el = el.querySelector(".ag-status-bar-right .ag-status-name-value");
          statbar_el.classList.remove("ag-hidden");
          statbar_el.children[1].innerText = x.n_row;
          const datasource = {
            // get rows data
            getRows(params) {
              fetch(x.dataURL,{
                method: 'post',
                body: JSON.stringify(params.request),
                headers: { 'Content-Type': 'application/json; charset=utf-8' }
              })
              .then(response => response.json())
              .then(data => {
                const statbar_el = el.querySelector(".ag-status-bar-left .ag-status-name-value");
                statbar_el.children[0].innerText = "Rows:";
                statbar_el.children[1].innerText = data.lastRow;
                params.successCallback(data.rowData, data.lastRow);
              });
            }
          };
          // register datasource with the grid
          gridOptions.api.setServerSideDatasource(datasource);
        }
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size
        // const column_stats = gridOptions.columnApi.getColumnState();
        // const total_with = column_stats.reduce((acc, item) => acc + item.width, 0)
        // If the container size is greater than the total width, reset the container width
        // if (total_with < width) {
        //  var container = document.getElementById(el.id);
        //  console.log(container);
        //  console.log(total_with + "px");
        //  container.style.width = total_with + "px";
        // }
      }

    };
  }
});

})();
