(function() {

HTMLWidgets.widget({

  name: 'aggrid',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance
    let gridOptions = {};
    var onSelectionChanged = function(event) {
      id = el.id + "_" + "rows_selected"
      let row_index = event.api.getSelectedNodes();
      row_index = row_index.map((item) => item.rowIndex + 1);
      Shiny.setInputValue(id, value = row_index);
    };

    return {

      renderValue: function(x) {

        // columnDefs dict to assay; R Object has names will covert to dict
        x.gridOptions.columnDefs = Object.values(x.gridOptions.columnDefs);

        gridOptions = x.gridOptions;

        // Whether to allow users to override？
        if (HTMLWidgets.shinyMode) {
          gridOptions.onSelectionChanged = onSelectionChanged;
        }
        console.log(gridOptions);
        if (x.server) {
          gridOptions.rowModelType = 'serverSide';
          gridOptions.maxBlocksInCache = 10;
        } else {
          gridOptions.rowData = x.data;
        }
        el.classList.add(x.theme || "ag-theme-balham");
        new agGrid.Grid(el, gridOptions);
        if (x.server) {
          const datasource = {
            // get rows data
            getRows(params) {
              params.request.startRow += 1;
              fetch(x.dataURL,{
                method: 'post',
                body: JSON.stringify(params.request),
                headers: { 'Content-Type': 'application/json; charset=utf-8' }
              })
              .then(response => response.json())
              .then(data => {
                console.log(data);
                params.successCallback(data.rowData, data.nrow);
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
