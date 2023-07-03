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

        gridOptions = x.gridOptions;
        // Whether to allow users to override？
        if (HTMLWidgets.shinyMode) {
          gridOptions.onSelectionChanged = onSelectionChanged;
        }
        console.log(gridOptions);
        el.classList.add(x.theme || "ag-theme-balham");
        new agGrid.Grid(el, gridOptions);
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
