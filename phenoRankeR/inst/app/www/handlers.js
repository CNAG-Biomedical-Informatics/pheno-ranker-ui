$(document).ready(function () {
  console.log("handlers.js loaded");
  Shiny.addCustomMessageHandler('changeURL', function (message) {
    console.log(message);
    var url = new URL(window.location.origin + window.location.pathname)
    url.searchParams.set('mode', message.mode);
    url.searchParams.set('id', message.id);
    history.pushState(null, '', url);
  });

  // Note: Math.random() is used to ensure that input$simulateBtnClicked
  // changes every time getInputs is called,
  // even if the input values themselves have not changed.
  shinyjs.getInputs = function () {
    const inputs = $(".my-input-class").map(function () {
      return this.value;
    }).get();
    // Note: is important to prefix the ids with the output
    // of session$ns otherwise the observers will not be triggered
    Shiny.onInputChange("sim_mode-inputs", inputs);
    Shiny.setInputValue("sim_mode-simulateBtnClicked", Math.random());
  };
});
