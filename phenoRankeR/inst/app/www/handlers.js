// Function to wait for an element with specific text content
function waitForElementWithText(selector, text, callback) {
  const interval = 100; // Interval in milliseconds to check for the element

  const intervalId = setInterval(function () {
    const elements = document.querySelectorAll(selector);
    Array.from(elements).forEach(function (element) {
      if (element.textContent.trim() === text) {
        clearInterval(intervalId);
        callback(element);
      }
    });
  }, interval);
}


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

  shinyjs.exampleRequestTriggered = function () {
    console.log("setExampleRequestTriggered");
    Shiny.setInputValue("input_examples-getExampleInputClicked", Math.random());
  };

  shinyjs.conversionStartTriggered = function () {
    console.log("setConversionStartTriggered");
    Shiny.setInputValue("conv_mode-convertBtnClicked", Math.random());
  };

  shinyjs.cohortRankingStartTriggered = function () {
    console.log("setCohortRankingStartTriggered");
    Shiny.setInputValue("cohort_mode-cohortRankingBtnClicked", Math.random());
  };

  Shiny.addCustomMessageHandler("triggerWaitForElement", function (message) {
    console.log("Waiting for:", message.text, "in element:", message.element);
    waitForElementWithText(message.element, message.text, function (element) {
      Shiny.setInputValue("input_examples-loader_example_retrieval-elementFound", Math.random());
      Shiny.setInputValue("sim_mode-loader_simulate-elementFound", Math.random());
      Shiny.setInputValue("conv_mode-loader_conv-elementFound", Math.random());
      Shiny.setInputValue("cohort_mode-loader_cohort_mode-elementFound", Math.random());
      console.log("Found element with text content:", element);
    });
  })
});
