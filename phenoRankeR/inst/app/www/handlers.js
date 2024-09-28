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
  
  const docsLink = document.createElement('a');
  docsLink.href = 'https://cnag-biomedical-informatics.github.io/pheno-ranker-ui';
  docsLink.target = '_blank';
  docsLink.innerHTML = '<i class="fas fa-book-open-reader"></i>';
  docsLink.style = 'font-size: 2.5em; color: black;';

  const githubLink = document.createElement('a');
  githubLink.href = 'https://github.com/CNAG-Biomedical-Informatics/pheno-ranker-ui';
  githubLink.target = '_blank';
  githubLink.innerHTML = '<i class="fab fa-github"></i>';
  githubLink.style = 'font-size: 2.5em; color: black;';

  const header = document.querySelector('.navbar > .container-fluid');
  if (!header.querySelector('.fa-book-open-reader')) {
    header.appendChild(docsLink);
  }

  if (!header.querySelector('.fa-github')) {
    header.appendChild(githubLink);
  }

   // Function to wait for the parentElement to appear
  function waitForParentElement(selector, callback) {
    const parentObserver = new MutationObserver((mutationsList, observer) => {
      for (const mutation of mutationsList) {
        if (mutation.type === 'childList') {
          // Check if the parent element is now present
          const parentElement = document.querySelector(selector);
          
          if (parentElement) {
              console.log("The parent element is now present.");
              // Call the callback function with the parentElement
              callback(parentElement);

              // Stop observing the document once the parent element has been found
              observer.disconnect();
              break;
          }
        }
      }
    });
    // Start observing the document body or a broader container until the parentElement appears
    parentObserver.observe(document.body, { childList: true, subtree: true });
  }

  // Function to check the visibility of the button
  function checkButtonVisibility(button) {
    const isVisible = button.clientHeight > 0 && button.clientWidth > 0;
    
    if (isVisible) {
        console.log("The button with class 'navbar-toggle collapsed' is visible.");
        // Add any logic you need when the button is visible
    } else {
        console.log("The button with class 'navbar-toggle collapsed' is hidden.");
        // Add any logic you need when the button is hidden
    }
  }

  // Function to observe the display change of the button
  function observeButtonVisibility(parentElement) {
    // Find the button within the parent element

    console.log(parentElement);
    const button = parentElement.querySelector('button.navbar-toggle.collapsed');
    console.log(button);
    
    // Create a MutationObserver to monitor attribute changes
    const buttonObserver = new MutationObserver(() => {
      checkButtonVisibility(button);
    });
    
    // Start observing the button for attribute changes
    buttonObserver.observe(
      button, 
      { attributes: true, childList: false, subtree: false }
    );
    // Check the visibility of the button initially
    checkButtonVisibility(button);

    // Add a resize event listener to check the visibility of the button
    window.addEventListener('resize', () => {
      checkButtonVisibility(button);
    });
  }

  // Call the function to wait for the parent element
  waitForParentElement('.navbar-header', observeButtonVisibility);

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

  shinyjs.beaconApiRequestTriggered = function () {
    console.log("setBeaconApiRequestTriggered");
    Shiny.setInputValue("beacon_api-queryBeaconApiClicked", Math.random());
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

  shinyjs.patientRankingStartTriggered = function () {
    console.log("setPatientRankingStartTriggered");
    Shiny.setInputValue("patient_mode-patientRankingBtnClicked", Math.random());
  };

  Shiny.addCustomMessageHandler("triggerWaitForElement", function (message) {
    console.log("Waiting for:", message.text, "in element:", message.element);
    waitForElementWithText(message.element, message.text, function (element) {
      Shiny.setInputValue("beacon_api-loader_beacon_api-elementFound", Math.random());
      Shiny.setInputValue("input_examples-loader_example_retrieval-elementFound", Math.random());
      Shiny.setInputValue("sim_mode-loader_simulate-elementFound", Math.random());
      Shiny.setInputValue("conv_mode-loader_conv-elementFound", Math.random());
      Shiny.setInputValue("cohort_mode-loader_cohort_mode-elementFound", Math.random());
      Shiny.setInputValue("patient_mode-loader_patient_mode-elementFound", Math.random());
      console.log("Found element with text content:", element);
    });
  })
});
