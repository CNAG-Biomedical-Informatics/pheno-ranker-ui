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

  const docsUrl = 'https://cnag-biomedical-informatics.github.io/pheno-ranker-ui';
  const githubUrl = 'https://github.com/CNAG-Biomedical-Informatics/pheno-ranker-ui'
  
  const docsLink = document.createElement('a');
  docsLink.href = docsUrl;
  docsLink.target = '_blank';
  docsLink.innerHTML = '<i class="fas fa-book-open-reader"></i>';
  docsLink.style = 'font-size: 1.8em; color: black; padding-right: 5px;';

  const githubLink = document.createElement('a');
  githubLink.href = githubUrl;
  githubLink.target = '_blank';
  githubLink.innerHTML = '<i class="fab fa-github"></i>';
  githubLink.style = 'font-size: 1.8em; color: black;';

  // add icons to the header
  const header = document.querySelector('.navbar > .container-fluid');
  if (!header.querySelector('.fa-book-open-reader')) {
    header.appendChild(docsLink);
  }

  if (!header.querySelector('.fa-github')) {
    header.appendChild(githubLink);
  }

  const docsLinkLi = document.createElement('li');
  const githubLinkLi = document.createElement('li');

  function addToCollapsedNavbar(docsLinkLi, githubLinkLi) {
    const collapsedNavbar = document.querySelector('ul.nav.navbar-nav.nav-underline.shiny-tab-input.shiny-bound-input');

    console.log(collapsedNavbar);
    
    // only add the links without the icons
    const docsLinkA = document.createElement('a');
    docsLinkA.href = docsUrl;
    docsLinkA.target = '_blank';
    docsLinkA.innerHTML = 'Documentation';
    docsLinkLi.style = 'display: None;';
    docsLinkLi.appendChild(docsLinkA);

    
    const githubLinkA = document.createElement('a');
    githubLinkA.href = githubUrl;
    githubLinkA.target = '_blank';
    githubLinkA.innerHTML = 'GitHub Repository';
    githubLinkLi.style = 'display: None;';
    githubLinkLi.appendChild(githubLinkA);
    
    if (!collapsedNavbar.querySelector('a[href="' + docsUrl + '"]')) {
      collapsedNavbar.appendChild(docsLinkLi);
    }

    if (!collapsedNavbar.querySelector('a[href="' + githubUrl + '"]')) {
      collapsedNavbar.appendChild(githubLinkLi);
    }
  }

  // Function to wait for the parentElement to appear
  function waitForParentElement(selector, callback) {
    const parentObserver = new MutationObserver((mutationsList, observer) => {
      for (const mutation of mutationsList) {
        if (mutation.type === 'childList') {
          // Check if the parent element is now present
          const parentElement = document.querySelector(selector);

          if (!callback) {
            addToCollapsedNavbar(docsLinkLi, githubLinkLi);
            observer.disconnect();
            break;
          }
          
          if (parentElement) {
            callback(parentElement);
            observer.disconnect();
            break;
          }
        }
      }
    });
    // Start observing the document body or a broader container until the parentElement appears
    parentObserver.observe(document.body, { childList: true, subtree: true });
  }

  function hideShowIconsOrLinks(navbarCollapsed) {
    if (navbarCollapsed) {
      githubLink.style.display = 'none';
      docsLink.style.display = 'none';
      docsLinkLi.style.display = 'block';
      githubLinkLi.style.display = 'block';
    }
    else {
      githubLink.style.display = 'block';
      docsLink.style.display = 'block';
      docsLinkLi.style.display = 'none';
      githubLinkLi.style.display = 'none';
    }
  }

  function checkButtonVisibility(button) {
    const isVisible = button.clientHeight > 0 && button.clientWidth > 0;
    hideShowIconsOrLinks(isVisible);
  }

  // Function to observe the display change of the button
  function observeButtonVisibility(parentElement) {
    const button = parentElement.querySelector('button.navbar-toggle.collapsed');
    
    const buttonObserver = new MutationObserver(() => {
      checkButtonVisibility(button);
    });
    
    // Start observing the button for attribute changes
    buttonObserver.observe(
      button, 
      { attributes: true, childList: false, subtree: false }
    );
    // initial visibility check
    checkButtonVisibility(button);

    // check visibility on resize
    window.addEventListener('resize', () => {
      checkButtonVisibility(button);
    });
  }

  // Call the function to wait for the parent element
  waitForParentElement(
    'ul.nav.navbar-nav.nav-underline.shiny-tab-input.shiny-bound-input',
  );
  waitForParentElement(
    '.navbar-header',
    observeButtonVisibility
  );

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
