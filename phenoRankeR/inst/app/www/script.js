console.log("script.js loaded");

// TODO
// the function is missing a namespace
// e.g. SimulateHistorySidebar

function changeIcon(id, textboxId, linkId, sidebar) {
  console.log("changeIcon called")

  const icon = document.querySelector(`#${id} i`);

  if (icon.classList.contains('fa-pen-to-square')) {
    icon.classList.replace('fa-pen-to-square', 'fa-check');
    return;
  }
  // icon.classList.contains('fa-check')
  icon.classList.replace('fa-check', 'fa-pen-to-square');

  const textbox = document.getElementById(textboxId);
  const link = document.getElementById(linkId);
  textbox.style.display = 'none';
  link.style.display = 'block';

  Shiny.setInputValue(`${sidebar}-check_icon_clicked`, {
    id,
    value: textbox.value
  });
}

// use that function in 
// mod_sim_mode line 339
function returnNumberInputField(data, type) {
  if (type === 'display') {
    return `<input class="my-input-class" type="number" min="0" step="1" value="${data}" />`;
  }
  return data;
}

