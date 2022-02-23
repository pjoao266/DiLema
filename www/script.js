var n_clicks = 0;
function click(){
  n_clicks = n_clicks + 1;
}

function copyTextToClipboard(text) {
  if (!navigator.clipboard) {
    fallbackCopyTextToClipboard(text);
    return;
  }
  navigator.clipboard.writeText(text).then(function() {
    console.log('Async: Copying to clipboard was successful!');
  }, function(err) {
    console.error('Async: Could not copy text: ', err);
  });
}
const letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
    const all_key_ids = [ ...letters, 'Enter', 'Back'];
    document.addEventListener('keydown', function(e) {
      let key = e.code.replace(/^Key/, '');
      if (letters.includes(key)) {
        document.getElementById(key).click();
      } else if (key == 'Enter') {
        document.getElementById('Enter').click();
      } else if (key == 'Backspace') {
        document.getElementById('Back').click();
      }
    });

    // For better responsiveness on touch devices, trigger a click on the button
    // when a touchstart event occurs; don't wait for the touchend event. So
    // that a click event doesn't happen when the touchend event happens (and
    // cause the letter to be typed a second time), we set the 'pointer-events'
    // CSS property to 'none' on the button. Then when there's _any_ touchend
    // event, unset the 'pointer-events' CSS property on all of the buttons, so
    // that the button can be touched again.
    let in_button_touch = false;
    document.addEventListener('touchstart', function(e) {
        if (all_key_ids.includes(e.target.id)) {
            e.target.click();
            e.target.style.pointerEvents = 'none';
            e.preventDefault();   // Disable text selection
            in_button_touch = true;
        }
    });
    document.addEventListener('touchend', function(e) {
        all_key_ids.map((id) => {
            document.getElementById(id).style.pointerEvents = null;
        });
        if (in_button_touch) {
            if (all_key_ids.includes(e.target.id)) {
                // Disable text selection and triggering of click event.
                e.preventDefault();
            }
            in_button_touch = false;
        }
    });