Shiny.addCustomMessageHandler("updateNavbar", function(data) {
  // Update navbar title
  if (data.title) {
    document.querySelector(".navbar-brand").innerText = data.title;
  }

  // Update tab labels
  if (data.tabs) {
    if (data.tabs.home) {
      document.querySelector('a[data-value="home"]').innerText = data.tabs.home;
    }
    if (data.tabs.about) {
      document.querySelector('a[data-value="about"]').innerText = data.tabs.about;
    }
  }
});
