Package.describe({
  summary: "osteele function.js with some modification for meteor"
});

Package.on_use(function (api) {
  api.export('Functional', 'client');
  api.add_files('lib/functional.js', 'client');
});
